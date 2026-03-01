/**
 * Trampoline evaluator — explicit-stack evaluation engine.
 *
 * `stepNode(node, env, k)` maps an AST node to the next `Step`.
 * `applyFrame(frame, value, k)` processes a completed sub-result against a frame.
 * `tick(step)` processes one step and returns the next (or a Promise<Step> for async).
 * `runSyncTrampoline(step)` runs the trampoline synchronously to completion.
 * `runAsyncTrampoline(step)` runs the trampoline asynchronously to completion.
 *
 * Entry points:
 * - `evaluate(ast, contextStack)` — evaluate an AST (sync or async)
 * - `evaluateNode(node, contextStack)` — evaluate a single node (sync or async)
 *
 * Design principles:
 * - `stepNode` is always synchronous and returns `Step`.
 * - `applyFrame` may return `Step | Promise<Step>` when normal expressions
 *   or compound function types produce async results.
 * - Normal built-in expressions are called directly with pre-evaluated args
 *   (they may still use the old recursive `evaluateNode` internally for
 *   higher-order callbacks — suspension through them is deferred).
 * - Binding utilities (`evaluateBindingNodeValues`, `tryMatch`) are called
 *   with a recursive `evaluateNode` helper. This is acceptable for Phase 1.
 * - All state lives in frames (no JS closures) — enabling serialization later.
 */

import { builtin } from '../builtin'
import { evaluateBindingNodeValues, getAllBindingTargetNames, tryMatch } from '../builtin/bindingNode'
import type { LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { MatchCase } from '../builtin/specialExpressions/match'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes, getNodeTypeName } from '../constants/constants'
import { LitsError, RecurSignal, UndefinedSymbolError, UserDefinedError } from '../errors'
import { getUndefinedSymbols } from '../getUndefinedSymbols'
import type { Any, Arr, Obj } from '../interface'
import type {
  Ast,
  AstNode,
  BindingNode,
  BindingTarget,
  CompFunction,
  EffectRef,
  EvaluatedFunction,
  EveryPredFunction,
  FNullFunction,
  FunctionLike,
  JuxtFunction,
  LitsFunction,
  ModuleFunction,
  NativeJsFunction,
  NormalBuiltinFunction,
  NormalExpressionNode,
  NumberNode,
  PartialFunction,
  ReservedSymbolNode,
  SomePredFunction,
  SpecialBuiltinFunction,
  SpecialExpressionNode,
  StringNode,
  SymbolNode,
  UserDefinedFunction,
  UserDefinedSymbolNode,
} from '../parser/types'
import { bindingTargetTypes } from '../parser/types'
import { reservedSymbolRecord } from '../tokenizer/reservedNames'
import type { SourceCodeInfo } from '../tokenizer/token'
import { asNonUndefined, isUnknownRecord } from '../typeGuards'
import { annotate } from '../typeGuards/annotatedArrays'
import { isNormalBuiltinSymbolNode, isNormalExpressionNodeWithName, isSpreadNode, isUserDefinedSymbolNode } from '../typeGuards/astNode'
import { asAny, asFunctionLike, assertEffectRef, assertSeq, isAny, isEffectRef, isObj } from '../typeGuards/lits'
import { isLitsFunction } from '../typeGuards/litsFunction'
import { assertNumber, isNumber } from '../typeGuards/number'
import { assertString } from '../typeGuards/string'
import { toAny } from '../utils'
import { arityAcceptsMin, assertNumberOfParams, toFixedArity } from '../utils/arity'
import { valueToString } from '../utils/debug/debugTools'
import type { MaybePromise } from '../utils/maybePromise'
import { chain, forEachSequential, mapSequential, reduceSequential } from '../utils/maybePromise'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import type { EffectHandler, Handlers, RunResult } from './effectTypes'
import { SuspensionSignal, isSuspensionSignal } from './effectTypes'
import type { ContextStack } from './ContextStack'
import { getEffectRef } from './effectRef'
import type {
  AndFrame,
  ArrayBuildFrame,
  BindingDefaultFrame,
  CallFnFrame,
  CondFrame,
  ContinuationStack,
  EffectResumeFrame,
  EvalArgsFrame,
  EvaluatedWithHandler,
  FnBodyFrame,
  ForLoopFrame,
  Frame,
  IfBranchFrame,
  LetBindFrame,
  LoopBindFrame,
  LoopIterateFrame,
  MatchFrame,
  NanCheckFrame,
  ObjectBuildFrame,
  OrFrame,
  PerformArgsFrame,
  QqFrame,
  RecurFrame,
  SequenceFrame,
  ThrowFrame,
  TryCatchFrame,
  TryWithFrame,
} from './frames'
import type { Context } from './interface'
import type { Step } from './step'

// Re-export for external use
export type { Step }

// ---------------------------------------------------------------------------
// Recursive evaluateNode — used as a helper for normal expressions and
// binding utilities until the full trampoline migration is complete.
// ---------------------------------------------------------------------------

function evaluateNodeRecursive(node: AstNode, contextStack: ContextStack): MaybePromise<Any> {
  switch (node[0]) {
    case NodeTypes.Number:
      return (node as NumberNode)[1]
    case NodeTypes.String:
      return (node as StringNode)[1]
    case NodeTypes.NormalBuiltinSymbol:
    case NodeTypes.SpecialBuiltinSymbol:
    case NodeTypes.UserDefinedSymbol:
      return contextStack.evaluateSymbol(node as SymbolNode)
    case NodeTypes.ReservedSymbol:
      return evaluateReservedSymbol(node as ReservedSymbolNode)
    case NodeTypes.NormalExpression: {
      const result = evaluateNormalExpressionRecursive(node as NormalExpressionNode, contextStack)
      return chain(result, (resolved) => {
        if (typeof resolved === 'number' && Number.isNaN(resolved)) {
          throw new LitsError('Number is NaN', node[2])
        }
        return annotate(resolved)
      })
    }
    case NodeTypes.SpecialExpression: {
      // Route through the trampoline — special expressions are fully handled
      // by stepSpecialExpression and their corresponding frame types.
      // This replaces the old `.evaluate()` call on special expression objects.
      const initial: Step = { type: 'Eval', node, env: contextStack, k: [] }
      try {
        return annotate(runSyncTrampoline(initial))
      }
      catch (error) {
        if (error instanceof LitsError && error.message.includes('Unexpected async operation')) {
          const freshInitial: Step = { type: 'Eval', node, env: contextStack, k: [] }
          return runAsyncTrampoline(freshInitial).then(r => annotate(r))
        }
        throw error
      }
    }
    /* v8 ignore next 2 */
    default:
      throw new LitsError(`${getNodeTypeName(node[0])}-node cannot be evaluated`, node[2])
  }
}

function evaluateParamsRecursive(
  paramNodes: AstNode[],
  contextStack: ContextStack,
): MaybePromise<{ params: Arr, placeholders: number[] }> {
  const params: Arr = []
  const placeholders: number[] = []
  const result = forEachSequential(paramNodes, (paramNode, index) => {
    if (isSpreadNode(paramNode)) {
      return chain(evaluateNodeRecursive(paramNode[1], contextStack), (spreadValue) => {
        if (Array.isArray(spreadValue)) {
          params.push(...spreadValue)
        }
        else {
          throw new LitsError(`Spread operator requires an array, got ${valueToString(paramNode)}`, paramNode[2])
        }
      })
    }
    else if (paramNode[0] === NodeTypes.ReservedSymbol && paramNode[1] === '_') {
      placeholders.push(index)
    }
    else {
      return chain(evaluateNodeRecursive(paramNode, contextStack), (value) => {
        params.push(value)
      })
    }
  })
  return chain(result, () => ({ params, placeholders }))
}

function evaluateNormalExpressionRecursive(node: NormalExpressionNode, contextStack: ContextStack): MaybePromise<Any> {
  const sourceCodeInfo = node[2]
  return chain(evaluateParamsRecursive(node[1][1], contextStack), ({ params, placeholders }) => {
    if (isNormalExpressionNodeWithName(node)) {
      const nameSymbol = node[1][0]
      if (placeholders.length > 0) {
        const fn = evaluateNodeRecursive(nameSymbol, contextStack)
        return chain(fn, (resolvedFn) => {
          const partialFunction: PartialFunction = {
            [FUNCTION_SYMBOL]: true,
            function: asFunctionLike(resolvedFn, sourceCodeInfo),
            functionType: 'Partial',
            params,
            placeholders,
            sourceCodeInfo,
            arity: toFixedArity(placeholders.length),
          }
          return partialFunction
        })
      }
      if (isNormalBuiltinSymbolNode(nameSymbol)) {
        const type = nameSymbol[1]
        const normalExpression = builtin.allNormalExpressions[type]!
        if (contextStack.pure && normalExpression.pure === false) {
          throw new LitsError(`Cannot call impure function '${normalExpression.name}' in pure mode`, node[2])
        }
        return normalExpression.evaluate(params, node[2], contextStack, { executeFunction: executeFunctionRecursive })
      }
      else {
        const fn = contextStack.getValue(nameSymbol[1])
        if (fn !== undefined) {
          return executeFunctionRecursive(asFunctionLike(fn, sourceCodeInfo), params, contextStack, sourceCodeInfo)
        }
        throw new UndefinedSymbolError(nameSymbol[1], node[2])
      }
    }
    else {
      const fnNode: AstNode = node[1][0]
      return chain(evaluateNodeRecursive(fnNode, contextStack), (resolvedFn) => {
        const fn = asFunctionLike(resolvedFn, sourceCodeInfo)
        if (placeholders.length > 0) {
          const partialFunction: PartialFunction = {
            [FUNCTION_SYMBOL]: true,
            function: fn,
            functionType: 'Partial',
            params,
            placeholders,
            sourceCodeInfo,
            arity: toFixedArity(placeholders.length),
          }
          return partialFunction
        }
        return executeFunctionRecursive(fn, params, contextStack, sourceCodeInfo)
      })
    }
  })
}

function executeFunctionRecursive(fn: FunctionLike, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  if (isLitsFunction(fn)) {
    return executeLitsFunctionRecursive(fn, params, contextStack, sourceCodeInfo)
  }
  if (Array.isArray(fn)) {
    return evaluateArrayAsFunction(fn, params, sourceCodeInfo)
  }
  if (isObj(fn)) {
    return evaluateObjectAsFunction(fn, params, sourceCodeInfo)
  }
  if (typeof fn === 'string') {
    return evaluateStringAsFunction(fn, params, sourceCodeInfo)
  }
  if (isNumber(fn)) {
    return evaluateNumberAsFunction(fn, params, sourceCodeInfo)
  }
  /* v8 ignore next 1 */
  throw new LitsError('Unexpected function type', sourceCodeInfo)
}

/**
 * Execute a LitsFunction recursively. This is the old-style executor
 * used as a fallback for normal expression callbacks.
 */
function executeLitsFunctionRecursive(fn: LitsFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  switch (fn.functionType) {
    case 'UserDefined':
      return executeUserDefinedRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Partial':
      return executePartialRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Comp':
      return executeCompRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Constantly':
      return fn.value
    case 'Juxt':
      return executeJuxtRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Complement':
      return chain(executeFunctionRecursive(fn.function, params, contextStack, sourceCodeInfo), result => !result)
    case 'EveryPred':
      return executeEveryPredRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'SomePred':
      return executeSomePredRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Fnull':
      return executeFnullRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Builtin':
      return executeBuiltinRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'SpecialBuiltin':
      return executeSpecialBuiltinRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'Module':
      return executeModuleRecursive(fn, params, contextStack, sourceCodeInfo)
    case 'NativeJsFunction':
      return executeNativeJsFunctionRecursive(fn, params, contextStack, sourceCodeInfo)
  }
}

function executeUserDefinedRecursive(fn: UserDefinedFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  function setupAndExecute(currentParams: Arr): MaybePromise<Any> {
    if (!arityAcceptsMin(fn.arity, currentParams.length)) {
      throw new LitsError(`Expected ${fn.arity} arguments, got ${currentParams.length}.`, sourceCodeInfo)
    }
    const evaluatedFunction = fn.evaluatedfunction
    const args = evaluatedFunction[0]
    const nbrOfNonRestArgs = args.filter(arg => arg[0] !== bindingTargetTypes.rest).length
    const newContextStack = contextStack.create(fn.evaluatedfunction[2])
    const newContext: Context = { self: { value: fn } }
    const rest: Arr = []
    let paramSetup: MaybePromise<void> = undefined as unknown as void
    for (let i = 0; i < currentParams.length; i += 1) {
      if (i < nbrOfNonRestArgs) {
        const paramIndex = i
        paramSetup = chain(paramSetup, () => {
          const param = toAny(currentParams[paramIndex])
          return chain(evaluateBindingNodeValues(args[paramIndex]!, param, node =>
            evaluateNodeRecursive(node, newContextStack.create(newContext))), (valueRecord) => {
            Object.entries(valueRecord).forEach(([key, value]) => {
              newContext[key] = { value }
            })
          })
        })
      }
      else {
        rest.push(toAny(currentParams[i]))
      }
    }
    let defaultSetup: MaybePromise<void> = undefined as unknown as void
    for (let i = currentParams.length; i < nbrOfNonRestArgs; i++) {
      const argIndex = i
      defaultSetup = chain(defaultSetup, () => {
        const arg = args[argIndex]!
        return chain(evaluateNodeRecursive(arg[1][1]!, contextStack.create(newContext)), (defaultValue) => {
          return chain(evaluateBindingNodeValues(arg, defaultValue, node =>
            evaluateNodeRecursive(node, contextStack.create(newContext))), (valueRecord) => {
            Object.entries(valueRecord).forEach(([key, value]) => {
              newContext[key] = { value }
            })
          })
        })
      })
    }
    return chain(paramSetup, () => chain(defaultSetup, () => {
      const restArgument = args.find(arg => arg[0] === bindingTargetTypes.rest)
      const restSetup: MaybePromise<void> = restArgument !== undefined
        ? chain(evaluateBindingNodeValues(restArgument, rest, node =>
            evaluateNodeRecursive(node, contextStack.create(newContext))), (valueRecord) => {
            Object.entries(valueRecord).forEach(([key, value]) => {
              newContext[key] = { value }
            })
          })
        : undefined as unknown as void
      return chain(restSetup, () => {
        const newContextStack2 = newContextStack.create(newContext)
        const bodyResult = reduceSequential(
          evaluatedFunction[1],
          (_acc, node) => evaluateNodeRecursive(node, newContextStack2),
          null as Any,
        )
        if (bodyResult instanceof Promise) {
          return bodyResult.catch((error: unknown) => {
            if (error instanceof RecurSignal) {
              return setupAndExecute(error.params)
            }
            throw error
          })
        }
        return bodyResult
      })
    }))
  }
  for (;;) {
    try {
      const result = setupAndExecute(params)
      return result
    }
    catch (error) {
      if (error instanceof RecurSignal) {
        params = error.params
        continue
      }
      throw error
    }
  }
}

function executePartialRecursive(fn: PartialFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const actualParams = [...fn.params]
  if (params.length !== fn.placeholders.length) {
    throw new LitsError(`(partial) expects ${fn.placeholders.length} arguments, got ${params.length}.`, sourceCodeInfo)
  }
  const paramsCopy = [...params]
  for (const placeholderIndex of fn.placeholders) {
    actualParams.splice(placeholderIndex, 0, paramsCopy.shift())
  }
  return executeFunctionRecursive(fn.function, actualParams, contextStack, sourceCodeInfo)
}

function executeCompRecursive(fn: CompFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const { params: f } = fn
  if (f.length === 0) {
    if (params.length !== 1)
      throw new LitsError(`(comp) expects one argument, got ${valueToString(params.length)}.`, sourceCodeInfo)
    return asAny(params[0], sourceCodeInfo)
  }
  let result: MaybePromise<Arr> = params
  for (let i = f.length - 1; i >= 0; i--) {
    const fun = f[i]!
    result = chain(result, currentParams =>
      chain(executeFunctionRecursive(asFunctionLike(fun, sourceCodeInfo), currentParams, contextStack, sourceCodeInfo), r => [r]))
  }
  return chain(result, finalArr => asAny(finalArr[0], sourceCodeInfo))
}

function executeJuxtRecursive(fn: JuxtFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  return mapSequential(fn.params, fun =>
    executeFunctionRecursive(asFunctionLike(fun, sourceCodeInfo), params, contextStack, sourceCodeInfo))
}

function executeEveryPredRecursive(fn: EveryPredFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const checks: Array<() => MaybePromise<Any>> = []
  for (const f of fn.params) {
    for (const param of params) {
      checks.push(() => executeFunctionRecursive(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo))
    }
  }
  return reduceSequential(checks, (acc, check) => {
    if (!acc)
      return false
    return chain(check(), result => !!result)
  }, true as Any)
}

function executeSomePredRecursive(fn: SomePredFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const checks: Array<() => MaybePromise<Any>> = []
  for (const f of fn.params) {
    for (const param of params) {
      checks.push(() => executeFunctionRecursive(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo))
    }
  }
  return reduceSequential(checks, (acc, check) => {
    if (acc)
      return true
    return chain(check(), result => !!result)
  }, false as Any)
}

function executeFnullRecursive(fn: FNullFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const fnulledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
  return executeFunctionRecursive(asFunctionLike(fn.function, sourceCodeInfo), fnulledParams, contextStack, sourceCodeInfo)
}

function executeBuiltinRecursive(fn: NormalBuiltinFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const normalExpression = asNonUndefined(builtin.allNormalExpressions[fn.normalBuiltinSymbolType], sourceCodeInfo)
  if (contextStack.pure && normalExpression.pure === false) {
    throw new LitsError(`Cannot call impure function '${fn.name}' in pure mode`, sourceCodeInfo)
  }
  return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunctionRecursive })
}

function executeSpecialBuiltinRecursive(fn: SpecialBuiltinFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const specialExpression = asNonUndefined(builtin.specialExpressions[fn.specialBuiltinSymbolType], sourceCodeInfo)
  if (specialExpression.evaluateAsNormalExpression) {
    return specialExpression.evaluateAsNormalExpression(params, sourceCodeInfo, contextStack, { executeFunction: executeFunctionRecursive })
  }
  throw new LitsError(`Special builtin function ${fn.specialBuiltinSymbolType} is not supported as normal expression.`, sourceCodeInfo)
}

function executeModuleRecursive(fn: ModuleFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  const litsModule = contextStack.getModule(fn.moduleName)
  if (!litsModule) {
    throw new LitsError(`Module '${fn.moduleName}' not found.`, sourceCodeInfo)
  }
  const expression = litsModule.functions[fn.functionName]
  if (!expression) {
    throw new LitsError(`Function '${fn.functionName}' not found in module '${fn.moduleName}'.`, sourceCodeInfo)
  }
  if (contextStack.pure && expression.pure === false) {
    throw new LitsError(`Cannot call impure function '${fn.functionName}' in pure mode`, sourceCodeInfo)
  }
  assertNumberOfParams(expression.arity, params.length, sourceCodeInfo)
  return expression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunctionRecursive })
}

function executeNativeJsFunctionRecursive(fn: NativeJsFunction, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo): MaybePromise<Any> {
  if (contextStack.pure && !fn.nativeFn.pure) {
    throw new LitsError(`Cannot call impure native function '${fn.name}' in pure mode`, sourceCodeInfo)
  }
  try {
    const result = fn.nativeFn.fn(...params)
    if (result instanceof Promise) {
      return result.then(
        resolved => toAny(resolved),
        (error: unknown) => {
          const message = typeof error === 'string'
            ? error
            : isUnknownRecord(error) && typeof error.message === 'string'
              ? error.message
              : '<no message>'
          throw new LitsError(`Native function threw: "${message}"`, sourceCodeInfo)
        },
      )
    }
    return toAny(result)
  }
  catch (error) {
    const message = typeof error === 'string'
      ? error
      : isUnknownRecord(error) && typeof error.message === 'string'
        ? error.message
        : '<no message>'
    throw new LitsError(`Native function threw: "${message}"`, sourceCodeInfo)
  }
}

// ---------------------------------------------------------------------------
// Value-as-function helpers (shared between trampoline and recursive paths)
// ---------------------------------------------------------------------------

function evaluateObjectAsFunction(fn: Obj, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  if (params.length !== 1)
    throw new LitsError('Object as function requires one string parameter.', sourceCodeInfo)
  const key = params[0]
  assertString(key, sourceCodeInfo)
  return toAny(fn[key])
}

function evaluateArrayAsFunction(fn: Arr, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  if (params.length !== 1)
    throw new LitsError('Array as function requires one non negative integer parameter.', sourceCodeInfo)
  const index = params[0]
  assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })
  return toAny(fn[index])
}

function evaluateStringAsFunction(fn: string, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  if (params.length !== 1)
    throw new LitsError('String as function requires one Obj parameter.', sourceCodeInfo)
  const param = toAny(params[0])
  if (isObj(param))
    return toAny((param)[fn])
  if (isNumber(param, { integer: true }))
    return toAny(fn[param])
  throw new LitsError(
    `string as function expects Obj or integer parameter, got ${valueToString(param)}`,
    sourceCodeInfo,
  )
}

function evaluateNumberAsFunction(fn: number, params: Arr, sourceCodeInfo?: SourceCodeInfo): Any {
  assertNumber(fn, undefined, { integer: true })
  if (params.length !== 1)
    throw new LitsError('Number as function requires one Arr parameter.', sourceCodeInfo)
  const param = params[0]
  assertSeq(param, sourceCodeInfo)
  return toAny(param[fn])
}

// ---------------------------------------------------------------------------
// Reserved symbol evaluation
// ---------------------------------------------------------------------------

function evaluateReservedSymbol(node: ReservedSymbolNode): Any {
  const reservedName = node[1]
  if (!['true', 'false', 'null'].includes(reservedName)) {
    throw new LitsError(`Reserved symbol ${reservedName} cannot be evaluated`, node[2])
  }
  const value = reservedSymbolRecord[reservedName]
  return asNonUndefined(value, node[2])
}

// ---------------------------------------------------------------------------
// Lambda helper (closure capture) — used by stepSpecialExpression for lambda
// ---------------------------------------------------------------------------

function evaluateFunction(
  fn: [BindingTarget[], AstNode[], ...unknown[]],
  contextStack: ContextStack,
): EvaluatedFunction {
  const functionContext: Context = {}
  const context = fn[0].reduce((ctx: Context, arg) => {
    Object.keys(getAllBindingTargetNames(arg)).forEach((name) => {
      ctx[name] = { value: null }
    })
    return ctx
  }, {})
  const undefinedSymbols = getUndefinedSymbols(fn[1], contextStack.new(context), builtin, evaluateNodeRecursive)
  undefinedSymbols.forEach((name) => {
    const value = contextStack.getValue(name)
    if (isAny(value)) {
      functionContext[name] = { value }
    }
  })
  return [fn[0], fn[1], functionContext]
}

// ---------------------------------------------------------------------------
// stepNode — map an AST node to the next Step
// ---------------------------------------------------------------------------

/**
 * Given an AST node, its environment, and a continuation stack, return
 * the next Step for the trampoline to process.
 *
 * Leaf nodes (numbers, strings, symbols) immediately produce values.
 * Compound nodes (expressions) push frames and return sub-evaluations.
 */
export function stepNode(node: AstNode, env: ContextStack, k: ContinuationStack): Step | Promise<Step> {
  switch (node[0]) {
    case NodeTypes.Number:
      return { type: 'Value', value: (node as NumberNode)[1], k }
    case NodeTypes.String:
      return { type: 'Value', value: (node as StringNode)[1], k }
    case NodeTypes.NormalBuiltinSymbol:
    case NodeTypes.SpecialBuiltinSymbol:
    case NodeTypes.UserDefinedSymbol:
      return { type: 'Value', value: env.evaluateSymbol(node as SymbolNode), k }
    case NodeTypes.ReservedSymbol:
      return { type: 'Value', value: evaluateReservedSymbol(node as ReservedSymbolNode), k }
    case NodeTypes.NormalExpression:
      return stepNormalExpression(node as NormalExpressionNode, env, k)
    case NodeTypes.SpecialExpression:
      return stepSpecialExpression(node as SpecialExpressionNode, env, k)
    /* v8 ignore next 2 */
    default:
      throw new LitsError(`${getNodeTypeName(node[0])}-node cannot be evaluated`, node[2])
  }
}

// ---------------------------------------------------------------------------
// stepNormalExpression — start evaluating a function call's arguments
// ---------------------------------------------------------------------------

/**
 * Normal expressions: evaluate arguments left-to-right, then dispatch.
 * Push EvalArgsFrame + NanCheckFrame, then start evaluating the first arg.
 */
function stepNormalExpression(node: NormalExpressionNode, env: ContextStack, k: ContinuationStack): Step | Promise<Step> {
  const argNodes = node[1][1]
  const sourceCodeInfo = node[2]

  // NaN guard wraps the final result
  const nanFrame: NanCheckFrame = { type: 'NanCheck', sourceCodeInfo }

  // Argument evaluator frame
  const evalArgsFrame: EvalArgsFrame = {
    type: 'EvalArgs',
    node,
    index: 0,
    params: [],
    placeholders: [],
    env,
    sourceCodeInfo,
  }

  // Find the first real argument to evaluate (skip leading placeholders)
  let startIndex = 0
  while (startIndex < argNodes.length) {
    const arg = argNodes[startIndex]!
    if (arg[0] === NodeTypes.ReservedSymbol && arg[1] === '_') {
      evalArgsFrame.placeholders.push(evalArgsFrame.params.length)
      startIndex++
    }
    else {
      break
    }
  }
  evalArgsFrame.index = startIndex

  if (startIndex >= argNodes.length) {
    // No real args to evaluate — dispatch immediately
    return dispatchCall(evalArgsFrame, [nanFrame, ...k])
  }

  // Start evaluating the first real argument
  const firstArg = argNodes[startIndex]!
  const newK: ContinuationStack = [evalArgsFrame, nanFrame, ...k]
  if (isSpreadNode(firstArg)) {
    return { type: 'Eval', node: firstArg[1], env, k: newK }
  }
  return { type: 'Eval', node: firstArg, env, k: newK }
}

// ---------------------------------------------------------------------------
// stepSpecialExpression — push frame for a special expression
// ---------------------------------------------------------------------------

/**
 * Special expressions: push a frame appropriate to the expression type
 * and return an EvalStep for the first sub-expression.
 */
function stepSpecialExpression(node: SpecialExpressionNode, env: ContextStack, k: ContinuationStack): Step | Promise<Step> {
  const sourceCodeInfo = node[2]
  const type = node[1][0]

  switch (type) {
    // --- if / unless ---
    case specialExpressionTypes.if:
    case specialExpressionTypes.unless: {
      const [conditionNode, thenNode, elseNode] = node[1][1] as [AstNode, AstNode, AstNode?]
      const frame: IfBranchFrame = {
        type: 'IfBranch',
        thenNode,
        elseNode,
        inverted: type === specialExpressionTypes.unless,
        env,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: conditionNode, env, k: [frame, ...k] }
    }

    // --- && (and) ---
    case specialExpressionTypes['&&']: {
      const nodes = node[1][1] as AstNode[]
      if (nodes.length === 0) {
        return { type: 'Value', value: true, k }
      }
      const frame: AndFrame = {
        type: 'And',
        nodes,
        index: 1,
        env,
        sourceCodeInfo,
      }
      if (nodes.length === 1) {
        return { type: 'Eval', node: nodes[0]!, env, k }
      }
      return { type: 'Eval', node: nodes[0]!, env, k: [frame, ...k] }
    }

    // --- || (or) ---
    case specialExpressionTypes['||']: {
      const nodes = node[1][1] as AstNode[]
      if (nodes.length === 0) {
        return { type: 'Value', value: false, k }
      }
      const frame: OrFrame = {
        type: 'Or',
        nodes,
        index: 1,
        env,
        sourceCodeInfo,
      }
      if (nodes.length === 1) {
        return { type: 'Eval', node: nodes[0]!, env, k }
      }
      return { type: 'Eval', node: nodes[0]!, env, k: [frame, ...k] }
    }

    // --- ?? (nullish coalescing) ---
    case specialExpressionTypes['??']: {
      const nodes = node[1][1] as AstNode[]
      if (nodes.length === 0) {
        return { type: 'Value', value: null, k }
      }
      // Check if the first node is an undefined user symbol
      const firstNode = nodes[0]!
      if (isUserDefinedSymbolNode(firstNode) && env.lookUp(firstNode) === null) {
        // Undefined symbol — treat as null, skip to next
        if (nodes.length === 1) {
          return { type: 'Value', value: null, k }
        }
        const frame: QqFrame = {
          type: 'Qq',
          nodes,
          index: 2,
          env,
          sourceCodeInfo,
        }
        const nextNode = nodes[1]!
        if (isUserDefinedSymbolNode(nextNode) && env.lookUp(nextNode) === null) {
          // Also undefined — continue skipping
          return skipUndefinedQq(frame, k)
        }
        if (nodes.length === 2) {
          return { type: 'Eval', node: nextNode, env, k }
        }
        return { type: 'Eval', node: nextNode, env, k: [frame, ...k] }
      }
      const frame: QqFrame = {
        type: 'Qq',
        nodes,
        index: 1,
        env,
        sourceCodeInfo,
      }
      if (nodes.length === 1) {
        return { type: 'Eval', node: firstNode, env, k }
      }
      return { type: 'Eval', node: firstNode, env, k: [frame, ...k] }
    }

    // --- cond ---
    case specialExpressionTypes.cond: {
      const cases = node[1][1] as [AstNode, AstNode][]
      if (cases.length === 0) {
        return { type: 'Value', value: null, k }
      }
      const frame: CondFrame = {
        type: 'Cond',
        phase: 'test',
        cases,
        index: 0,
        env,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: cases[0]![0], env, k: [frame, ...k] }
    }

    // --- match ---
    case specialExpressionTypes.match: {
      const matchValueNode = node[1][1] as AstNode
      const cases = node[1][2] as MatchCase[]
      const frame: MatchFrame = {
        type: 'Match',
        phase: 'matchValue',
        matchValue: null,
        cases,
        index: 0,
        bindings: {},
        env,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: matchValueNode, env, k: [frame, ...k] }
    }

    // --- block (do...end) ---
    case specialExpressionTypes.block: {
      const nodes = node[1][1] as AstNode[]
      const newContext: Context = {}
      const newEnv = env.create(newContext)
      if (nodes.length === 0) {
        return { type: 'Value', value: null, k }
      }
      if (nodes.length === 1) {
        return { type: 'Eval', node: nodes[0]!, env: newEnv, k }
      }
      const frame: SequenceFrame = {
        type: 'Sequence',
        nodes,
        index: 1,
        env: newEnv,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: nodes[0]!, env: newEnv, k: [frame, ...k] }
    }

    // --- let ---
    case specialExpressionTypes.let: {
      const bindingNode = node[1][1] as BindingNode
      const target = bindingNode[1][0]
      const valueNode = bindingNode[1][1]
      const frame: LetBindFrame = {
        type: 'LetBind',
        target,
        env,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: valueNode, env, k: [frame, ...k] }
    }

    // --- loop ---
    case specialExpressionTypes.loop: {
      const bindingNodes = node[1][1] as BindingNode[]
      const body = node[1][2] as AstNode
      if (bindingNodes.length === 0) {
        // No bindings — just evaluate the body with an empty context
        const newContext: Context = {}
        const frame: LoopIterateFrame = {
          type: 'LoopIterate',
          bindingNodes,
          bindingContext: newContext,
          body,
          env: env.create(newContext),
          sourceCodeInfo,
        }
        return { type: 'Eval', node: body, env: env.create(newContext), k: [frame, ...k] }
      }
      // Start evaluating the first binding's value
      const frame: LoopBindFrame = {
        type: 'LoopBind',
        phase: 'value',
        bindingNodes,
        index: 0,
        context: {},
        body,
        env,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: bindingNodes[0]![1][1], env, k: [frame, ...k] }
    }

    // --- for / doseq ---
    case specialExpressionTypes.for:
    case specialExpressionTypes.doseq: {
      const loopBindings = node[1][1] as LoopBindingNode[]
      const body = node[1][2] as AstNode
      const returnResult = type === specialExpressionTypes.for
      if (loopBindings.length === 0) {
        return { type: 'Value', value: returnResult ? [] : null, k }
      }
      const context: Context = {}
      const newEnv = env.create(context)
      const frame: ForLoopFrame = {
        type: 'ForLoop',
        returnResult,
        bindingNodes: loopBindings,
        body,
        result: [],
        phase: 'evalCollection',
        bindingLevel: 0,
        levelStates: [],
        context,
        env: newEnv,
        sourceCodeInfo,
      }
      // Evaluate the first binding's collection expression
      const firstBinding = loopBindings[0]!
      const collectionNode = firstBinding[0][1][1] // bindingNode → [target, valueNode]
      return { type: 'Eval', node: collectionNode, env: newEnv, k: [frame, ...k] }
    }

    // --- try ---
    case specialExpressionTypes.try: {
      const tryExpression = node[1][1] as AstNode
      const errorSymbol = node[1][2] as SymbolNode | undefined
      const catchExpression = node[1][3] as AstNode | undefined
      const withHandlerNodes = node[1][4] as [AstNode, AstNode][] | undefined

      // Push effect handler frame if with-handlers exist
      if (withHandlerNodes && withHandlerNodes.length > 0) {
        // Eagerly evaluate effect expressions using recursive evaluator.
        // Effect expressions are always simple (effect(name) or variable refs),
        // so synchronous evaluation is safe.
        const evaluatedHandlers: EvaluatedWithHandler[] = withHandlerNodes.map(([effectExpr, handlerNode]) => ({
          effectRef: evaluateNodeRecursive(effectExpr, env) as Any,
          handlerNode,
        }))
        const withFrame: TryWithFrame = {
          type: 'TryWith',
          handlers: evaluatedHandlers,
          env,
          sourceCodeInfo,
        }
        if (catchExpression) {
          const catchFrame: TryCatchFrame = {
            type: 'TryCatch',
            errorSymbol: errorSymbol ? (errorSymbol as UserDefinedSymbolNode)[1] : null,
            catchNode: catchExpression,
            env,
            sourceCodeInfo,
          }
          return { type: 'Eval', node: tryExpression, env, k: [withFrame, catchFrame, ...k] }
        }
        return { type: 'Eval', node: tryExpression, env, k: [withFrame, ...k] }
      }

      // No with-handlers — just try/catch
      if (catchExpression) {
        const frame: TryCatchFrame = {
          type: 'TryCatch',
          errorSymbol: errorSymbol ? (errorSymbol as UserDefinedSymbolNode)[1] : null,
          catchNode: catchExpression,
          env,
          sourceCodeInfo,
        }
        return { type: 'Eval', node: tryExpression, env, k: [frame, ...k] }
      }
      // No catch, no with — just evaluate the body
      return { type: 'Eval', node: tryExpression, env, k }
    }

    // --- throw ---
    case specialExpressionTypes.throw: {
      const throwExpr = node[1][1] as AstNode
      const frame: ThrowFrame = {
        type: 'Throw',
        sourceCodeInfo,
      }
      return { type: 'Eval', node: throwExpr, env, k: [frame, ...k] }
    }

    // --- recur ---
    case specialExpressionTypes.recur: {
      const nodes = node[1][1] as AstNode[]
      if (nodes.length === 0) {
        return handleRecur([], k, sourceCodeInfo)
      }
      const frame: RecurFrame = {
        type: 'Recur',
        nodes,
        index: 1,
        params: [],
        env,
        sourceCodeInfo,
      }
      if (nodes.length === 1) {
        // Only one param — evaluate it, then recur
        const singleFrame: RecurFrame = { ...frame, index: 1 }
        return { type: 'Eval', node: nodes[0]!, env, k: [singleFrame, ...k] }
      }
      return { type: 'Eval', node: nodes[0]!, env, k: [frame, ...k] }
    }

    // --- array ---
    case specialExpressionTypes.array: {
      const nodes = node[1][1] as AstNode[]
      if (nodes.length === 0) {
        return { type: 'Value', value: [], k }
      }
      const firstNode = nodes[0]!
      const isFirstSpread = isSpreadNode(firstNode)
      const frame: ArrayBuildFrame = {
        type: 'ArrayBuild',
        nodes,
        index: 0,
        result: [],
        isSpread: isFirstSpread,
        env,
        sourceCodeInfo,
      }
      return {
        type: 'Eval',
        node: isFirstSpread ? firstNode[1] : firstNode,
        env,
        k: [frame, ...k],
      }
    }

    // --- object ---
    case specialExpressionTypes.object: {
      const nodes = node[1][1] as AstNode[]
      if (nodes.length === 0) {
        return { type: 'Value', value: {}, k }
      }
      const firstNode = nodes[0]!
      const isFirstSpread = isSpreadNode(firstNode)
      const frame: ObjectBuildFrame = {
        type: 'ObjectBuild',
        nodes,
        index: 0,
        result: {},
        currentKey: null,
        isSpread: isFirstSpread,
        env,
        sourceCodeInfo,
      }
      return {
        type: 'Eval',
        node: isFirstSpread ? firstNode[1] : firstNode,
        env,
        k: [frame, ...k],
      }
    }

    // --- lambda (fn / ->) ---
    case specialExpressionTypes['0_lambda']: {
      const fn = node[1][1] as [BindingTarget[], AstNode[], ...unknown[]]
      const docString = (node[1][2] ?? '') as string
      const evaluatedFunc = evaluateFunction(fn, env)
      const min = evaluatedFunc[0].filter(arg => arg[0] !== bindingTargetTypes.rest && arg[1][1] === undefined).length
      const max = evaluatedFunc[0].some(arg => arg[0] === bindingTargetTypes.rest) ? undefined : evaluatedFunc[0].length
      const arity = { min: min > 0 ? min : undefined, max }
      const litsFunction: LitsFunction = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo: node[2],
        functionType: 'UserDefined',
        name: undefined,
        evaluatedfunction: evaluatedFunc,
        arity,
        docString,
      }
      return { type: 'Value', value: litsFunction, k }
    }

    // --- defined? ---
    case specialExpressionTypes['defined?']: {
      const symbolNode = node[1][1] as SymbolNode
      if (!isUserDefinedSymbolNode(symbolNode)) {
        return { type: 'Value', value: true, k }
      }
      const lookUpResult = env.lookUp(symbolNode)
      return { type: 'Value', value: lookUpResult !== null, k }
    }

    // --- import ---
    case specialExpressionTypes.import: {
      const moduleName = node[1][1] as string
      // Check for value modules first (file modules from bundles)
      const valueModule = env.getValueModule(moduleName)
      if (valueModule.found) {
        return { type: 'Value', value: valueModule.value as Any, k }
      }
      // Fall back to builtin modules
      const litsModule = env.getModule(moduleName)
      if (!litsModule) {
        throw new LitsError(`Unknown module: '${moduleName}'`, sourceCodeInfo)
      }
      const result: Record<string, ModuleFunction> = {}
      for (const [functionName, expression] of Object.entries(litsModule.functions)) {
        result[functionName] = {
          [FUNCTION_SYMBOL]: true,
          sourceCodeInfo,
          functionType: 'Module',
          moduleName,
          functionName,
          arity: expression.arity,
        }
      }
      return { type: 'Value', value: result, k }
    }

    // --- effect ---
    case specialExpressionTypes.effect: {
      const effectName = node[1][1] as string
      return { type: 'Value', value: getEffectRef(effectName), k }
    }

    // --- perform ---
    case specialExpressionTypes.perform: {
      const effectExpr = node[1][1] as AstNode
      const argExprs = node[1][2] as AstNode[]
      const allNodes = [effectExpr, ...argExprs]
      if (allNodes.length === 1) {
        // Only the effect expression, no args — evaluate effect then dispatch
        const frame: PerformArgsFrame = {
          type: 'PerformArgs',
          argNodes: allNodes,
          index: 1,
          params: [],
          env,
          sourceCodeInfo,
        }
        return { type: 'Eval', node: allNodes[0]!, env, k: [frame, ...k] }
      }
      const frame: PerformArgsFrame = {
        type: 'PerformArgs',
        argNodes: allNodes,
        index: 1,
        params: [],
        env,
        sourceCodeInfo,
      }
      return { type: 'Eval', node: allNodes[0]!, env, k: [frame, ...k] }
    }

    /* v8 ignore next 2 */
    default:
      throw new LitsError(`Unknown special expression type: ${type}`, sourceCodeInfo)
  }
}

// ---------------------------------------------------------------------------
// dispatchCall — dispatch a function call after args are evaluated
// ---------------------------------------------------------------------------

/**
 * After all arguments are collected in an EvalArgsFrame, determine what
 * to call and return the next Step.
 */
function dispatchCall(frame: EvalArgsFrame, k: ContinuationStack): Step | Promise<Step> {
  const { node, params, placeholders, env, sourceCodeInfo } = frame

  if (isNormalExpressionNodeWithName(node)) {
    const nameSymbol = node[1][0]

    // --- Partial application ---
    if (placeholders.length > 0) {
      const fn = env.evaluateSymbol(nameSymbol)
      const partialFunction: PartialFunction = {
        [FUNCTION_SYMBOL]: true,
        function: asFunctionLike(fn, sourceCodeInfo),
        functionType: 'Partial',
        params,
        placeholders,
        sourceCodeInfo,
        arity: toFixedArity(placeholders.length),
      }
      return { type: 'Value', value: partialFunction, k }
    }

    // --- Named builtin ---
    if (isNormalBuiltinSymbolNode(nameSymbol)) {
      const builtinType = nameSymbol[1]
      const normalExpression = builtin.allNormalExpressions[builtinType]!
      if (env.pure && normalExpression.pure === false) {
        throw new LitsError(`Cannot call impure function '${normalExpression.name}' in pure mode`, sourceCodeInfo)
      }
      // Call the normal expression directly — it may use executeFunction internally
      const result = normalExpression.evaluate(params, sourceCodeInfo, env, { executeFunction: executeFunctionRecursive })
      return wrapMaybePromiseAsStep(result, k)
    }

    // --- Named user-defined ---
    const fn = env.getValue(nameSymbol[1])
    if (fn !== undefined) {
      return dispatchFunction(asFunctionLike(fn, sourceCodeInfo), params, placeholders, env, sourceCodeInfo, k)
    }
    throw new UndefinedSymbolError(nameSymbol[1], sourceCodeInfo)
  }
  else {
    // --- Anonymous function expression ---
    // The function expression is the first payload element; need to evaluate it
    const fnNode: AstNode = node[1][0]
    const callFrame: CallFnFrame = {
      type: 'CallFn',
      params,
      placeholders,
      env,
      sourceCodeInfo,
    }
    return { type: 'Eval', node: fnNode, env, k: [callFrame, ...k] }
  }
}

/**
 * Dispatch a resolved function value with pre-evaluated parameters.
 */
function dispatchFunction(fn: FunctionLike, params: Arr, placeholders: number[], env: ContextStack, sourceCodeInfo: SourceCodeInfo | undefined, k: ContinuationStack): Step | Promise<Step> {
  if (placeholders.length > 0) {
    const partialFunction: PartialFunction = {
      [FUNCTION_SYMBOL]: true,
      function: fn,
      functionType: 'Partial',
      params,
      placeholders,
      sourceCodeInfo,
      arity: toFixedArity(placeholders.length),
    }
    return { type: 'Value', value: partialFunction, k }
  }

  if (isLitsFunction(fn)) {
    return dispatchLitsFunction(fn, params, env, sourceCodeInfo, k)
  }

  // Non-function callables: arrays, objects, strings, numbers
  if (Array.isArray(fn)) {
    return { type: 'Value', value: evaluateArrayAsFunction(fn, params, sourceCodeInfo), k }
  }
  if (isObj(fn)) {
    return { type: 'Value', value: evaluateObjectAsFunction(fn, params, sourceCodeInfo), k }
  }
  if (typeof fn === 'string') {
    return { type: 'Value', value: evaluateStringAsFunction(fn, params, sourceCodeInfo), k }
  }
  if (isNumber(fn)) {
    return { type: 'Value', value: evaluateNumberAsFunction(fn, params, sourceCodeInfo), k }
  }
  /* v8 ignore next 1 */
  throw new LitsError('Unexpected function type', sourceCodeInfo)
}

/**
 * Dispatch a LitsFunction. User-defined functions are set up with frames;
 * compound function types (Comp, Juxt, etc.) use the recursive executor.
 */
function dispatchLitsFunction(fn: LitsFunction, params: Arr, env: ContextStack, sourceCodeInfo: SourceCodeInfo | undefined, k: ContinuationStack): Step | Promise<Step> {
  switch (fn.functionType) {
    case 'UserDefined': {
      return setupUserDefinedCall(fn, params, env, sourceCodeInfo, k)
    }
    // Compound function types: use recursive execution for now
    // (they involve internal iteration that would need its own frame types)
    case 'Partial':
    case 'Comp':
    case 'Constantly':
    case 'Juxt':
    case 'Complement':
    case 'EveryPred':
    case 'SomePred':
    case 'Fnull':
    case 'Builtin':
    case 'SpecialBuiltin':
    case 'Module':
    case 'NativeJsFunction': {
      const result = executeLitsFunctionRecursive(fn, params, env, sourceCodeInfo)
      return wrapMaybePromiseAsStep(result, k)
    }
  }
}

/**
 * Set up a user-defined function call: bind params, push FnBodyFrame.
 *
 * For Phase 1, parameter binding (including destructuring defaults) is done
 * synchronously via evaluateBindingNodeValues with the recursive evaluator.
 * This will be converted to use frames in a later phase.
 */
function setupUserDefinedCall(fn: UserDefinedFunction, params: Arr, env: ContextStack, sourceCodeInfo: SourceCodeInfo | undefined, k: ContinuationStack): Step | Promise<Step> {
  if (!arityAcceptsMin(fn.arity, params.length)) {
    throw new LitsError(`Expected ${fn.arity} arguments, got ${params.length}.`, sourceCodeInfo)
  }
  const evaluatedFunc = fn.evaluatedfunction
  const args = evaluatedFunc[0]
  const nbrOfNonRestArgs = args.filter(arg => arg[0] !== bindingTargetTypes.rest).length
  const newContextStack = env.create(fn.evaluatedfunction[2])
  const newContext: Context = { self: { value: fn } }
  const rest: Arr = []

  // Bind non-rest params synchronously
  for (let i = 0; i < params.length; i += 1) {
    if (i < nbrOfNonRestArgs) {
      const param = toAny(params[i])
      const valueRecord = evaluateBindingNodeValues(args[i]!, param, n =>
        evaluateNodeRecursive(n, newContextStack.create(newContext)))
      if (valueRecord instanceof Promise) {
        // Fall back to recursive execution for async binding
        const result = executeUserDefinedRecursive(fn, params, env, sourceCodeInfo)
        return wrapMaybePromiseAsStep(result, k)
      }
      Object.entries(valueRecord).forEach(([key, value]) => {
        newContext[key] = { value }
      })
    }
    else {
      rest.push(toAny(params[i]))
    }
  }

  // Handle default values for optional params
  for (let i = params.length; i < nbrOfNonRestArgs; i++) {
    const arg = args[i]!
    const defaultValue = evaluateNodeRecursive(arg[1][1]!, newContextStack.create(newContext))
    if (defaultValue instanceof Promise) {
      const result = executeUserDefinedRecursive(fn, params, env, sourceCodeInfo)
      return wrapMaybePromiseAsStep(result, k)
    }
    const valueRecord = evaluateBindingNodeValues(arg, defaultValue, n =>
      evaluateNodeRecursive(n, newContextStack.create(newContext)))
    if (valueRecord instanceof Promise) {
      const result = executeUserDefinedRecursive(fn, params, env, sourceCodeInfo)
      return wrapMaybePromiseAsStep(result, k)
    }
    Object.entries(valueRecord).forEach(([key, value]) => {
      newContext[key] = { value }
    })
  }

  // Handle rest argument
  const restArgument = args.find(arg => arg[0] === bindingTargetTypes.rest)
  if (restArgument) {
    const valueRecord = evaluateBindingNodeValues(restArgument, rest, n =>
      evaluateNodeRecursive(n, newContextStack.create(newContext)))
    if (valueRecord instanceof Promise) {
      const result = executeUserDefinedRecursive(fn, params, env, sourceCodeInfo)
      return wrapMaybePromiseAsStep(result, k)
    }
    Object.entries(valueRecord).forEach(([key, value]) => {
      newContext[key] = { value }
    })
  }

  // Evaluate the body as a sequence
  const bodyNodes = evaluatedFunc[1]
  const bodyEnv = newContextStack.create(newContext)

  if (bodyNodes.length === 0) {
    return { type: 'Value', value: null, k }
  }

  const fnBodyFrame: FnBodyFrame = {
    type: 'FnBody',
    fn,
    bodyIndex: 1,
    env: bodyEnv,
    outerEnv: env,
    sourceCodeInfo,
  }

  if (bodyNodes.length === 1) {
    return { type: 'Eval', node: bodyNodes[0]!, env: bodyEnv, k: [fnBodyFrame, ...k] }
  }

  return { type: 'Eval', node: bodyNodes[0]!, env: bodyEnv, k: [fnBodyFrame, ...k] }
}

// ---------------------------------------------------------------------------
// applyFrame — process a completed sub-result against a frame
// ---------------------------------------------------------------------------

/**
 * Given a completed sub-expression value and the top frame from the
 * continuation stack, determine the next Step.
 */
export function applyFrame(frame: Frame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  switch (frame.type) {
    case 'Sequence':
      return applySequence(frame, value, k)
    case 'IfBranch':
      return applyIfBranch(frame, value, k)
    case 'Cond':
      return applyCond(frame, value, k)
    case 'Match':
      return applyMatch(frame, value, k)
    case 'And':
      return applyAnd(frame, value, k)
    case 'Or':
      return applyOr(frame, value, k)
    case 'Qq':
      return applyQq(frame, value, k)
    case 'ArrayBuild':
      return applyArrayBuild(frame, value, k)
    case 'ObjectBuild':
      return applyObjectBuild(frame, value, k)
    case 'LetBind':
      return applyLetBind(frame, value, k)
    case 'LoopBind':
      return applyLoopBind(frame, value, k)
    case 'LoopIterate':
      return applyLoopIterate(frame, value, k)
    case 'ForLoop':
      return applyForLoop(frame, value, k)
    case 'Throw':
      return applyThrow(frame, value, k)
    case 'Recur':
      return applyRecur(frame, value, k)
    case 'PerformArgs':
      return applyPerformArgs(frame, value, k)
    case 'TryCatch':
      return applyTryCatch(value, k)
    case 'TryWith':
      return applyTryWith(value, k)
    case 'EffectResume':
      return applyEffectResume(frame, value, k)
    case 'EvalArgs':
      return applyEvalArgs(frame, value, k)
    case 'CallFn':
      return applyCallFn(frame, value, k)
    case 'FnBody':
      return applyFnBody(frame, value, k)
    case 'BindingDefault':
      return applyBindingDefault(frame, value, k)
    case 'NanCheck':
      return applyNanCheck(frame, value, k)
    /* v8 ignore next 2 */
    default: {
      const _exhaustive: never = frame
      throw new LitsError(`Unhandled frame type: ${(_exhaustive as Frame).type}`, undefined)
    }
  }
}

// ---------------------------------------------------------------------------
// Frame apply handlers
// ---------------------------------------------------------------------------

function applySequence(frame: SequenceFrame, _value: Any, k: ContinuationStack): Step {
  const { nodes, index, env } = frame
  if (index >= nodes.length) {
    // All nodes evaluated — return the last value
    return { type: 'Value', value: _value, k }
  }
  // More nodes to evaluate
  const newFrame: SequenceFrame = { ...frame, index: index + 1 }
  if (index === nodes.length - 1) {
    // Last node — no need for frame
    return { type: 'Eval', node: nodes[index]!, env, k }
  }
  return { type: 'Eval', node: nodes[index]!, env, k: [newFrame, ...k] }
}

function applyIfBranch(frame: IfBranchFrame, value: Any, k: ContinuationStack): Step {
  const { thenNode, elseNode, inverted, env } = frame
  const condition = inverted ? !value : value
  if (condition) {
    return { type: 'Eval', node: thenNode, env, k }
  }
  if (elseNode) {
    return { type: 'Eval', node: elseNode, env, k }
  }
  return { type: 'Value', value: null, k }
}

function applyCond(frame: CondFrame, value: Any, k: ContinuationStack): Step {
  const { cases, index, env } = frame

  if (frame.phase === 'test') {
    if (value) {
      // Test is truthy — evaluate the body
      return { type: 'Eval', node: cases[index]![1], env, k }
    }
    // Test is falsy — try next case
    const nextIndex = index + 1
    if (nextIndex >= cases.length) {
      return { type: 'Value', value: null, k }
    }
    const newFrame: CondFrame = { ...frame, index: nextIndex }
    return { type: 'Eval', node: cases[nextIndex]![0], env, k: [newFrame, ...k] }
  }

  // phase === 'body' — body has been evaluated
  return { type: 'Value', value, k }
}

function applyMatch(frame: MatchFrame, value: Any, k: ContinuationStack): Step {
  const { cases, env } = frame

  if (frame.phase === 'matchValue') {
    // matchValue has been evaluated — start processing cases
    const matchValue = value
    return processMatchCase({ ...frame, matchValue, phase: 'guard' }, k)
  }

  if (frame.phase === 'guard') {
    // Guard was evaluated
    if (!value) {
      // Guard failed — try next case
      const newFrame: MatchFrame = { ...frame, index: frame.index + 1, bindings: {} }
      return processMatchCase(newFrame, k)
    }
    // Guard passed — evaluate body
    const context: Context = {}
    for (const [name, val] of Object.entries(frame.bindings)) {
      context[name] = { value: val }
    }
    const newEnv = env.create(context)
    return { type: 'Eval', node: cases[frame.index]![1], env: newEnv, k }
  }

  // phase === 'body' — body has been evaluated
  return { type: 'Value', value, k }
}

/**
 * Process match cases starting from `frame.index`.
 * Uses recursive tryMatch with evaluateNodeRecursive for pattern matching.
 */
function processMatchCase(frame: MatchFrame, k: ContinuationStack): Step {
  const { matchValue, cases, index, env, sourceCodeInfo } = frame

  for (let i = index; i < cases.length; i++) {
    const [pattern, body, guard] = cases[i]!
    const bindings = tryMatch(pattern, matchValue, n => evaluateNodeRecursive(n, env))

    if (bindings instanceof Promise) {
      // Async tryMatch — fall back to recursive evaluation
      // This handles the case where pattern matching involves async operations
      throw new LitsError('Async pattern matching not supported in trampoline yet', sourceCodeInfo)
    }

    if (bindings === null) {
      continue // Pattern didn't match — try next case
    }

    // Pattern matched
    if (guard) {
      // Need to evaluate guard with bindings in scope
      const context: Context = {}
      for (const [name, val] of Object.entries(bindings)) {
        context[name] = { value: val }
      }
      const guardEnv = env.create(context)
      const guardFrame: MatchFrame = { ...frame, phase: 'guard', index: i, bindings }
      return { type: 'Eval', node: guard, env: guardEnv, k: [guardFrame, ...k] }
    }

    // No guard — evaluate body directly
    const context: Context = {}
    for (const [name, val] of Object.entries(bindings)) {
      context[name] = { value: val }
    }
    const bodyEnv = env.create(context)
    return { type: 'Eval', node: body, env: bodyEnv, k }
  }

  // No case matched
  return { type: 'Value', value: null, k }
}

function applyAnd(frame: AndFrame, value: Any, k: ContinuationStack): Step {
  if (!value) {
    return { type: 'Value', value, k }
  }
  const { nodes, index, env } = frame
  if (index >= nodes.length) {
    return { type: 'Value', value, k }
  }
  if (index === nodes.length - 1) {
    // Last node — no need for frame
    return { type: 'Eval', node: nodes[index]!, env, k }
  }
  const newFrame: AndFrame = { ...frame, index: index + 1 }
  return { type: 'Eval', node: nodes[index]!, env, k: [newFrame, ...k] }
}

function applyOr(frame: OrFrame, value: Any, k: ContinuationStack): Step {
  if (value) {
    return { type: 'Value', value, k }
  }
  const { nodes, index, env } = frame
  if (index >= nodes.length) {
    return { type: 'Value', value, k }
  }
  if (index === nodes.length - 1) {
    return { type: 'Eval', node: nodes[index]!, env, k }
  }
  const newFrame: OrFrame = { ...frame, index: index + 1 }
  return { type: 'Eval', node: nodes[index]!, env, k: [newFrame, ...k] }
}

function applyQq(frame: QqFrame, value: Any, k: ContinuationStack): Step {
  // If value is non-null, we found our result
  if (value !== null) {
    return { type: 'Value', value, k }
  }
  return advanceQq(frame, k)
}

/** Advance ?? to the next node, skipping undefined user symbols. */
function advanceQq(frame: QqFrame, k: ContinuationStack): Step {
  const { nodes, env } = frame
  let { index } = frame

  // Skip undefined user symbols
  while (index < nodes.length) {
    const node = nodes[index]!
    if (isUserDefinedSymbolNode(node) && env.lookUp(node) === null) {
      index++
      continue
    }
    break
  }

  if (index >= nodes.length) {
    return { type: 'Value', value: null, k }
  }

  if (index === nodes.length - 1) {
    // Last node — no need for frame
    return { type: 'Eval', node: nodes[index]!, env, k }
  }

  const newFrame: QqFrame = { ...frame, index: index + 1 }
  return { type: 'Eval', node: nodes[index]!, env, k: [newFrame, ...k] }
}

/** Skip undefined user symbols in ?? until we find one to evaluate. */
function skipUndefinedQq(frame: QqFrame, k: ContinuationStack): Step {
  return advanceQq(frame, k)
}

function applyArrayBuild(frame: ArrayBuildFrame, value: Any, k: ContinuationStack): Step {
  const { nodes, result, env, sourceCodeInfo } = frame

  // Process the completed value
  if (frame.isSpread) {
    if (!Array.isArray(value)) {
      throw new LitsError('Spread value is not an array', sourceCodeInfo)
    }
    result.push(...value)
  }
  else {
    result.push(value)
  }

  // Advance to next element
  const nextIndex = frame.index + 1
  if (nextIndex >= nodes.length) {
    return { type: 'Value', value: result, k }
  }

  const nextNode = nodes[nextIndex]!
  const isNextSpread = isSpreadNode(nextNode)
  const newFrame: ArrayBuildFrame = { ...frame, index: nextIndex, isSpread: isNextSpread }
  return {
    type: 'Eval',
    node: isNextSpread ? nextNode[1] : nextNode,
    env,
    k: [newFrame, ...k],
  }
}

function applyObjectBuild(frame: ObjectBuildFrame, value: Any, k: ContinuationStack): Step {
  const { nodes, result, env, sourceCodeInfo } = frame

  if (frame.isSpread) {
    // Spread value should be an object
    if (!isUnknownRecord(value)) {
      throw new LitsError('Spread value is not an object', sourceCodeInfo)
    }
    Object.assign(result, value)
    // Advance to next entry
    const nextIndex = frame.index + 1
    if (nextIndex >= nodes.length) {
      return { type: 'Value', value: result, k }
    }
    const nextNode = nodes[nextIndex]!
    const isNextSpread = isSpreadNode(nextNode)
    const newFrame: ObjectBuildFrame = { ...frame, index: nextIndex, currentKey: null, isSpread: isNextSpread }
    return {
      type: 'Eval',
      node: isNextSpread ? nextNode[1] : nextNode,
      env,
      k: [newFrame, ...k],
    }
  }

  if (frame.currentKey === null) {
    // We just evaluated a key expression
    assertString(value, sourceCodeInfo)
    const valueNode = nodes[frame.index + 1]
    if (valueNode === undefined) {
      throw new LitsError('Missing value for key', sourceCodeInfo)
    }
    const newFrame: ObjectBuildFrame = { ...frame, currentKey: value }
    return { type: 'Eval', node: valueNode, env, k: [newFrame, ...k] }
  }
  else {
    // We just evaluated a value expression
    result[frame.currentKey] = value
    // Advance to next key-value pair
    const nextIndex = frame.index + 2
    if (nextIndex >= nodes.length) {
      return { type: 'Value', value: result, k }
    }
    const nextNode = nodes[nextIndex]!
    const isNextSpread = isSpreadNode(nextNode)
    const newFrame: ObjectBuildFrame = { ...frame, index: nextIndex, currentKey: null, isSpread: isNextSpread }
    return {
      type: 'Eval',
      node: isNextSpread ? nextNode[1] : nextNode,
      env,
      k: [newFrame, ...k],
    }
  }
}

function applyLetBind(frame: LetBindFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  const { target, env, sourceCodeInfo } = frame

  // Process the binding using the recursive helper
  const bindingResult = evaluateBindingNodeValues(target, value, n => evaluateNodeRecursive(n, env))
  return chain(bindingResult, (br) => {
    env.addValues(br, sourceCodeInfo)
    return { type: 'Value' as const, value, k }
  })
}

function applyLoopBind(frame: LoopBindFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  const { bindingNodes, index, context, body, env, sourceCodeInfo } = frame

  // Value for the current binding has been evaluated
  const bindingNode = bindingNodes[index]!
  const target = bindingNode[1][0]

  const valueRecord = evaluateBindingNodeValues(target, value, n => evaluateNodeRecursive(n, env.create(context)))
  return chain(valueRecord, (vr) => {
    Object.entries(vr).forEach(([name, val]) => {
      context[name] = { value: val }
    })

    // Move to next binding
    const nextIndex = index + 1
    if (nextIndex >= bindingNodes.length) {
      // All bindings done — set up the loop iteration
      const loopEnv = env.create(context)
      const iterateFrame: LoopIterateFrame = {
        type: 'LoopIterate',
        bindingNodes,
        bindingContext: context,
        body,
        env: loopEnv,
        sourceCodeInfo,
      }
      return { type: 'Eval' as const, node: body, env: loopEnv, k: [iterateFrame, ...k] }
    }

    // Evaluate next binding's value expression (in context with previous bindings)
    const newFrame: LoopBindFrame = { ...frame, index: nextIndex }
    return { type: 'Eval' as const, node: bindingNodes[nextIndex]![1][1], env: env.create(context), k: [newFrame, ...k] }
  })
}

function applyLoopIterate(_frame: LoopIterateFrame, value: Any, k: ContinuationStack): Step {
  // Body has been evaluated successfully — return the value
  // (recur is handled by the RecurFrame, which will pop back to this frame)
  return { type: 'Value', value, k }
}

function applyForLoop(frame: ForLoopFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  const { returnResult, bindingNodes, result, env, sourceCodeInfo, context } = frame
  const { asColl, isSeq } = getCollectionUtils()

  switch (frame.phase) {
    case 'evalCollection': {
      // A collection expression has been evaluated
      const coll = asColl(value, sourceCodeInfo)
      const seq = isSeq(coll) ? coll : Object.entries(coll as Obj)

      if ((seq as Arr).length === 0) {
        // Empty collection — abort this level
        return handleForAbort(frame, k)
      }

      // Store collection for this level
      const levelStates = [...frame.levelStates]
      levelStates[frame.bindingLevel] = { collection: seq as Arr, index: 0 }

      // Process the first element's binding
      const binding = bindingNodes[frame.bindingLevel]!
      const targetNode = binding[0][1][0]
      const element = (seq as Arr)[0]

      const elValue = asAny(element, sourceCodeInfo)
      const valueRecord = evaluateBindingNodeValues(targetNode, elValue, n => evaluateNodeRecursive(n, env))
      return chain(valueRecord, (vr) => {
        Object.entries(vr).forEach(([name, val]) => {
          context[name] = { value: val }
        })

        // Process let-bindings if any
        const letBindings = binding[1]
        if (letBindings.length > 0) {
          return processForLetBindings(frame, levelStates, letBindings, 0, k)
        }

        // Process when-guard if any
        return processForGuards(frame, levelStates, k)
      })
    }

    case 'evalLet': {
      // A let-binding value has been evaluated; handled via recursive fallback
      // (processForLetBindings handles this inline)
      /* v8 ignore next 1 */
      throw new LitsError('ForLoop evalLet should not reach applyFrame', sourceCodeInfo)
    }

    case 'evalWhen': {
      // When-guard has been evaluated
      if (!value) {
        // When-guard failed — advance to next element
        return advanceForElement(frame, k)
      }
      // Check while-guard
      const binding = bindingNodes[frame.bindingLevel]!
      const whileNode = binding[3]
      if (whileNode) {
        const newFrame: ForLoopFrame = { ...frame, phase: 'evalWhile' }
        return { type: 'Eval', node: whileNode, env, k: [newFrame, ...k] }
      }
      return processForNextLevel(frame, k)
    }

    case 'evalWhile': {
      if (!value) {
        // While-guard failed — skip remaining elements at this level
        const levelStates = [...frame.levelStates]
        levelStates[frame.bindingLevel] = {
          ...levelStates[frame.bindingLevel]!,
          index: Number.POSITIVE_INFINITY,
        }
        return advanceForElement({ ...frame, levelStates }, k)
      }
      return processForNextLevel(frame, k)
    }

    case 'evalBody': {
      // Body has been evaluated
      if (returnResult) {
        result.push(value)
      }
      // Advance innermost binding to next element
      return advanceForElement(frame, k)
    }

    /* v8 ignore next 2 */
    case 'evalElement':
      throw new LitsError(`Unexpected ForLoop phase: ${frame.phase}`, sourceCodeInfo)
  }

  /* v8 ignore next 1 */
  return { type: 'Value', value: null, k }
}

/** Handle for-loop abort: no more elements at the outermost level. */
function handleForAbort(frame: ForLoopFrame, k: ContinuationStack): Step {
  return { type: 'Value', value: frame.returnResult ? frame.result : null, k }
}

/** Advance to the next element at the current binding level. */
function advanceForElement(frame: ForLoopFrame, k: ContinuationStack): Step | Promise<Step> {
  const { bindingNodes, env, sourceCodeInfo, context } = frame
  const levelStates = [...frame.levelStates]
  const bindingLevel = frame.bindingLevel

  // Advance the innermost level
  const currentLevel = bindingLevel
  const currentState = levelStates[currentLevel]!
  const nextElementIndex = currentState.index + 1

  if (nextElementIndex >= currentState.collection.length) {
    // No more elements at this level — back up
    if (currentLevel === 0) {
      return handleForAbort(frame, k)
    }
    // Move to next element of the parent level
    return advanceForElement({ ...frame, bindingLevel: currentLevel - 1 }, k)
  }

  // Process next element at current level
  levelStates[currentLevel] = { ...currentState, index: nextElementIndex }
  const binding = bindingNodes[currentLevel]!
  const targetNode = binding[0][1][0]
  const element = currentState.collection[nextElementIndex]
  const elValue = asAny(element, sourceCodeInfo)

  const valueRecord = evaluateBindingNodeValues(targetNode, elValue, n => evaluateNodeRecursive(n, env))
  return chain(valueRecord, (vr) => {
    Object.entries(vr).forEach(([name, val]) => {
      context[name] = { value: val }
    })

    // Process let-bindings
    const letBindings = binding[1]
    if (letBindings.length > 0) {
      return processForLetBindings({ ...frame, levelStates, bindingLevel: currentLevel }, levelStates, letBindings, 0, k)
    }

    return processForGuards({ ...frame, levelStates, bindingLevel: currentLevel }, levelStates, k)
  })
}

/** Process let-bindings at the current for-loop level. */
function processForLetBindings(frame: ForLoopFrame, levelStates: ForLoopFrame['levelStates'], letBindings: BindingNode[], letIndex: number, k: ContinuationStack): Step | Promise<Step> {
  const { env, context } = frame

  let result: MaybePromise<void> = undefined as unknown as void
  for (let i = letIndex; i < letBindings.length; i++) {
    const bindingIndex = i
    result = chain(result, () => {
      const bindingNode = letBindings[bindingIndex]!
      const [target, bindingValue] = bindingNode[1]
      const val = evaluateNodeRecursive(bindingValue, env)
      return chain(val, (v) => {
        const valueRecord = evaluateBindingNodeValues(target, v, n => evaluateNodeRecursive(n, env))
        return chain(valueRecord, (vr) => {
          Object.entries(vr).forEach(([name, value]) => {
            context[name] = { value }
          })
        })
      })
    })
  }

  return chain(result, () => processForGuards({ ...frame, levelStates }, levelStates, k))
}

/** Process when/while guards at the current level. */
function processForGuards(frame: ForLoopFrame, levelStates: ForLoopFrame['levelStates'], k: ContinuationStack): Step {
  const { bindingNodes, env } = frame
  const binding = bindingNodes[frame.bindingLevel]!
  const whenNode = binding[2]
  const whileNode = binding[3]

  if (whenNode) {
    const newFrame: ForLoopFrame = { ...frame, levelStates, phase: 'evalWhen' }
    return { type: 'Eval', node: whenNode, env, k: [newFrame, ...k] }
  }

  if (whileNode) {
    const newFrame: ForLoopFrame = { ...frame, levelStates, phase: 'evalWhile' }
    return { type: 'Eval', node: whileNode, env, k: [newFrame, ...k] }
  }

  return processForNextLevel({ ...frame, levelStates }, k)
}

/** After guards pass, either go deeper (more binding levels) or evaluate body. */
function processForNextLevel(frame: ForLoopFrame, k: ContinuationStack): Step {
  const { bindingNodes, body, env } = frame
  const nextLevel = frame.bindingLevel + 1

  if (nextLevel < bindingNodes.length) {
    // Go deeper — evaluate the next level's collection
    const binding = bindingNodes[nextLevel]!
    const collectionNode = binding[0][1][1]
    const newFrame: ForLoopFrame = {
      ...frame,
      phase: 'evalCollection',
      bindingLevel: nextLevel,
    }
    return { type: 'Eval', node: collectionNode, env, k: [newFrame, ...k] }
  }

  // All levels bound — evaluate the body
  const newFrame: ForLoopFrame = { ...frame, phase: 'evalBody' }
  return { type: 'Eval', node: body, env, k: [newFrame, ...k] }
}

/**
 * Search the continuation stack for the nearest TryCatchFrame.
 * If found, evaluate the catch body with the error bound (if errorSymbol is set).
 * If not found, re-throw the error.
 */
function unwindToTryCatch(error: unknown, k: ContinuationStack): Step {
  for (let i = 0; i < k.length; i++) {
    const f = k[i]!
    if (f.type === 'TryCatch') {
      const { errorSymbol, catchNode, env } = f
      const catchContext: Context = errorSymbol
        ? { [errorSymbol]: { value: error as Any } }
        : {}
      const remainingK = k.slice(i + 1)
      return { type: 'Eval', node: catchNode, env: env.create(catchContext), k: remainingK }
    }
  }
  // No TryCatchFrame found — re-throw the error
  throw error
}

function applyThrow(frame: ThrowFrame, value: Any, k: ContinuationStack): Step {
  assertString(value, frame.sourceCodeInfo, { nonEmpty: true })
  const error = new UserDefinedError(value, frame.sourceCodeInfo)
  return unwindToTryCatch(error, k)
}

function applyRecur(frame: RecurFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  const { nodes, index, params, env } = frame
  params.push(value)

  if (index >= nodes.length) {
    // All recur params collected — handle recur via continuation stack
    return handleRecur(params, k, frame.sourceCodeInfo)
  }

  // Evaluate next param
  const newFrame: RecurFrame = { ...frame, index: index + 1 }
  return { type: 'Eval', node: nodes[index]!, env, k: [newFrame, ...k] }
}

/**
 * Handle recur by searching the continuation stack for the nearest
 * LoopIterateFrame or FnBodyFrame, rebinding parameters, and restarting.
 * This replaces the exception-based RecurSignal approach from the recursive
 * evaluator with proper continuation-based control flow.
 */
function handleRecur(params: Arr, k: ContinuationStack, sourceCodeInfo: SourceCodeInfo | undefined): Step | Promise<Step> {
  for (let i = 0; i < k.length; i++) {
    const frame = k[i]!

    if (frame.type === 'LoopIterate') {
      // Found loop frame — rebind variables and re-evaluate body
      const { bindingNodes, bindingContext, body, env } = frame
      const remainingK = k.slice(i + 1)

      if (params.length !== bindingNodes.length) {
        throw new LitsError(
          `recur expected ${bindingNodes.length} parameters, got ${params.length}`,
          sourceCodeInfo,
        )
      }

      const rebindAll: MaybePromise<void> = forEachSequential(
        bindingNodes,
        (bindingNode, j) => {
          const target = bindingNode[1][0]
          const param = toAny(params[j])
          return chain(
            evaluateBindingNodeValues(target, param, n => evaluateNodeRecursive(n, env)),
            (valueRecord) => {
              Object.entries(valueRecord).forEach(([name, val]) => {
                bindingContext[name] = { value: val }
              })
            },
          )
        },
      )

      return chain(rebindAll, () => {
        // Push fresh LoopIterateFrame and re-evaluate body
        const newIterateFrame: LoopIterateFrame = {
          type: 'LoopIterate',
          bindingNodes,
          bindingContext,
          body,
          env,
          sourceCodeInfo: frame.sourceCodeInfo,
        }
        return { type: 'Eval' as const, node: body, env, k: [newIterateFrame, ...remainingK] }
      })
    }

    if (frame.type === 'FnBody') {
      // Found function body frame — restart with new params
      const { fn, outerEnv } = frame
      const remainingK = k.slice(i + 1)
      return setupUserDefinedCall(fn, params, outerEnv, frame.sourceCodeInfo, remainingK)
    }
  }

  throw new LitsError('recur called outside of loop or function body', sourceCodeInfo)
}

function applyTryCatch(_value: Any, k: ContinuationStack): Step {
  // Try body completed successfully — the catch frame is discarded.
  // The value propagates up past the TryCatchFrame.
  return { type: 'Value', value: _value, k }
}

function applyTryWith(_value: Any, k: ContinuationStack): Step {
  // Try body completed successfully — the with frame is discarded.
  // The value propagates up past the TryWithFrame.
  return { type: 'Value', value: _value, k }
}

function applyEffectResume(frame: EffectResumeFrame, value: Any, _k: ContinuationStack): Step {
  // The handler returned a value. Replace the continuation with resumeK
  // (the original continuation from the perform call site, with TryWithFrame
  // still on the stack for subsequent performs).
  // The _k (handler's remaining outer_k) is discarded — resumeK already
  // includes the full original continuation.
  return { type: 'Value', value, k: frame.resumeK }
}

function applyPerformArgs(frame: PerformArgsFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  const { argNodes, index, params, env } = frame
  params.push(value)

  if (index >= argNodes.length) {
    // All values collected — first is the effect ref, rest are args
    const effectRef = params[0]!
    assertEffectRef(effectRef, frame.sourceCodeInfo)
    const args = params.slice(1)
    // Produce a PerformStep — let the trampoline dispatch it
    return { type: 'Perform', effect: effectRef, args, k }
  }

  // Evaluate next arg
  const newFrame: PerformArgsFrame = { ...frame, index: index + 1 }
  return { type: 'Eval', node: argNodes[index]!, env, k: [newFrame, ...k] }
}

/**
 * Dispatch a Perform step by searching the continuation stack for a matching
 * TryWithFrame. If found, evaluate the handler and use its return value as the
 * result of the perform call. If not found, throw an unhandled effect error.
 *
 * Handler semantics (per P&P / Lits contract):
 * - Handler receives the perform args as an array: `([arg1, arg2]) -> ...`
 * - Handler's return value IS the resume value — no explicit resume needed
 * - Handlers run OUTSIDE the try/with scope — the TryWithFrame is removed
 *   from the handler's error/effect path. An EffectResumeFrame bridges the
 *   handler's return value back to the original continuation.
 * - The handler function's environment is the one captured at the with-clause,
 *   NOT the environment at the perform call site
 *
 * Continuation structure:
 *   Original k:   [...body_k, TryWithFrame(i), TryCatchFrame?(i+1), ...outer_k]
 *   Handler's k:  [EffectResumeFrame{resumeK=k}, ...outer_k]
 *   When handler returns V: EffectResumeFrame replaces k with original k,
 *   so V flows through body_k with TryWithFrame still on stack.
 */
function dispatchPerform(effect: EffectRef, args: Arr, k: ContinuationStack, sourceCodeInfo?: SourceCodeInfo, handlers?: Handlers, signal?: AbortSignal): Step | Promise<Step> {
  for (let i = 0; i < k.length; i++) {
    const frame = k[i]!
    if (frame.type === 'TryWith') {
      // Search this frame's handlers for a matching effect
      for (const handler of frame.handlers) {
        if (isEffectRef(handler.effectRef) && handler.effectRef.name === effect.name) {
          // Found a match!
          // resumeK = original k — handler's return value resumes here
          // (TryWithFrame stays on the stack for subsequent performs)
          const resumeK = k

          // Determine outer_k — skip TryWithFrame and any adjacent TryCatchFrame
          // from the same try/with/catch block, so errors and effects from the
          // handler propagate upward past the current try block.
          let skipEnd = i + 1
          if (skipEnd < k.length && k[skipEnd]!.type === 'TryCatch') {
            skipEnd++ // also skip the TryCatchFrame from the same block
          }
          const outerK = k.slice(skipEnd)

          // Handler's continuation: EffectResumeFrame bridges back to resumeK
          const effectResumeFrame: EffectResumeFrame = {
            type: 'EffectResume',
            resumeK,
            sourceCodeInfo,
          }
          const handlerK: ContinuationStack = [effectResumeFrame, ...outerK]

          // Evaluate the handler fn expression using recursive evaluator.
          // Handler expressions are always simple (lambda or variable refs).
          const handlerFn = evaluateNodeRecursive(handler.handlerNode, frame.env) as Any
          const fnLike = asFunctionLike(handlerFn, frame.sourceCodeInfo)
          // Call the handler with args as an array
          return dispatchFunction(fnLike, [args], [], frame.env, sourceCodeInfo, handlerK)
        }
      }
    }
  }

  // No matching local handler found — dispatch to host handler if available.
  const hostHandler = handlers?.[effect.name]
  if (hostHandler) {
    return dispatchHostHandler(hostHandler, args, k, signal, sourceCodeInfo)
  }

  // No host handler either — unhandled effect.
  throw new LitsError(`Unhandled effect: '${effect.name}'`, sourceCodeInfo)
}

/**
 * Dispatch an effect to a host-provided JavaScript handler.
 *
 * Creates an `EffectContext` with `resume` and `suspend` callbacks, then
 * calls the handler and returns a `Promise<Step>` that resolves when the
 * handler calls one of those callbacks:
 *
 * - `resume(value)` — resolves with a `ValueStep` that continues evaluation
 *   at the `perform` call site with the provided value and the original `k`.
 *   If `value` is a `Promise`, it's awaited first.
 * - `suspend(meta?)` — rejects with a `SuspensionSignal` carrying the
 *   continuation `k` and optional metadata. The effect trampoline loop
 *   catches this and returns `RunResult.suspended`.
 *
 * Host handler errors are treated as Lits-level errors — they're fed to
 * `unwindToTryCatch` so that Lits `try/catch` blocks can intercept them.
 */
function dispatchHostHandler(
  handler: EffectHandler,
  args: Arr,
  k: ContinuationStack,
  signal: AbortSignal | undefined,
  sourceCodeInfo: SourceCodeInfo | undefined,
): Promise<Step> {
  const effectSignal = signal ?? new AbortController().signal

  return new Promise<Step>((resolve, reject) => {
    let settled = false

    const ctx = {
      args: Array.from(args) as Any[],
      signal: effectSignal,
      resume: (value: Any | Promise<Any>) => {
        if (settled) {
          throw new LitsError('Effect handler called resume() more than once or after suspend()', sourceCodeInfo)
        }
        settled = true

        if (value instanceof Promise) {
          value.then(
            (v) => {
              resolve({ type: 'Value', value: v, k })
            },
            (e) => {
              // The promise-value rejected — treat as a Lits-level error
              // so try/catch in the Lits program can handle it.
              try {
                resolve(unwindToTryCatch(e instanceof Error ? new LitsError(e, sourceCodeInfo) : new LitsError(`${e}`, sourceCodeInfo), k))
              }
              catch (unwoundError) {
                reject(unwoundError)
              }
            },
          )
        }
        else {
          resolve({ type: 'Value', value, k })
        }
      },
      suspend: (meta?: Any) => {
        if (settled) {
          throw new LitsError('Effect handler called suspend() more than once or after resume()', sourceCodeInfo)
        }
        settled = true
        reject(new SuspensionSignal(k, meta))
      },
    }

    handler(ctx).catch((e) => {
      if (settled) {
        // Handler already resolved via resume/suspend — ignore late errors
        return
      }
      settled = true
      if (isSuspensionSignal(e)) {
        reject(e)
      }
      else {
        // Handler itself threw — treat as a Lits-level error.
        try {
          resolve(unwindToTryCatch(e instanceof Error ? new LitsError(e, sourceCodeInfo) : new LitsError(`${e}`, sourceCodeInfo), k))
        }
        catch (unwoundError) {
          reject(unwoundError)
        }
      }
    })
  })
}

function applyEvalArgs(frame: EvalArgsFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  const { node, params, placeholders, env } = frame
  const argNodes = node[1][1]
  const currentArgNode = argNodes[frame.index]!

  // Process the completed value
  if (isSpreadNode(currentArgNode)) {
    if (!Array.isArray(value)) {
      throw new LitsError(`Spread operator requires an array, got ${valueToString(value)}`, currentArgNode[2])
    }
    params.push(...value)
  }
  else {
    params.push(value)
  }

  // Find the next real argument (skip placeholders)
  let nextIndex = frame.index + 1
  while (nextIndex < argNodes.length) {
    const nextArg = argNodes[nextIndex]!
    if (nextArg[0] === NodeTypes.ReservedSymbol && nextArg[1] === '_') {
      placeholders.push(params.length)
      nextIndex++
    }
    else {
      break
    }
  }

  if (nextIndex >= argNodes.length) {
    // All args evaluated — dispatch the call
    return dispatchCall({ ...frame, index: nextIndex }, k)
  }

  // Evaluate next argument
  const newFrame: EvalArgsFrame = { ...frame, index: nextIndex }
  const nextArg = argNodes[nextIndex]!
  if (isSpreadNode(nextArg)) {
    return { type: 'Eval', node: nextArg[1], env, k: [newFrame, ...k] }
  }
  return { type: 'Eval', node: nextArg, env, k: [newFrame, ...k] }
}

function applyCallFn(frame: CallFnFrame, value: Any, k: ContinuationStack): Step | Promise<Step> {
  // `value` is the resolved function value
  const fn = asFunctionLike(value, frame.sourceCodeInfo)
  return dispatchFunction(fn, frame.params, frame.placeholders, frame.env, frame.sourceCodeInfo, k)
}

function applyFnBody(frame: FnBodyFrame, value: Any, k: ContinuationStack): Step {
  const { fn, bodyIndex, env } = frame
  const bodyNodes = fn.evaluatedfunction[1]

  if (bodyIndex >= bodyNodes.length) {
    // All body nodes evaluated — return the result
    return { type: 'Value', value, k }
  }

  // More body nodes to evaluate.
  // The FnBodyFrame is always pushed — even for the last body node — because
  // `handleRecur` walks the continuation stack looking for it. When recur fires
  // inside the last expression, handleRecur finds this frame, slices the stack
  // at that point, and calls setupUserDefinedCall with the remaining stack.
  // This replaces the old FnBodyFrame rather than growing the stack — achieving
  // proper tail call elimination.
  const newFrame: FnBodyFrame = { ...frame, bodyIndex: bodyIndex + 1 }
  return { type: 'Eval', node: bodyNodes[bodyIndex]!, env, k: [newFrame, ...k] }
}

function applyBindingDefault(frame: BindingDefaultFrame, value: Any, k: ContinuationStack): Step {
  // Default value has been evaluated — continue binding processing
  // For Phase 1, this is handled by the recursive evaluateBindingNodeValues
  // This frame type will be fully utilized in later phases.
  const { target, record, env, sourceCodeInfo } = frame
  const valueRecord = evaluateBindingNodeValues(target, value, n => evaluateNodeRecursive(n, env))
  if (valueRecord instanceof Promise) {
    throw new LitsError('Async binding default evaluation not supported in trampoline yet', sourceCodeInfo)
  }
  Object.assign(record, valueRecord)
  return { type: 'Value', value, k }
}

function applyNanCheck(frame: NanCheckFrame, value: Any, k: ContinuationStack): Step {
  if (typeof value === 'number' && Number.isNaN(value)) {
    throw new LitsError('Number is NaN', frame.sourceCodeInfo)
  }
  return { type: 'Value', value: annotate(value), k }
}

// ---------------------------------------------------------------------------
// Utility functions
// ---------------------------------------------------------------------------

/**
 * Wrap a MaybePromise<Any> result into a Step or Promise<Step>.
 * If the result is a value, return a ValueStep immediately.
 * If it's a Promise, return a Promise<Step> that resolves to a ValueStep.
 * The trampoline loop handles the async case: runSyncTrampoline throws,
 * runAsyncTrampoline awaits.
 */
function wrapMaybePromiseAsStep(result: MaybePromise<Any>, k: ContinuationStack): Step | Promise<Step> {
  if (result instanceof Promise) {
    return result.then(
      value => ({ type: 'Value' as const, value, k }),
      error => unwindToTryCatch(error, k),
    )
  }
  return { type: 'Value', value: result, k }
}

/** Lazy-load collection utilities to avoid circular imports. */
function getCollectionUtils(): { asColl: (v: Any, s?: SourceCodeInfo) => Any, isSeq: (v: Any) => boolean } {
  return {
    asColl: (v: Any, s?: SourceCodeInfo) => {
      if (typeof v === 'string' || Array.isArray(v) || isObj(v)) {
        return v
      }
      throw new LitsError(`Expected collection, got ${valueToString(v)}`, s)
    },
    isSeq: (v: Any) => typeof v === 'string' || Array.isArray(v),
  }
}

// ---------------------------------------------------------------------------
// Trampoline loop — tick, runSyncTrampoline, runAsyncTrampoline
// ---------------------------------------------------------------------------

/**
 * Process one step of the trampoline. Returns the next step, or a
 * Promise<Step> when an async operation (e.g., native JS function) is
 * encountered.
 *
 * - `Value` with empty `k`: the program is done (terminal state).
 * - `Value` with non-empty `k`: pop the top frame and apply it.
 * - `Eval`: evaluate an AST node via `stepNode` (always synchronous).
 * - `Apply`: apply a frame to a value (may return Promise<Step>).
 * - `Perform`: effect dispatch — local (try/with) first, then host handlers.
 *
 * When `handlers` and `signal` are provided (from `run()`), host handlers are
 * available as a fallback for effects not matched by any local `try/with`.
 */
export function tick(step: Step, handlers?: Handlers, signal?: AbortSignal): Step | Promise<Step> {
  try {
    switch (step.type) {
      case 'Value': {
        if (step.k.length === 0) {
          return step // Terminal state — program is complete
        }
        const [frame, ...rest] = step.k
        return applyFrame(frame!, step.value, rest)
      }
      case 'Eval':
        return stepNode(step.node, step.env, step.k)
      case 'Apply':
        return applyFrame(step.frame, step.value, step.k)
      case 'Perform':
        return dispatchPerform(step.effect, step.args, step.k, undefined, handlers, signal)
    }
  }
  catch (error) {
    // Search the continuation stack for a TryCatchFrame to handle the error.
    // This handles both explicit throw() and runtime errors (e.g., type errors).
    return unwindToTryCatch(error, step.k)
  }
}

/**
 * Run the trampoline synchronously to completion.
 * Throws if any step produces a Promise (i.e., an async operation was
 * encountered in a synchronous context).
 */
export function runSyncTrampoline(initial: Step): Any {
  let step: Step | Promise<Step> = initial
  for (;;) {
    if (step instanceof Promise) {
      throw new LitsError('Unexpected async operation in synchronous context. Use async.run() for async operations.', undefined)
    }
    if (step.type === 'Value' && step.k.length === 0) {
      return step.value
    }
    step = tick(step)
  }
}

/**
 * Run the trampoline asynchronously to completion.
 * Awaits any Promise<Step> that surfaces from async operations.
 */
export async function runAsyncTrampoline(initial: Step): Promise<Any> {
  let step: Step | Promise<Step> = initial
  for (;;) {
    if (step instanceof Promise) {
      step = await step
    }
    if (step.type === 'Value' && step.k.length === 0) {
      return step.value
    }
    step = tick(step)
  }
}

// ---------------------------------------------------------------------------
// Public entry points — evaluate an AST or a single node
// ---------------------------------------------------------------------------

/**
 * Build the initial Step for evaluating an AST (sequence of top-level nodes).
 */
function buildInitialStep(nodes: AstNode[], env: ContextStack): Step {
  if (nodes.length === 0) {
    return { type: 'Value', value: null, k: [] }
  }
  if (nodes.length === 1) {
    return { type: 'Eval', node: nodes[0]!, env, k: [] }
  }
  const sequenceFrame: SequenceFrame = {
    type: 'Sequence',
    nodes,
    index: 1,
    env,
  }
  return { type: 'Eval', node: nodes[0]!, env, k: [sequenceFrame] }
}

/**
 * Evaluate an AST using the trampoline.
 * Returns the final value synchronously, or a Promise if async operations
 * are involved (e.g., native JS functions returning Promises).
 */
export function evaluate(ast: Ast, contextStack: ContextStack): MaybePromise<Any> {
  const initial = buildInitialStep(ast.body, contextStack)
  // Try synchronous first; if a Promise surfaces, switch to async
  try {
    return runSyncTrampoline(initial)
  }
  catch (error) {
    if (error instanceof LitsError && error.message.includes('Unexpected async operation')) {
      // An async operation was encountered — re-run with the async trampoline.
      // We must rebuild the initial step since the sync attempt may have
      // partially mutated frames.
      const freshInitial = buildInitialStep(ast.body, contextStack)
      return runAsyncTrampoline(freshInitial)
    }
    throw error
  }
}

/**
 * Evaluate an AST using the async trampoline directly.
 * Use this when the caller knows that async operations may be involved
 * (e.g., from Lits.async.run) to avoid the sync-first-then-retry pattern
 * which can cause side effects to be executed twice.
 */
export function evaluateAsync(ast: Ast, contextStack: ContextStack): Promise<Any> {
  const initial = buildInitialStep(ast.body, contextStack)
  return runAsyncTrampoline(initial)
}

/**
 * Evaluate a single AST node using the trampoline.
 * Used as the `evaluateNode` callback passed to `getUndefinedSymbols`
 * and other utilities.
 */
export function evaluateNode(node: AstNode, contextStack: ContextStack): MaybePromise<Any> {
  const initial: Step = { type: 'Eval', node, env: contextStack, k: [] }
  try {
    return runSyncTrampoline(initial)
  }
  catch (error) {
    if (error instanceof LitsError && error.message.includes('Unexpected async operation')) {
      const freshInitial: Step = { type: 'Eval', node, env: contextStack, k: [] }
      return runAsyncTrampoline(freshInitial)
    }
    throw error
  }
}

// ---------------------------------------------------------------------------
// Effect trampoline — async loop with host handler support
// ---------------------------------------------------------------------------

/**
 * Evaluate an AST with full effect handler support.
 *
 * Uses the async trampoline loop, passing `handlers` and `signal` to `tick`
 * so that `dispatchPerform` can fall back to host handlers when no local
 * `try/with` matches.
 *
 * Always resolves — never rejects. All errors are captured in
 * `RunResult.error`. Suspension is signaled via `RunResult.suspended`.
 *
 * The `AbortController` is created internally per `run()` call. The signal
 * is passed to every host handler. Used for `race()` cancellation (Phase 6)
 * and host-side timeouts.
 */
export async function evaluateWithEffects(
  ast: Ast,
  contextStack: ContextStack,
  handlers?: Handlers,
): Promise<RunResult> {
  const abortController = new AbortController()
  const signal = abortController.signal
  const initial = buildInitialStep(ast.body, contextStack)

  try {
    let step: Step | Promise<Step> = initial
    for (;;) {
      if (step instanceof Promise) {
        step = await step
      }
      if (step.type === 'Value' && step.k.length === 0) {
        return { type: 'completed', value: step.value }
      }
      step = tick(step, handlers, signal)
    }
  }
  catch (error) {
    if (isSuspensionSignal(error)) {
      return { type: 'suspended', continuation: error.k, meta: error.meta }
    }
    if (error instanceof LitsError) {
      return { type: 'error', error }
    }
    return { type: 'error', error: new LitsError(`${error}`, undefined) }
  }
}
