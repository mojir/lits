import { specialExpressions } from '../builtin'
import { evaluateBindingNodeValues } from '../builtin/bindingNode'
import { allNormalExpressions } from '../builtin/normalExpressions'
import { LitsError, RecurSignal } from '../errors'
import { arityAcceptsMin, assertNumberOfParams } from '../utils/arity'
import type { Any, Arr } from '../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  LitsFunctionType,
  ModuleFunction,
  NativeJsFunction,
  NormalBuiltinFunction,
  PartialFunction,
  SomePredFunction,
  SpecialBuiltinFunction,
  UserDefinedFunction,
} from '../parser/types'
import { bindingTargetTypes } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { asNonUndefined, isUnknownRecord } from '../typeGuards'
import { asAny, asFunctionLike } from '../typeGuards/lits'
import { toAny } from '../utils'
import { valueToString } from '../utils/debug/debugTools'
import type { MaybePromise } from '../utils/maybePromise'
import { chain, mapSequential, reduceSequential } from '../utils/maybePromise'
import type { ContextStack } from './ContextStack'
import type { Context, EvaluateNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<LitsFunctionType, (
  fn: any,
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  helpers: { evaluateNode: EvaluateNode, executeFunction: ExecuteFunction },
) => MaybePromise<Any>>

export const functionExecutors: FunctionExecutors = {
  NativeJsFunction: (fn: NativeJsFunction, params, sourceCodeInfo) => {
    try {
      const result = fn.nativeFn.fn(...params)
      // If the native function returns a Promise, await it transparently
      if (result instanceof Promise) {
        return result.then(
          resolved => toAny(resolved),
          (error: unknown) => {
            const message
              = typeof error === 'string'
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
      const message
        = typeof error === 'string'
          ? error
          : isUnknownRecord(error) && typeof error.message === 'string'
            ? error.message
            : '<no message>'
      throw new LitsError(`Native function threw: "${message}"`, sourceCodeInfo)
    }
  },
  UserDefined: (fn: UserDefinedFunction, params, sourceCodeInfo, contextStack, { evaluateNode }) => {
    function setupAndExecute(currentParams: Arr): MaybePromise<Any> {
      if (!arityAcceptsMin(fn.arity, currentParams.length)) {
        throw new LitsError(`Expected ${fn.arity} arguments, got ${currentParams.length}.`, sourceCodeInfo)
      }
      const evaluatedFunction = fn.evaluatedfunction
      const args = evaluatedFunction[0]
      const nbrOfNonRestArgs: number = args.filter(arg => arg[0] !== bindingTargetTypes.rest).length

      const newContextStack = contextStack.create(fn.evaluatedfunction[2])
      const newContext: Context = { self: { value: fn } }

      const rest: Arr = []

      // Process non-rest params sequentially since binding evaluation may be async
      let paramSetup: MaybePromise<void> = undefined as unknown as void
      for (let i = 0; i < currentParams.length; i += 1) {
        if (i < nbrOfNonRestArgs) {
          const paramIndex = i
          paramSetup = chain(paramSetup, () => {
            const param = toAny(currentParams[paramIndex])
            return chain(evaluateBindingNodeValues(args[paramIndex]!, param, node =>
              evaluateNode(node, newContextStack.create(newContext))), (valueRecord) => {
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

      // Handle default values for optional params — chain sequentially since they may be async
      let defaultSetup: MaybePromise<void> = undefined as unknown as void
      for (let i = currentParams.length; i < nbrOfNonRestArgs; i++) {
        const argIndex = i
        defaultSetup = chain(defaultSetup, () => {
          const arg = args[argIndex]!
          return chain(evaluateNode(arg[1][1]!, contextStack.create(newContext)), (defaultValue) => {
            return chain(evaluateBindingNodeValues(arg, defaultValue, node =>
              evaluateNode(node, contextStack.create(newContext))), (valueRecord) => {
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
              evaluateNode(node, contextStack.create(newContext))), (valueRecord) => {
              Object.entries(valueRecord).forEach(([key, value]) => {
                newContext[key] = { value }
              })
            })
          : undefined as unknown as void

        return chain(restSetup, () => {
          // Evaluate body nodes sequentially
          const newContextStack2 = newContextStack.create(newContext)
          const bodyResult = reduceSequential(
            evaluatedFunction[1],
            (_acc, node) => evaluateNode(node, newContextStack2),
            null as Any,
          )

          // Handle RecurSignal for async body results
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

    // Sync recur loop: use for(;;) to avoid stack overflow for sync tail recursion
    for (;;) {
      try {
        const result = setupAndExecute(params)
        // If result is async, the RecurSignal handling is inside the Promise chain
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
  },
  Partial: (fn: PartialFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const actualParams = [...fn.params]
    if (params.length !== fn.placeholders.length) {
      throw new LitsError(`(partial) expects ${fn.placeholders.length} arguments, got ${params.length}.`, sourceCodeInfo)
    }
    const paramsCopy = [...params]
    for (const placeholderIndex of fn.placeholders) {
      actualParams.splice(placeholderIndex, 0, paramsCopy.shift())
    }
    return executeFunction(fn.function, actualParams, contextStack, sourceCodeInfo)
  },
  Comp: (fn: CompFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const { params: f } = fn
    if (f.length === 0) {
      if (params.length !== 1)
        throw new LitsError(`(comp) expects one argument, got ${valueToString(params.length)}.`, sourceCodeInfo)

      return asAny(params[0], sourceCodeInfo)
    }
    // reduceRight with MaybePromise: each step wraps result in array, passes to next function
    let result: MaybePromise<Arr> = params
    for (let i = f.length - 1; i >= 0; i--) {
      const fun = f[i]!
      result = chain(result, (currentParams) => {
        return chain(
          executeFunction(asFunctionLike(fun, sourceCodeInfo), currentParams, contextStack, sourceCodeInfo),
          r => [r],
        )
      })
    }
    return chain(result, finalArr => asAny(finalArr[0], sourceCodeInfo))
  },
  Constantly: (fn: ConstantlyFunction) => {
    return fn.value
  },
  Juxt: (fn: JuxtFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return mapSequential(fn.params, fun =>
      executeFunction(asFunctionLike(fun, sourceCodeInfo), params, contextStack, sourceCodeInfo))
  },
  Complement: (fn: ComplementFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return chain(
      executeFunction(fn.function, params, contextStack, sourceCodeInfo),
      result => !result,
    )
  },
  EveryPred: (fn: EveryPredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    // Flatten to sequential checks: for each predicate, for each param
    const checks: Array<() => MaybePromise<Any>> = []
    for (const f of fn.params) {
      for (const param of params) {
        checks.push(() => executeFunction(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo))
      }
    }
    return reduceSequential(
      checks,
      (acc, check) => {
        if (!acc)
          return false
        return chain(check(), result => !!result)
      },
      true as Any,
    )
  },
  SomePred: (fn: SomePredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const checks: Array<() => MaybePromise<Any>> = []
    for (const f of fn.params) {
      for (const param of params) {
        checks.push(() => executeFunction(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo))
      }
    }
    return reduceSequential(
      checks,
      (acc, check) => {
        if (acc)
          return true
        return chain(check(), result => !!result)
      },
      false as Any,
    )
  },
  Fnull: (fn: FNullFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const fnulledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
    return executeFunction(asFunctionLike(fn.function, sourceCodeInfo), fnulledParams, contextStack, sourceCodeInfo)
  },
  Builtin: (fn: NormalBuiltinFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const normalExpression = asNonUndefined(allNormalExpressions[fn.normalBuiltinSymbolType], sourceCodeInfo)
    return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction })
  },
  SpecialBuiltin: (fn: SpecialBuiltinFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const specialExpression = asNonUndefined(specialExpressions[fn.specialBuiltinSymbolType], sourceCodeInfo)
    if (specialExpression.evaluateAsNormalExpression) {
      return specialExpression.evaluateAsNormalExpression(params, sourceCodeInfo, contextStack, { executeFunction })
    }
    else {
      throw new LitsError(`Special builtin function ${fn.specialBuiltinSymbolType} is not supported as normal expression.`, sourceCodeInfo)
    }
  },
  Module: (fn: ModuleFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const module = contextStack.getModule(fn.moduleName)
    if (!module) {
      throw new LitsError(`Module '${fn.moduleName}' not found.`, sourceCodeInfo)
    }
    const expression = module.functions[fn.functionName]
    if (!expression) {
      throw new LitsError(`Function '${fn.functionName}' not found in module '${fn.moduleName}'.`, sourceCodeInfo)
    }
    assertNumberOfParams(expression.arity, params.length, sourceCodeInfo)
    return expression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction })
  },
}
