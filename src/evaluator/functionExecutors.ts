import { specialExpressions } from '../builtin'
import { evalueateBindingNodeValues } from '../builtin/bindingNode'
import { allNormalExpressions } from '../builtin/normalExpressions'
import { LitsError, RecurSignal } from '../errors'
import type { Any, Arr } from '../interface'
// Import from index to ensure namespaces are registered
import { getNamespace } from '../namespaces'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  LitsFunctionType,
  NamespaceFunction,
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
import { arityAccepts, arityAcceptsMin } from '../utils/arity'
import { valueToString } from '../utils/debug/debugTools'
import type { ContextStack } from './ContextStack'
import type { Context, EvaluateNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<LitsFunctionType, (
  fn: any,
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  helpers: { evaluateNode: EvaluateNode, executeFunction: ExecuteFunction },
) => Any>

export const functionExecutors: FunctionExecutors = {
  NativeJsFunction: (fn: NativeJsFunction, params, sourceCodeInfo) => {
    try {
      return toAny(fn.nativeFn.fn(...params))
    }
    catch (error) {
      const message
        = typeof error === 'string'
          ? error
          : isUnknownRecord(error) && typeof error.message === 'string'
            ? error.message
            : '<no message>'
      throw new LitsError(`Native function throwed: "${message}"`, sourceCodeInfo)
    }
  },
  UserDefined: (fn: UserDefinedFunction, params, sourceCodeInfo, contextStack, { evaluateNode }) => {
    for (;;) {
      if (!arityAcceptsMin(fn.arity, params.length)) {
        throw new LitsError(`Expected ${fn.arity} arguments, got ${params.length}.`, sourceCodeInfo)
      }
      // checkParams(fn.evaluatedfunction, params.length, sourceCodeInfo)
      const evaluatedFunction = fn.evaluatedfunction
      const args = evaluatedFunction[0]
      const nbrOfNonRestArgs: number = args.filter(arg => arg[0] !== bindingTargetTypes.rest).length

      const newContextStack = contextStack.create(fn.evaluatedfunction[2])
      const newContext: Context = { self: { value: fn } }

      const rest: Arr = []
      for (let i = 0; i < params.length; i += 1) {
        if (i < nbrOfNonRestArgs) {
          const param = toAny(params[i])
          const valueRecord = evalueateBindingNodeValues(args[i]!, param, node =>
            evaluateNode(node, newContextStack.create(newContext)))
          Object.entries(valueRecord).forEach(([key, value]) => {
            newContext[key] = { value }
          })
        }
        else {
          rest.push(toAny(params[i]))
        }
      }

      for (let i = params.length; i < nbrOfNonRestArgs; i++) {
        const arg = args[i]!
        const defaultValue = evaluateNode(arg[1][1]!, contextStack.create(newContext))
        const valueRecord = evalueateBindingNodeValues(arg, defaultValue, node =>
          evaluateNode(node, contextStack.create(newContext)))
        Object.entries(valueRecord).forEach(([key, value]) => {
          newContext[key] = { value }
        })
      }

      const restArgument = args.find(arg => arg[0] === bindingTargetTypes.rest)
      if (restArgument !== undefined) {
        const valueRecord = evalueateBindingNodeValues(restArgument, rest, node => evaluateNode(node, contextStack.create(newContext)))
        Object.entries(valueRecord).forEach(([key, value]) => {
          newContext[key] = { value }
        })
      }

      try {
        let result: Any = null
        const newContextStack2 = newContextStack.create(newContext)
        for (const node of evaluatedFunction[1]) {
          result = evaluateNode(node, newContextStack2)
        }

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
    return asAny(
      f.reduceRight((result: Arr, fun) => {
        return [executeFunction(asFunctionLike(fun, sourceCodeInfo), result, contextStack, sourceCodeInfo)]
      }, params)[0],
      sourceCodeInfo,
    )
  },
  Constantly: (fn: ConstantlyFunction) => {
    return fn.value
  },
  Juxt: (fn: JuxtFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return fn.params.map(fun => executeFunction(asFunctionLike(fun, sourceCodeInfo), params, contextStack, sourceCodeInfo))
  },
  Complement: (fn: ComplementFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return !executeFunction(fn.function, params, contextStack, sourceCodeInfo)
  },
  EveryPred: (fn: EveryPredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    for (const f of fn.params) {
      for (const param of params) {
        const result = executeFunction(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo)
        if (!result)
          return false
      }
    }
    return true
  },
  SomePred: (fn: SomePredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    for (const f of fn.params) {
      for (const param of params) {
        const result = executeFunction(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo)
        if (result)
          return true
      }
    }
    return false
  },
  Fnull: (fn: FNullFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const fnulledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
    return executeFunction(asFunctionLike(fn.function, sourceCodeInfo), fnulledParams, contextStack, sourceCodeInfo)
  },
  Builtin: (fn: NormalBuiltinFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const normalExpression = asNonUndefined(allNormalExpressions[fn.normalBuitinSymbolType], sourceCodeInfo)
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
  Namespace: (fn: NamespaceFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const namespace = getNamespace(fn.namespaceName)
    if (!namespace) {
      throw new LitsError(`Namespace '${fn.namespaceName}' not found.`, sourceCodeInfo)
    }
    const expression = namespace.functions[fn.functionName]
    if (!expression) {
      throw new LitsError(`Function '${fn.functionName}' not found in namespace '${fn.namespaceName}'.`, sourceCodeInfo)
    }
    if (!arityAccepts(expression.arity, params.length)) {
      throw new LitsError(`Function '${fn.functionName}' expects ${expression.arity.min}${expression.arity.max === expression.arity.min ? '' : `-${expression.arity.max}`} arguments, got ${params.length}.`, sourceCodeInfo)
    }
    return expression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction })
  },
}
