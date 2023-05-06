import { normalExpressions } from '../builtin/normalExpressions'
import { ContextStack } from '../ContextStack'
import { Context } from '../ContextStack/interface'
import { LitsError, RecurSignal } from '../errors'
import { Any, Arr } from '../interface'
import {
  BuiltinFunction,
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EvaluatedFunctionOverload,
  EveryPredFunction,
  FNilFunction,
  JuxtFunction,
  LitsFunctionType,
  PartialFunction,
  SomePredFunction,
  UserDefinedFunction,
} from '../parser/interface'
import { DebugInfo } from '../tokenizer/interface'
import { toAny } from '../utils'
import { any, asValue, string } from '../utils/assertion'
import { valueToString } from '../utils/helpers'
import { EvaluateAstNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<
  LitsFunctionType,
  (
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    fn: any,
    params: Arr,
    debugInfo: DebugInfo | undefined,
    contextStack: ContextStack,
    helpers: { evaluateAstNode: EvaluateAstNode; executeFunction: ExecuteFunction },
  ) => Any
>

function findOverloadFunction(
  overloads: EvaluatedFunctionOverload[],
  nbrOfParams: number,
  debugInfo?: DebugInfo,
): EvaluatedFunctionOverload {
  const overloadFunction = overloads.find(overload => {
    const arity = overload.arity
    if (typeof arity === `number`) {
      return arity === nbrOfParams
    } else {
      return arity.min <= nbrOfParams
    }
  })
  if (!overloadFunction) {
    throw new LitsError(`Unexpected number of arguments, got ${valueToString(nbrOfParams)}.`, debugInfo)
  }
  return overloadFunction
}

export const functionExecutors: FunctionExecutors = {
  'user-defined': (fn: UserDefinedFunction, params, debugInfo, contextStack, { evaluateAstNode }) => {
    for (;;) {
      const overloadFunction = findOverloadFunction(fn.overloads, params.length, debugInfo)
      const args = overloadFunction.arguments
      const nbrOfMandatoryArgs: number = args.mandatoryArguments.length

      const newContext: Context = { ...overloadFunction.functionContext }

      const length = Math.max(params.length, args.mandatoryArguments.length)
      const rest: Arr = []
      for (let i = 0; i < length; i += 1) {
        if (i < nbrOfMandatoryArgs) {
          const param = toAny(params[i])
          const key = string.as(args.mandatoryArguments[i], debugInfo)
          newContext[key] = { value: param }
        } else {
          rest.push(toAny(params[i]))
        }
      }

      if (args.restArgument) {
        newContext[args.restArgument] = { value: rest }
      }

      try {
        let result: Any = null
        for (const node of overloadFunction.body) {
          result = evaluateAstNode(node, contextStack.withContext(newContext))
        }
        return result
      } catch (error) {
        if (error instanceof RecurSignal) {
          params = error.params
          continue
        }
        throw error
      }
    }
  },
  partial: (fn: PartialFunction, params, debugInfo, contextStack, { executeFunction }) => {
    return executeFunction(fn.fn, [...fn.params, ...params], contextStack, debugInfo)
  },
  comp: (fn: CompFunction, params, debugInfo, contextStack, { executeFunction }) => {
    const { fns } = fn
    if (fns.length === 0) {
      if (params.length !== 1) {
        throw new LitsError(`(comp) expects one argument, got ${valueToString(params.length)}.`, debugInfo)
      }
      return any.as(params[0], debugInfo)
    }
    return any.as(
      fns.reduceRight((result: Arr, fn) => {
        return [executeFunction(toAny(fn), result, contextStack, debugInfo)]
      }, params)[0],
      debugInfo,
    )
  },
  constantly: (fn: ConstantlyFunction) => {
    return fn.value
  },
  juxt: (fn: JuxtFunction, params, debugInfo, contextStack, { executeFunction }) => {
    return fn.fns.map(fn => executeFunction(toAny(fn), params, contextStack, debugInfo))
  },
  complement: (fn: ComplementFunction, params, debugInfo, contextStack, { executeFunction }) => {
    return !executeFunction(fn.fn, params, contextStack, debugInfo)
  },
  'every-pred': (fn: EveryPredFunction, params, debugInfo, contextStack, { executeFunction }) => {
    for (const f of fn.fns) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack, debugInfo)
        if (!result) {
          return false
        }
      }
    }
    return true
  },
  'some-pred': (fn: SomePredFunction, params, debugInfo, contextStack, { executeFunction }) => {
    for (const f of fn.fns) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack, debugInfo)
        if (result) {
          return true
        }
      }
    }
    return false
  },
  fnil: (fn: FNilFunction, params, debugInfo, contextStack, { executeFunction }) => {
    const fniledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
    return executeFunction(toAny(fn.fn), fniledParams, contextStack, debugInfo)
  },
  builtin: (fn: BuiltinFunction, params, debugInfo, contextStack, { executeFunction }) => {
    const normalExpression = asValue(normalExpressions[fn.name], debugInfo)
    normalExpression.validateArity(params.length, debugInfo)
    return normalExpression.evaluate(params, debugInfo, contextStack, { executeFunction })
  },
}
