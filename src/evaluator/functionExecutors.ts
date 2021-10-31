import { normalExpressions } from '../builtin/normalExpressions'
import { RecurSignal } from '../errors'
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
import { asAny, asNotUndefined, asString, toAny } from '../utils'
import { Context, ContextStack, EvaluateAstNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<
  LitsFunctionType,
  (
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    fn: any,
    params: Arr,
    contextStack: ContextStack,
    helpers: { evaluateAstNode: EvaluateAstNode; executeFunction: ExecuteFunction },
  ) => Any
>

function findOverloadFunction(overloads: EvaluatedFunctionOverload[], nbrOfParams: number): EvaluatedFunctionOverload {
  const overloadFunction = overloads.find(overload => {
    const arity = overload.arity
    if (typeof arity === `number`) {
      return arity === nbrOfParams
    } else {
      return arity.min <= nbrOfParams
    }
  })
  if (!overloadFunction) {
    throw Error(`Unexpected number of arguments, got ${nbrOfParams}`)
  }
  return overloadFunction
}

export const functionExecutors: FunctionExecutors = {
  'user-defined': (fn: UserDefinedFunction, params, contextStack, { evaluateAstNode }) => {
    for (;;) {
      const overloadFunction = findOverloadFunction(fn.overloads, params.length)
      const args = overloadFunction.arguments
      const nbrOfMandatoryArgs: number = args.mandatoryArguments.length

      const newContext: Context = { ...overloadFunction.functionContext }

      const length = Math.max(params.length, args.mandatoryArguments.length)
      const rest: Arr = []
      for (let i = 0; i < length; i += 1) {
        if (i < nbrOfMandatoryArgs) {
          const param = toAny(params[i])
          const key = asString(args.mandatoryArguments[i])
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
  partial: (fn: PartialFunction, params, contextStack, { executeFunction }) => {
    return executeFunction(fn.fn, [...fn.params, ...params], contextStack)
  },
  comp: (fn: CompFunction, params, contextStack, { executeFunction }) => {
    const { fns } = fn
    if (fns.length === 0) {
      if (params.length !== 1) {
        throw Error(`(comp) expects one argument, got ${params.length}`)
      }
      return asAny(params[0])
    }
    return asAny(
      fns.reduceRight((result: Arr, fn) => {
        return [executeFunction(toAny(fn), result, contextStack)]
      }, params)[0],
    )
  },
  constantly: (fn: ConstantlyFunction) => {
    return fn.value
  },
  juxt: (fn: JuxtFunction, params, contextStack, { executeFunction }) => {
    return fn.fns.map(fn => executeFunction(toAny(fn), params, contextStack))
  },
  complement: (fn: ComplementFunction, params, contextStack, { executeFunction }) => {
    return !executeFunction(fn.fn, params, contextStack)
  },
  'every-pred': (fn: EveryPredFunction, params, contextStack, { executeFunction }) => {
    for (const f of fn.fns) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack)
        if (!result) {
          return false
        }
      }
    }
    return true
  },
  'some-pred': (fn: SomePredFunction, params, contextStack, { executeFunction }) => {
    for (const f of fn.fns) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack)
        if (result) {
          return true
        }
      }
    }
    return false
  },
  fnil: (fn: FNilFunction, params, contextStack, { executeFunction }) => {
    const fniledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
    return executeFunction(toAny(fn.fn), fniledParams, contextStack)
  },
  builtin: (fn: BuiltinFunction, params, contextStack, { executeFunction }) => {
    const normalExpression = asNotUndefined(normalExpressions[fn.name])
    return normalExpression.evaluate(params, contextStack, { executeFunction })
  },
}
