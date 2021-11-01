import { normalExpressions } from '../builtin/normalExpressions'
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
import { TokenMeta } from '../tokenizer/interface'
import { asNotUndefined, toAny } from '../utils'
import { any } from '../utils/assertion'
import { Context, ContextStack, EvaluateAstNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<
  LitsFunctionType,
  (
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    fn: any,
    params: Arr,
    meta: TokenMeta,
    contextStack: ContextStack,
    helpers: { evaluateAstNode: EvaluateAstNode; executeFunction: ExecuteFunction },
  ) => Any
>

function findOverloadFunction(
  overloads: EvaluatedFunctionOverload[],
  nbrOfParams: number,
  meta: TokenMeta,
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
    throw new LitsError(`Unexpected number of arguments, got ${nbrOfParams}`, meta)
  }
  return overloadFunction
}

export const functionExecutors: FunctionExecutors = {
  'user-defined': (fn: UserDefinedFunction, params, meta, contextStack, { evaluateAstNode }) => {
    for (;;) {
      const overloadFunction = findOverloadFunction(fn.overloads, params.length, meta)
      const args = overloadFunction.arguments
      const nbrOfMandatoryArgs: number = args.mandatoryArguments.length

      const newContext: Context = { ...overloadFunction.functionContext }

      const length = Math.max(params.length, args.mandatoryArguments.length)
      const rest: Arr = []
      for (let i = 0; i < length; i += 1) {
        if (i < nbrOfMandatoryArgs) {
          const param = toAny(params[i])
          const key = asNotUndefined(args.mandatoryArguments[i], meta)
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
  partial: (fn: PartialFunction, params, meta, contextStack, { executeFunction }) => {
    return executeFunction(fn.fn, [...fn.params, ...params], meta, contextStack)
  },
  comp: (fn: CompFunction, params, meta, contextStack, { executeFunction }) => {
    const { fns } = fn
    if (fns.length === 0) {
      if (params.length !== 1) {
        throw new LitsError(`(comp) expects one argument, got ${params.length}`, meta)
      }
      return any.as(params[0], meta)
    }
    return any.as(
      fns.reduceRight((result: Arr, fn) => {
        return [executeFunction(toAny(fn), result, meta, contextStack)]
      }, params)[0],
      meta,
    )
  },
  constantly: (fn: ConstantlyFunction) => {
    return fn.value
  },
  juxt: (fn: JuxtFunction, params, meta, contextStack, { executeFunction }) => {
    return fn.fns.map(fn => executeFunction(toAny(fn), params, meta, contextStack))
  },
  complement: (fn: ComplementFunction, params, meta, contextStack, { executeFunction }) => {
    return !executeFunction(fn.fn, params, meta, contextStack)
  },
  'every-pred': (fn: EveryPredFunction, params, meta, contextStack, { executeFunction }) => {
    for (const f of fn.fns) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], meta, contextStack)
        if (!result) {
          return false
        }
      }
    }
    return true
  },
  'some-pred': (fn: SomePredFunction, params, meta, contextStack, { executeFunction }) => {
    for (const f of fn.fns) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], meta, contextStack)
        if (result) {
          return true
        }
      }
    }
    return false
  },
  fnil: (fn: FNilFunction, params, meta, contextStack, { executeFunction }) => {
    const fniledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
    return executeFunction(toAny(fn.fn), fniledParams, meta, contextStack)
  },
  builtin: (fn: BuiltinFunction, params, meta, contextStack, { executeFunction }) => {
    const normalExpression = asNotUndefined(normalExpressions[fn.name], meta)
    return normalExpression.evaluate(params, meta, contextStack, { executeFunction })
  },
}
