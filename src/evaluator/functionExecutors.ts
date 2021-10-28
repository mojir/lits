import { normalExpressions } from '../builtin/normalExpressions'
import { RecurSignal } from '../errors'
import { Any, Arr } from '../interface'
import {
  BuiltinFunction,
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNilFunction,
  JuxtFunction,
  LispishFunctionType,
  PartialFunction,
  SomePredFunction,
  UserDefinedFunction,
} from '../parser/interface'
import { asAny, asNotUndefined, asString, toAny } from '../utils'
import { Context, ContextStack, EvaluateAstNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<
  LispishFunctionType,
  (
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    fn: any,
    params: Arr,
    contextStack: ContextStack,
    helpers: { evaluateAstNode: EvaluateAstNode; executeFunction: ExecuteFunction },
  ) => Any
>

export const functionExecutors: FunctionExecutors = {
  'user-defined': (fn: UserDefinedFunction, params, contextStack, { evaluateAstNode }) => {
    const args = fn.arguments
    const nbrOfMandatoryArgs: number = args.mandatoryArguments.length
    const nbrOfOptionalArgs: number = args.optionalArguments.length
    const maxNbrOfParameters: null | number = args.restArgument ? null : nbrOfMandatoryArgs + nbrOfOptionalArgs

    for (;;) {
      const newContext: Context = { ...fn.functionContext }
      if (params.length < args.mandatoryArguments.length) {
        throw Error(
          `Function ${fn.name ?? `(fn)`} requires at least ${args.mandatoryArguments.length} arguments. Got ${
            params.length
          }`,
        )
      }

      if (maxNbrOfParameters !== null && params.length > maxNbrOfParameters) {
        throw Error(
          `Function "${fn.name ?? `λ`}" requires at most ${maxNbrOfParameters} arguments. Got ${params.length}`,
        )
      }

      const length = Math.max(params.length, args.mandatoryArguments.length + args.optionalArguments.length)
      const rest: Arr = []
      for (let i = 0; i < length; i += 1) {
        if (i < nbrOfMandatoryArgs) {
          const param = toAny(params[i])
          const key = asString(args.mandatoryArguments[i])
          newContext[key] = { value: param }
        } else if (i < nbrOfMandatoryArgs + nbrOfOptionalArgs) {
          const arg = asNotUndefined(args.optionalArguments[i - nbrOfMandatoryArgs])
          const param = i < params.length ? toAny(params[i]) : arg.defaultValue ?? null
          const key = arg.name
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
        for (const node of fn.body) {
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
