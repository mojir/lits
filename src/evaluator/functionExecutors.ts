import { normalExpressions } from '../builtin/normalExpressions'
import { LitsError, RecurSignal } from '../errors'
import type { Any, Arr } from '../interface'
import type {
  BuiltinFunction,
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EvaluatedFunction,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  LitsFunctionType,
  NativeJsFunction,
  PartialFunction,
  SomePredFunction,
  UserDefinedFunction,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { toAny } from '../utils'
import { valueToString } from '../utils/debug/debugTools'
import { asString } from '../typeGuards/string'
import { asNonUndefined, isUnknownRecord } from '../typeGuards'
import { asAny } from '../typeGuards/lits'
import type { Context, EvaluateAstNode, ExecuteFunction } from './interface'
import type { ContextStack } from './ContextStack'

type FunctionExecutors = Record<
  LitsFunctionType,
  (
    fn: any,
    params: Arr,
    sourceCodeInfo: SourceCodeInfo | undefined,
    contextStack: ContextStack,
    helpers: { evaluateAstNode: EvaluateAstNode, executeFunction: ExecuteFunction },
  ) => Any
>

function checkParams(
  fn: EvaluatedFunction,
  nbrOfParams: number,
  sourceCodeInfo?: SourceCodeInfo,
) {
  const arity = fn.arity
  if (typeof arity === 'number') {
    if (arity === nbrOfParams) {
      return
    }
  }
  else {
    if (arity.min <= nbrOfParams) {
      return
    }
  }
  throw new LitsError(`Unexpected number of arguments, got ${valueToString(nbrOfParams)}.`, sourceCodeInfo)
}

export const functionExecutors: FunctionExecutors = {
  NativeJsFunction: (fn: NativeJsFunction, params, sourceCodeInfo) => {
    try {
      // eslint-disable-next-line ts/no-unsafe-assignment
      const clonedParams = JSON.parse(JSON.stringify(params))
      // eslint-disable-next-line ts/no-unsafe-argument
      return toAny(fn.nativeFn.fn(...clonedParams))
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
  UserDefined: (fn: UserDefinedFunction, params, sourceCodeInfo, contextStack, { evaluateAstNode }) => {
    for (;;) {
      checkParams(fn.function, params.length, sourceCodeInfo)
      const overloadFunction = fn.function
      const args = overloadFunction.arguments
      const nbrOfMandatoryArgs: number = args.mandatoryArguments.length

      const newContext: Context = { ...overloadFunction.context }

      const length = Math.max(params.length, args.mandatoryArguments.length)
      const rest: Arr = []
      for (let i = 0; i < length; i += 1) {
        if (i < nbrOfMandatoryArgs) {
          const param = toAny(params[i])
          const key = asString(args.mandatoryArguments[i], sourceCodeInfo)
          newContext[key] = { value: param }
        }
        else {
          rest.push(toAny(params[i]))
        }
      }

      if (args.restArgument)
        newContext[args.restArgument] = { value: rest }

      try {
        let result: Any = null
        const newContextStack = contextStack.create(newContext)
        for (const node of overloadFunction.body) {
          result = evaluateAstNode(node, newContextStack)
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
    return executeFunction(fn.function, [...fn.params, ...params], contextStack, sourceCodeInfo)
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
        return [executeFunction(toAny(fun), result, contextStack, sourceCodeInfo)]
      }, params)[0],
      sourceCodeInfo,
    )
  },
  Constantly: (fn: ConstantlyFunction) => {
    return fn.value
  },
  Juxt: (fn: JuxtFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return fn.params.map(fun => executeFunction(toAny(fun), params, contextStack, sourceCodeInfo))
  },
  Complement: (fn: ComplementFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return !executeFunction(fn.function, params, contextStack, sourceCodeInfo)
  },
  EveryPred: (fn: EveryPredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    for (const f of fn.params) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack, sourceCodeInfo)
        if (!result)
          return false
      }
    }
    return true
  },
  SomePred: (fn: SomePredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    for (const f of fn.params) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack, sourceCodeInfo)
        if (result)
          return true
      }
    }
    return false
  },
  Fnull: (fn: FNullFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const fnulledParams = params.map((param, index) => (param === null ? toAny(fn.params[index]) : param))
    return executeFunction(toAny(fn.function), fnulledParams, contextStack, sourceCodeInfo)
  },
  Builtin: (fn: BuiltinFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const normalExpression = asNonUndefined(normalExpressions[fn.n], sourceCodeInfo)
    return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction })
  },
}
