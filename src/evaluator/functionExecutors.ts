import { normalExpressions } from '../builtin/normalExpressions'
import { LitsError, RecurSignal } from '../errors'
import type { Any, Arr } from '../interface'
import type {
  BuiltinFunction,
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EvaluatedFunctionOverload,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  LitsFunctionType,
  NativeJsFunction,
  PartialFunction,
  SomePredFunction,
  UserDefinedFunction,
} from '../parser/interface'
import type { SourceCodeInfo } from '../tokenizer/interface'
import { toAny } from '../utils'
import { valueToString } from '../utils/debug/debugTools'
import { FunctionType } from '../constants/constants'
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

function findOverloadFunction(
  overloads: EvaluatedFunctionOverload[],
  nbrOfParams: number,
  sourceCodeInfo?: SourceCodeInfo,
): EvaluatedFunctionOverload {
  const overloadFunction = overloads.find((overload) => {
    const arity = overload.a
    if (typeof arity === 'number')
      return arity === nbrOfParams
    else
      return arity.min <= nbrOfParams
  })
  if (!overloadFunction)
    throw new LitsError(`Unexpected number of arguments, got ${valueToString(nbrOfParams)}.`, sourceCodeInfo)

  return overloadFunction
}

export const functionExecutors: FunctionExecutors = {
  [FunctionType.NativeJsFunction]: (fn: NativeJsFunction, params, sourceCodeInfo) => {
    try {
      // eslint-disable-next-line ts/no-unsafe-assignment
      const clonedParams = JSON.parse(JSON.stringify(params))
      // eslint-disable-next-line ts/no-unsafe-argument
      return toAny(fn.f.fn(...clonedParams))
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
  [FunctionType.UserDefined]: (fn: UserDefinedFunction, params, sourceCodeInfo, contextStack, { evaluateAstNode }) => {
    for (;;) {
      const overloadFunction = findOverloadFunction(fn.o, params.length, sourceCodeInfo)
      const args = overloadFunction.as
      const nbrOfMandatoryArgs: number = args.mandatoryArguments.length

      const newContext: Context = { ...overloadFunction.f }

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
        const newContextStack = contextStack.create(newContext, fn.x)
        for (const node of overloadFunction.b)
          result = evaluateAstNode(node, newContextStack)

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
  [FunctionType.Partial]: (fn: PartialFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return executeFunction(fn.f, [...fn.p, ...params], contextStack, sourceCodeInfo)
  },
  [FunctionType.Comp]: (fn: CompFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const { f } = fn
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
  [FunctionType.Constantly]: (fn: ConstantlyFunction) => {
    return fn.v
  },
  [FunctionType.Juxt]: (fn: JuxtFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return fn.f.map(fun => executeFunction(toAny(fun), params, contextStack, sourceCodeInfo))
  },
  [FunctionType.Complement]: (fn: ComplementFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    return !executeFunction(fn.f, params, contextStack, sourceCodeInfo)
  },
  [FunctionType.EveryPred]: (fn: EveryPredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    for (const f of fn.f) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack, sourceCodeInfo)
        if (!result)
          return false
      }
    }
    return true
  },
  [FunctionType.SomePred]: (fn: SomePredFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    for (const f of fn.f) {
      for (const param of params) {
        const result = executeFunction(toAny(f), [param], contextStack, sourceCodeInfo)
        if (result)
          return true
      }
    }
    return false
  },
  [FunctionType.Fnull]: (fn: FNullFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const fnulledParams = params.map((param, index) => (param === null ? toAny(fn.p[index]) : param))
    return executeFunction(toAny(fn.f), fnulledParams, contextStack, sourceCodeInfo)
  },
  [FunctionType.Builtin]: (fn: BuiltinFunction, params, sourceCodeInfo, contextStack, { executeFunction }) => {
    const normalExpression = asNonUndefined(normalExpressions[fn.n], sourceCodeInfo)
    return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction })
  },
}
