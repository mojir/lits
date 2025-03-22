import { evalueateBindingNodeValues } from '../builtin/bindingNode'
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
import { asNonUndefined, isUnknownRecord } from '../typeGuards'
import { asAny } from '../typeGuards/lits'
import { toAny } from '../utils'
import { valueToString } from '../utils/debug/debugTools'
import type { ContextStack } from './ContextStack'
import type { Context, EvaluateNode, ExecuteFunction } from './interface'

type FunctionExecutors = Record<
  LitsFunctionType,
  (
    fn: any,
    params: Arr,
    sourceCodeInfo: SourceCodeInfo | undefined,
    contextStack: ContextStack,
    helpers: { evaluateNode: EvaluateNode, executeFunction: ExecuteFunction },
  ) => Any
>

function checkParams(
  evaluatedFunction: EvaluatedFunction,
  nbrOfParams: number,
  sourceCodeInfo?: SourceCodeInfo,
) {
  const hasRest = evaluatedFunction.arguments.some(arg => arg.type === 'rest')
  const minArity = evaluatedFunction.arguments.filter(arg => arg.type !== 'rest' && !arg.default).length
  const maxArity = hasRest ? Number.MAX_SAFE_INTEGER : evaluatedFunction.arguments.length
  if (nbrOfParams < minArity || nbrOfParams > maxArity) {
    throw new LitsError(`Unexpected number of arguments, got ${valueToString(nbrOfParams)}.`, sourceCodeInfo)
  }
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
  UserDefined: (fn: UserDefinedFunction, params, sourceCodeInfo, contextStack, { evaluateNode }) => {
    for (;;) {
      checkParams(fn.evaluatedfunction, params.length, sourceCodeInfo)
      const evaluatedFunction = fn.evaluatedfunction
      const args = evaluatedFunction.arguments
      const nbrOfNonRestArgs: number = args.filter(arg => arg.type !== 'rest').length

      const newContextStack = contextStack.create(fn.evaluatedfunction.context)
      const newContext: Context = {}

      const rest: Arr = []
      for (let i = 0; i < params.length; i += 1) {
        if (i < nbrOfNonRestArgs) {
          const param = toAny(params[i])
          const valueRecord = evalueateBindingNodeValues(args[i]!, param, Node =>
            evaluateNode(Node, newContextStack.create(newContext)))
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
        const defaultValue = evaluateNode(arg.default!, contextStack.create(newContext))
        const valueRecord = evalueateBindingNodeValues(arg, defaultValue, Node =>
          evaluateNode(Node, contextStack.create(newContext)))
        Object.entries(valueRecord).forEach(([key, value]) => {
          newContext[key] = { value }
        })
      }

      const restArgument = args.find(arg => arg.type === 'rest')
      if (restArgument !== undefined) {
        const valueRecord = evalueateBindingNodeValues(restArgument, rest, Node => evaluateNode(Node, contextStack.create(newContext)))
        Object.entries(valueRecord).forEach(([key, value]) => {
          newContext[key] = { value }
        })
      }

      try {
        let result: Any = null
        const newContextStack2 = newContextStack.create(newContext)
        for (const node of evaluatedFunction.body) {
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
