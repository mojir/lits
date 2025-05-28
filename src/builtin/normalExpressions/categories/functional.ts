import type { Any, Arr } from '../../../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNullFunction,
  FunctionLike,
  JuxtFunction,
  SomePredFunction,
} from '../../../parser/types'
import { toAny } from '../../../utils'
import { getArityFromFunction, getCommonArityFromFunctions, toFixedArity } from '../../../utils/arity'
import { FUNCTION_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertArray } from '../../../typeGuards/array'
import { asFunctionLike, assertFunctionLike } from '../../../typeGuards/lits'
import { LitsError } from '../../../errors'

export const functionalNormalExpression: BuiltinNormalExpressions = {
  '|>': {
    evaluate: ([value, func], sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertFunctionLike(func, sourceCodeInfo)
      return executeFunction(func, [value], contextStack, sourceCodeInfo)
    },
    arity: toFixedArity(2),
  },
  'apply': {
    evaluate: ([func, ...params]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertFunctionLike(func, sourceCodeInfo)
      const paramsLength = params.length
      const last = params[paramsLength - 1]
      assertArray(last, sourceCodeInfo)
      const applyArray = [...params.slice(0, -1), ...last]
      return executeFunction(func, applyArray, contextStack, sourceCodeInfo)
    },
    arity: { min: 2 },
  },

  'identity': {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    arity: toFixedArity(1),
  },

  'comp': {
    evaluate: (params, sourceCodeInfo): CompFunction => {
      params.forEach(param => assertFunctionLike(param, sourceCodeInfo))
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Comp',
        params,
        arity: params.length > 0 ? getArityFromFunction(params.at(-1) as FunctionLike) : { min: 1, max: 1 },
        docString: '',
      }
    },
    arity: {},
  },

  'constantly': {
    evaluate: ([value], sourceCodeInfo): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Constantly',
        value: toAny(value),
        arity: {},
        docString: '',
      }
    },
    arity: toFixedArity(1),
  },

  'juxt': {
    evaluate: (params, sourceCodeInfo): JuxtFunction => {
      params.forEach(param => assertFunctionLike(param, sourceCodeInfo))
      const arity = getCommonArityFromFunctions(params as FunctionLike[])
      if (arity === null) {
        throw new LitsError('All functions must accept the same number of arguments', sourceCodeInfo)
      }
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Juxt',
        params,
        arity,
        docString: '',
      }
    },
    arity: { min: 1 },
  },

  'complement': {
    evaluate: ([fn], sourceCodeInfo): ComplementFunction => {
      const fun = asFunctionLike(fn, sourceCodeInfo)
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Complement',
        function: fun,
        arity: getArityFromFunction(fun),
        docString: '',
      }
    },
    arity: toFixedArity(1),
  },

  'every-pred': {
    evaluate: (params, sourceCodeInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'EveryPred',
        params,
        arity: { min: 1, max: 1 },
        docString: '',
      }
    },
    arity: { min: 1 },
  },

  'some-pred': {
    evaluate: (params, sourceCodeInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'SomePred',
        params,
        arity: { min: 1, max: 1 },
        docString: '',
      }
    },
    arity: { min: 1 },
  },

  'fnull': {
    evaluate: ([fn, ...params], sourceCodeInfo): FNullFunction => {
      const fun = asFunctionLike(fn, sourceCodeInfo)
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Fnull',
        function: fun,
        params,
        arity: getArityFromFunction(fun),
        docString: '',
      }
    },
    arity: { min: 2 },
  },
}
