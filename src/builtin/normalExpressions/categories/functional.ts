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
import { getCommonParamCountFromFunctions, getParamCountFromFunction } from '../../../utils/paramCount'
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
    paramCount: 2,
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
    paramCount: { min: 2 },
  },

  'identity': {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    paramCount: 1,
  },

  'comp': {
    evaluate: (params, sourceCodeInfo): CompFunction => {
      params.forEach(param => assertFunctionLike(param, sourceCodeInfo))
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Comp',
        params,
        paramCount: params.length > 0 ? getParamCountFromFunction(params.at(-1) as FunctionLike) : 1,
        docString: '',
      }
    },
    paramCount: {},
  },

  'constantly': {
    evaluate: ([value], sourceCodeInfo): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Constantly',
        value: toAny(value),
        paramCount: {},
        docString: '',
      }
    },
    paramCount: 1,
  },

  'juxt': {
    evaluate: (params, sourceCodeInfo): JuxtFunction => {
      params.forEach(param => assertFunctionLike(param, sourceCodeInfo))
      const paramCount = getCommonParamCountFromFunctions(params as FunctionLike[])
      if (paramCount === null) {
        throw new LitsError('All functions must accept the same number of arguments', sourceCodeInfo)
      }
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Juxt',
        params,
        paramCount,
        docString: '',
      }
    },
    paramCount: { min: 1 },
  },

  'complement': {
    evaluate: ([fn], sourceCodeInfo): ComplementFunction => {
      const fun = asFunctionLike(fn, sourceCodeInfo)
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Complement',
        function: fun,
        paramCount: getParamCountFromFunction(fun),
        docString: '',
      }
    },
    paramCount: 1,
  },

  'every-pred': {
    evaluate: (params, sourceCodeInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'EveryPred',
        params,
        paramCount: 1,
        docString: '',
      }
    },
    paramCount: { min: 1 },
  },

  'some-pred': {
    evaluate: (params, sourceCodeInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'SomePred',
        params,
        paramCount: 1,
        docString: '',
      }
    },
    paramCount: { min: 1 },
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
        paramCount: getParamCountFromFunction(fun),
        docString: '',
      }
    },
    paramCount: { min: 2 },
  },
}
