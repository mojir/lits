import type { Any, Arr } from '../../../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  SomePredFunction,
} from '../../../parser/types'
import { toAny } from '../../../utils'
import { FUNCTION_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertArray } from '../../../typeGuards/array'
import { asFunctionLike, assertFunctionLike } from '../../../typeGuards/lits'

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
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Comp',
        params,
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
      }
    },
    paramCount: 1,
  },

  'juxt': {
    evaluate: (params, sourceCodeInfo): JuxtFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Juxt',
        params,
      }
    },
    paramCount: { min: 1 },
  },

  'complement': {
    evaluate: ([fn], sourceCodeInfo): ComplementFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Complement',
        function: asFunctionLike(fn, sourceCodeInfo),
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
      }
    },
    paramCount: { min: 1 },
  },

  'fnull': {
    evaluate: ([fn, ...params], sourceCodeInfo): FNullFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        functionType: 'Fnull',
        function: asFunctionLike(fn, sourceCodeInfo),
        params,
      }
    },
    paramCount: { min: 2 },
  },
}
