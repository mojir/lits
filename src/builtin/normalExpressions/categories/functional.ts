import { FunctionType } from '../../../constants/constants'
import type { Any, Arr } from '../../../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  PartialFunction,
  SomePredFunction,
} from '../../../parser/interface'
import { toAny } from '../../../utils'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import { FUNCTION_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertArray } from '../../../typeGuards/array'

export const functionalNormalExpression: BuiltinNormalExpressions = {
  'apply': {
    evaluate: ([func, ...params]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      assertLitsFunction(func, sourceCodeInfo)
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

  'partial': {
    evaluate: ([fn, ...params], sourceCodeInfo): PartialFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Partial,
        f: toAny(fn),
        p: params,
      }
    },
    paramCount: { min: 1 },
  },

  'comp': {
    evaluate: (fns, sourceCodeInfo): CompFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Comp,
        f: fns,
      }
    },
    paramCount: {},
  },

  'constantly': {
    evaluate: ([value], sourceCodeInfo): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Constantly,
        v: toAny(value),
      }
    },
    paramCount: 1,
  },

  'juxt': {
    evaluate: (fns, sourceCodeInfo): JuxtFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Juxt,
        f: fns,
      }
    },
    paramCount: { min: 1 },
  },

  'complement': {
    evaluate: ([fn], sourceCodeInfo): ComplementFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Complement,
        f: toAny(fn),
      }
    },
    paramCount: 1,
  },

  'every-pred': {
    evaluate: (fns, sourceCodeInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.EveryPred,
        f: fns,
      }
    },
    paramCount: { min: 1 },
  },

  'some-pred': {
    evaluate: (fns, sourceCodeInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.SomePred,
        f: fns,
      }
    },
    paramCount: { min: 1 },
  },

  'fnull': {
    evaluate: ([fn, ...params], sourceCodeInfo): FNullFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Fnull,
        f: toAny(fn),
        p: params,
      }
    },
    paramCount: { min: 2 },
  },
}
