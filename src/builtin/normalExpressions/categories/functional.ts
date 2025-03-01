import { FunctionType } from '../../../constants/constants'
import type { Any, Arr } from '../../../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FNilFunction,
  JuxtFunction,
  PartialFunction,
  SomePredFunction,
} from '../../../parser/interface'
import { toAny } from '../../../utils'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import { FUNCTION_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertArray } from '../../../typeGuards/array'
import { assertNumberOfParams } from '../../../typeGuards'

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
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },

  'identity': {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    validate: node => assertNumberOfParams(1, node),
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
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  'comp': {
    evaluate: (fns, sourceCodeInfo): CompFunction => {
      if (fns.length > 1) {
        const last = fns[fns.length - 1]
        if (Array.isArray(last))
          // eslint-disable-next-line ts/no-unsafe-assignment
          fns = [...fns.slice(0, -1), ...last]
      }
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Comp,
        f: fns,
      }
    },
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
    validate: node => assertNumberOfParams(1, node),
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
    validate: node => assertNumberOfParams({ min: 1 }, node),
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
    validate: node => assertNumberOfParams(1, node),
  },

  'every_pred': {
    evaluate: (fns, sourceCodeInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.EveryPred,
        f: fns,
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  'some_pred': {
    evaluate: (fns, sourceCodeInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.SomePred,
        f: fns,
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  'fnil': {
    evaluate: ([fn, ...params], sourceCodeInfo): FNilFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        t: FunctionType.Fnil,
        f: toAny(fn),
        p: params,
      }
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
}
