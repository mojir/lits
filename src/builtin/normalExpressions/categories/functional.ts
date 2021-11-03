import { Any, Arr } from '../../../interface'
import {
  ComplementFunction,
  CompFunction,
  ConstantlyFunction,
  EveryPredFunction,
  FUNCTION_SYMBOL,
  JuxtFunction,
  PartialFunction,
  SomePredFunction,
  FNilFunction,
} from '../../../parser/interface'
import { toAny } from '../../../utils'
import { litsFunction, array, assertNumberOfParams } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'
export const functionalNormalExpression: BuiltinNormalExpressions = {
  apply: {
    evaluate: ([func, ...params]: Arr, sourceCodeInfo, contextStack, { executeFunction }): Any => {
      litsFunction.assert(func, sourceCodeInfo)
      const paramsLength = params.length
      const last = params[paramsLength - 1]
      array.assert(last, sourceCodeInfo)
      const applyArray = [...params.slice(0, -1), ...last]
      return executeFunction(func, applyArray, sourceCodeInfo, contextStack)
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },

  identity: {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    validate: node => assertNumberOfParams(1, node),
  },

  partial: {
    evaluate: ([fn, ...params], sourceCodeInfo): PartialFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `partial`,
        fn: toAny(fn),
        params,
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  comp: {
    evaluate: (fns, sourceCodeInfo): CompFunction => {
      if (fns.length > 1) {
        const last = fns[fns.length - 1]
        if (array.is(last)) {
          fns = [...fns.slice(0, -1), ...last]
        }
      }
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `comp`,
        fns,
      }
    },
  },

  constantly: {
    evaluate: ([value], sourceCodeInfo): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `constantly`,
        value: toAny(value),
      }
    },
    validate: node => assertNumberOfParams(1, node),
  },

  juxt: {
    evaluate: (fns, sourceCodeInfo): JuxtFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `juxt`,
        fns,
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  complement: {
    evaluate: ([fn], sourceCodeInfo): ComplementFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `complement`,
        fn: toAny(fn),
      }
    },
    validate: node => assertNumberOfParams(1, node),
  },

  'every-pred': {
    evaluate: (fns, sourceCodeInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `every-pred`,
        fns,
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  'some-pred': {
    evaluate: (fns, sourceCodeInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `some-pred`,
        fns,
      }
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  fnil: {
    evaluate: ([fn, ...params], sourceCodeInfo): FNilFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo,
        type: `fnil`,
        fn: toAny(fn),
        params,
      }
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
}
