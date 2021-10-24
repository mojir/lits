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
import { assertArr, assertLength, assertLispishFunction, isArr, toAny } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const functionalNormalExpression: BuiltinNormalExpressions = {
  apply: {
    evaluate: ([func, ...params]: Arr, contextStack, { executeFunction }): Any => {
      assertLispishFunction(func)
      const paramsLength = params.length
      const last = params[paramsLength - 1]
      assertArr(last)
      const applyArray = [...params.slice(0, -1), ...last]
      return executeFunction(func, applyArray, contextStack)
    },
    validate: node => assertLength({ min: 2 }, node),
  },

  identity: {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    validate: node => assertLength(1, node),
  },

  partial: {
    evaluate: ([fn, ...params]): PartialFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `partial`,
        fn: toAny(fn),
        params,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  comp: {
    evaluate: (fns): CompFunction => {
      if (fns.length > 1) {
        const last = fns[fns.length - 1]
        if (isArr(last)) {
          fns = [...fns.slice(0, -1), ...last]
        }
      }
      return {
        [FUNCTION_SYMBOL]: true,
        type: `comp`,
        fns,
      }
    },
  },

  constantly: {
    evaluate: ([value]): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `constantly`,
        value: toAny(value),
      }
    },
    validate: node => assertLength(1, node),
  },

  juxt: {
    evaluate: (fns): JuxtFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `juxt`,
        fns,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  complement: {
    evaluate: ([fn]): ComplementFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `complement`,
        fn: toAny(fn),
      }
    },
    validate: node => assertLength(1, node),
  },

  'every-pred': {
    evaluate: (fns): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `every-pred`,
        fns,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  'some-pred': {
    evaluate: (fns): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `some-pred`,
        fns,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  fnil: {
    evaluate: ([fn, ...params]): FNilFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        type: `fnil`,
        fn: toAny(fn),
        params,
      }
    },
    validate: node => assertLength({ min: 2 }, node),
  },
}
