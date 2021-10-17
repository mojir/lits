import { Arr } from '../../../interface'
import {
  ComplementLispishFunction,
  CompLispishFunction,
  ConstantlyLispishFunction,
  EveryPredLispishFunction,
  functionSymbol,
  JuxtLispishFunction,
  PartialLispishFunction,
} from '../../../parser/interface'
import { assertArr, assertLength, assertLispishFunction, isArr } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const functionalNormalExpression: BuiltinNormalExpressions = {
  apply: {
    evaluate: ([func, ...params]: Arr, contextStack, { evaluateFunction }): unknown => {
      assertLispishFunction(func)
      const paramsLength = params.length
      const last = params[paramsLength - 1]
      assertArr(last)
      const applyArray = [...params.slice(0, -1), ...last]
      return evaluateFunction(func, applyArray, contextStack)
    },
    validate: node => assertLength({ min: 2 }, node),
  },

  identity: {
    evaluate: ([value]): unknown => {
      return value
    },
    validate: node => assertLength(1, node),
  },

  partial: {
    evaluate: ([fn, ...params]): PartialLispishFunction => {
      return {
        [functionSymbol]: true,
        type: `partial`,
        fn,
        params,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  comp: {
    evaluate: (fns): CompLispishFunction => {
      if (fns.length > 1) {
        const last = fns[fns.length - 1]
        if (isArr(last)) {
          fns = [...fns.slice(0, -1), ...last]
        }
      }
      return {
        [functionSymbol]: true,
        type: `comp`,
        fns,
      }
    },
  },

  constantly: {
    evaluate: ([value]): ConstantlyLispishFunction => {
      return {
        [functionSymbol]: true,
        type: `constantly`,
        value,
      }
    },
    validate: node => assertLength(1, node),
  },

  juxt: {
    evaluate: (fns): JuxtLispishFunction => {
      return {
        [functionSymbol]: true,
        type: `juxt`,
        fns,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },

  complement: {
    evaluate: ([fn]): ComplementLispishFunction => {
      return {
        [functionSymbol]: true,
        type: `complement`,
        fn,
      }
    },
    validate: node => assertLength(1, node),
  },

  'every-pred': {
    evaluate: (fns): EveryPredLispishFunction => {
      return {
        [functionSymbol]: true,
        type: `every-pred`,
        fns,
      }
    },
    validate: node => assertLength({ min: 1 }, node),
  },
}
