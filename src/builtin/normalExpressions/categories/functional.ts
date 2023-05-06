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
    evaluate: ([func, ...params]: Arr, debugInfo, contextStack, { executeFunction }): Any => {
      litsFunction.assert(func, debugInfo)
      const paramsLength = params.length
      const last = params[paramsLength - 1]
      array.assert(last, debugInfo)
      const applyArray = [...params.slice(0, -1), ...last]
      return executeFunction(func, applyArray, contextStack, debugInfo)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `apply`, debugInfo),
  },

  identity: {
    evaluate: ([value]): Any => {
      return toAny(value)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `identity`, debugInfo),
  },

  partial: {
    evaluate: ([fn, ...params], debugInfo): PartialFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `partial`,
        fn: toAny(fn),
        params,
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `partial`, debugInfo),
  },

  comp: {
    evaluate: (fns, debugInfo): CompFunction => {
      if (fns.length > 1) {
        const last = fns[fns.length - 1]
        if (array.is(last)) {
          fns = [...fns.slice(0, -1), ...last]
        }
      }
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `comp`,
        fns,
      }
    },
    validateArity: () => undefined,
  },

  constantly: {
    evaluate: ([value], debugInfo): ConstantlyFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `constantly`,
        value: toAny(value),
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `constantly`, debugInfo),
  },

  juxt: {
    evaluate: (fns, debugInfo): JuxtFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `juxt`,
        fns,
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `juxt`, debugInfo),
  },

  complement: {
    evaluate: ([fn], debugInfo): ComplementFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `complement`,
        fn: toAny(fn),
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `complement`, debugInfo),
  },

  'every-pred': {
    evaluate: (fns, debugInfo): EveryPredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `every-pred`,
        fns,
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `every-pred`, debugInfo),
  },

  'some-pred': {
    evaluate: (fns, debugInfo): SomePredFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `some-pred`,
        fns,
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `some-pred`, debugInfo),
  },

  fnil: {
    evaluate: ([fn, ...params], debugInfo): FNilFunction => {
      return {
        [FUNCTION_SYMBOL]: true,
        debugInfo,
        type: `fnil`,
        fn: toAny(fn),
        params,
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `fnil`, debugInfo),
  },
}
