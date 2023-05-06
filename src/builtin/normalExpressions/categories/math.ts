import { Type } from '../../../types/Type'
import { Arr } from '../../../interface'
import { MAX_NUMBER, MIN_NUMBER } from '../../../utils'
import { any, assertNumberOfParams, asValue, number } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'
import { DebugInfo } from '../../../tokenizer/interface'

export const mathNormalExpression: BuiltinNormalExpressions = {
  inc: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }
        return first + 1
      } else {
        const paramType = Type.of(first)

        if (paramType.is(Type.zero)) {
          return 1
        }

        const types: Type[] = []

        if (paramType.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (paramType.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (paramType.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }

        if (paramType.intersects(Type.zero)) {
          types.push(Type.positiveInteger)
        }
        if (paramType.intersects(Type.negativeFloat)) {
          if (paramType.isInteger()) {
            types.push(Type.nonPositiveInteger)
          } else {
            types.push(Type.float)
          }
        }
        if (paramType.intersects(Type.positiveFloat)) {
          types.push(Type.positiveInfinity)
          if (paramType.isInteger()) {
            types.push(Type.positiveInteger)
          } else {
            types.push(Type.positiveFloat)
          }
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `inc`, debugInfo),
  },

  dec: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }
        return first - 1
      } else {
        const paramType = Type.of(first)

        if (paramType.is(Type.zero)) {
          return -1
        }

        const types: Type[] = []

        if (paramType.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (paramType.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (paramType.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }

        if (paramType.intersects(Type.zero)) {
          types.push(Type.negativeInteger)
        }
        if (paramType.intersects(Type.positiveFloat)) {
          if (paramType.isInteger()) {
            types.push(Type.nonNegativeInteger)
          } else {
            types.push(Type.float)
          }
        }
        if (paramType.intersects(Type.negativeFloat)) {
          types.push(Type.negativeInfinity)
          if (paramType.isInteger()) {
            types.push(Type.negativeInteger)
          } else {
            types.push(Type.negativeFloat)
          }
        }
        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `dec`, debugInfo),
  },

  '+': {
    evaluate: (params, debugInfo): number | Type => {
      if (params.every(param => !(param instanceof Type))) {
        return params.reduce((result: number, param) => {
          if (!number.is(param)) {
            return NaN
          }
          return result + param
        }, 0)
      } else {
        return getTypeOfSum(params, debugInfo)
      }
    },
    validateArity: () => undefined,
  },

  '-': {
    evaluate: (params, debugInfo): number | Type => {
      if (params.every(param => !(param instanceof Type))) {
        if (params.length === 0) {
          return 0
        }
        const [first, ...rest] = params
        if (!number.is(first)) {
          return NaN
        }
        if (rest.length === 0) {
          return -first
        }
        return rest.reduce((result: number, param) => {
          if (!number.is(param)) {
            return NaN
          }
          return result - param
        }, first)
      } else {
        const firstParam = asValue(params[0])
        if (params.length === 1) {
          return negate(firstParam)
        }
        const rest = params.slice(1).map(negate)
        return getTypeOfSum([firstParam, ...rest], debugInfo)
      }
    },
    validateArity: () => undefined,
  },

  '*': {
    evaluate: (params): number | Type => {
      if (params.every(param => !(param instanceof Type))) {
        return params.reduce((result: number, param) => {
          if (!number.is(param)) {
            return NaN
          }
          return result * param
        }, 1)
      } else {
        const paramTypes = params.map(param => Type.of(param))
        return getTypeOfProduct(paramTypes)
      }
    },
    validateArity: () => undefined,
  },

  '/': {
    evaluate: (params): number | Type => {
      if (params.every(param => !(param instanceof Type))) {
        if (params.length === 0) {
          return 1
        }
        const [first, ...rest] = params
        if (!number.is(first)) {
          return NaN
        }

        if (rest.length === 0) {
          return 1 / first
        }

        return rest.reduce((result: number, param) => {
          if (!number.is(param)) {
            return NaN
          }
          return result / param
        }, first)
      } else {
        const paramTypes = params.map(param => Type.of(param))

        return getTypeOfDivision(paramTypes).toNumberValue()
      }
    },
    validateArity: () => undefined,
  },

  quot: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [dividend, divisor] = params
        if (!number.is(dividend)) {
          return NaN
        }
        if (!number.is(divisor)) {
          return NaN
        }
        return Math.trunc(dividend / divisor)
      } else {
        const a = Type.of(params[0])
        const b = Type.of(params[1])

        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (b.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.or(b).intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.positiveInfinity) && b.exclude(Type.nan).intersects(Type.number)) {
          types.push(Type.positiveInfinity)
        }

        if (a.intersects(Type.negativeInfinity) && b.exclude(Type.nan).intersects(Type.number)) {
          types.push(Type.negativeInfinity)
        }

        if (b.intersects(Type.positiveInfinity.or(Type.negativeInfinity))) {
          if (a.intersects(Type.float)) {
            types.push(Type.zero)
          }
        }

        if (a.intersects(Type.positiveZero) && b.intersects(Type.nonZeroFloat)) {
          types.push(Type.positiveZero)
        }
        if (a.intersects(Type.negativeZero) && b.intersects(Type.nonZeroFloat)) {
          types.push(Type.negativeZero)
        }

        if (b.intersects(Type.positiveZero)) {
          if (a.intersects(Type.zero)) {
            types.push(Type.nan)
          }
          if (a.intersects(Type.positiveFloat)) {
            types.push(Type.positiveInfinity)
          }
          if (a.intersects(Type.negativeFloat)) {
            types.push(Type.negativeInfinity)
          }
        }

        if (b.intersects(Type.negativeZero)) {
          if (a.intersects(Type.zero)) {
            types.push(Type.nan)
          }
          if (a.intersects(Type.positiveFloat)) {
            types.push(Type.negativeInfinity)
          }
          if (a.intersects(Type.negativeFloat)) {
            types.push(Type.positiveInfinity)
          }
        }

        if (a.intersects(Type.positiveFloat)) {
          if (b.intersects(Type.positiveFloat)) {
            types.push(Type.nonNegativeInteger)
            types.push(Type.positiveInfinity)
          }
          if (b.intersects(Type.negativeFloat)) {
            types.push(Type.nonPositiveInteger)
            types.push(Type.negativeInfinity)
          }
        }
        if (a.intersects(Type.negativeFloat)) {
          if (b.intersects(Type.negativeFloat)) {
            types.push(Type.nonNegativeInteger)
            types.push(Type.positiveInfinity)
          }
          if (b.intersects(Type.positiveFloat)) {
            types.push(Type.nonPositiveInteger)
            types.push(Type.negativeInfinity)
          }
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `quot`, debugInfo),
  },

  mod: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [dividend, divisor] = params
        if (!number.is(dividend)) {
          return NaN
        }
        if (!number.is(divisor)) {
          return NaN
        }
        const quotient = Math.floor(dividend / divisor)
        if (quotient === 0) {
          return dividend
        }
        return dividend - divisor * quotient
      } else {
        const a = Type.of(params[0])
        const b = Type.of(params[1])

        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (b.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.nan) || a.intersects(Type.infinity)) {
          types.push(Type.nan)
        }
        if (b.intersects(Type.nan)) {
          types.push(Type.nan)
        }
        if (b.intersects(Type.positiveInfinity) && a.intersects(Type.float)) {
          types.push(Type.zero)
        }
        if (b.intersects(Type.negativeInfinity) && a.intersects(Type.float)) {
          types.push(Type.zero)
        }

        if (b.intersects(Type.zero)) {
          types.push(Type.nan)
        }
        if (b.intersects(Type.negativeFloat) && a.intersects(Type.float)) {
          types.push(Type.nonPositiveFloat)
        }
        if (b.intersects(Type.positiveFloat) && a.intersects(Type.float)) {
          types.push(Type.nonNegativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `mod`, debugInfo),
  },

  rem: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [dividend, divisor] = params
        if (!number.is(dividend)) {
          return NaN
        }
        if (!number.is(divisor)) {
          return NaN
        }

        const quotient = Math.trunc(dividend / divisor)

        if (quotient === 0) {
          return dividend
        }

        return dividend - divisor * quotient
      } else {
        const a = Type.of(params[0])
        const b = Type.of(params[1])

        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (b.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.or(b).intersects(Type.nan.or(Type.infinity))) {
          types.push(Type.nan)
        }
        if (b.intersects(Type.zero)) {
          types.push(Type.nan)
        }
        if (a.intersects(Type.negativeFloat) && b.intersects(Type.float)) {
          types.push(Type.nonPositiveFloat)
        }
        if (a.intersects(Type.positiveFloat) && b.intersects(Type.float)) {
          types.push(Type.nonNegativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `rem`, debugInfo),
  },

  sqrt: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }
        // Handle Math.sqrt(-0) ==> -0, which I think is wrong.
        return first === 0 ? 0 : Math.sqrt(first)
      } else {
        const type = Type.of(first)

        const types: Type[] = []

        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.nan.or(Type.negativeInfinity))) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }

        if (type.intersects(Type.zero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.positiveFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `sqrt`, debugInfo),
  },

  cbrt: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }
        return Math.cbrt(first)
      } else {
        const type = Type.of(first)

        const types: Type[] = []

        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.nan)) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (type.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }

        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }

        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }

        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.positiveFloat)
        }

        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.negativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `cbrt`, debugInfo),
  },

  pow: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        if (!number.is(first)) {
          return NaN
        }
        if (!number.is(second)) {
          return NaN
        }
        if (first === 1 && !Number.isNaN(second)) {
          return 1
        }
        if (first < 0 && first >= -1 && second === -Infinity) {
          return NaN
        }
        if (first <= -1 && second === Infinity) {
          return NaN
        }
        return Math.pow(first, second)
      } else {
        const a = asValue(Type.of(params[0]))
        const b = asValue(Type.of(params[1]))

        const types: Type[] = []

        let ones = 0

        if (a.or(b).intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.nan)) {
          if (b.intersects(Type.zero)) {
            types.push(Type.positiveInteger)
          }
          if (!b.exclude(Type.zero).isNever()) {
            types.push(Type.nan)
          }
        }

        if (b.intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.positiveInfinity)) {
          if (b.intersects(Type.positiveInfinity.or(Type.positiveFloat))) {
            types.push(Type.positiveInfinity)
          }
          if (b.intersects(Type.negativeInfinity.or(Type.negativeFloat))) {
            types.push(Type.positiveZero)
          }
          if (b.intersects(Type.zero)) {
            types.push(Type.positiveInteger)
            ones += 1
          }
        }
        if (a.intersects(Type.negativeInfinity)) {
          if (b.intersects(Type.positiveInfinity)) {
            types.push(Type.nan)
          }
          if (b.intersects(Type.positiveFloat)) {
            types.push(Type.positiveInfinity)
            types.push(Type.negativeInfinity)
          }
          if (b.intersects(Type.negativeInfinity.or(Type.negativeFloat))) {
            types.push(Type.positiveZero)
          }
          if (b.intersects(Type.negativeInteger)) {
            types.push(Type.negativeZero)
          }
          if (b.intersects(Type.zero)) {
            types.push(Type.positiveInteger)
            ones += 1
          }
        }

        if (b.intersects(Type.positiveInfinity)) {
          if (a.intersects(Type.zero)) {
            types.push(Type.zero)
          }
          if (a.intersects(Type.positiveFloat)) {
            types.push(Type.positiveInfinity)
            types.push(Type.positiveInteger)
            if (!a.isInteger()) {
              types.push(Type.zero)
            }
          }
          if (a.intersects(Type.negativeFloat)) {
            types.push(Type.nan)
            if (!a.isInteger()) {
              types.push(Type.zero)
            }
          }
        }

        if (b.intersects(Type.negativeInfinity)) {
          if (a.intersects(Type.zero)) {
            types.push(Type.positiveInfinity)
          }
          if (a.intersects(Type.positiveFloat)) {
            types.push(Type.positiveInteger)
            types.push(Type.zero)
            if (!a.isInteger()) {
              types.push(Type.positiveInfinity)
            }
          }
          if (a.intersects(Type.negativeFloat)) {
            types.push(Type.nan)
            types.push(Type.zero)
          }
        }

        if (a.intersects(Type.positiveZero)) {
          if (b.intersects(Type.zero)) {
            types.push(Type.positiveInteger)
            ones += 1
          }
          if (b.intersects(Type.positiveFloat)) {
            types.push(Type.positiveZero)
          }
          if (b.intersects(Type.negativeFloat)) {
            types.push(Type.positiveInfinity)
          }
        }
        if (a.intersects(Type.negativeZero)) {
          if (b.intersects(Type.zero)) {
            types.push(Type.positiveInteger)
            ones += 1
          }
          if (b.intersects(Type.positiveFloat)) {
            types.push(Type.positiveZero)
            types.push(Type.negativeZero)
          }
          if (b.intersects(Type.negativeFloat)) {
            types.push(Type.positiveInfinity)
            types.push(Type.negativeInfinity)
          }
        }

        if (b.intersects(Type.zero)) {
          if (a.intersects(Type.float)) {
            types.push(Type.positiveInteger)
            ones += 1
          }
        }

        if (a.intersects(Type.positiveFloat)) {
          if (b.intersects(Type.positiveFloat)) {
            types.push(Type.positiveInfinity)
          }
          if (a.isInteger()) {
            if (b.intersects(Type.positiveFloat)) {
              if (b.isInteger()) {
                types.push(Type.positiveInteger)
              } else {
                types.push(Type.positiveFloat)
              }
            }
            if (b.intersects(Type.negativeFloat)) {
              types.push(Type.positiveFloat)
              types.push(Type.positiveZero)
            }
          } else {
            if (b.intersects(Type.positiveFloat)) {
              types.push(Type.positiveFloat)
            }
            if (b.intersects(Type.negativeFloat)) {
              types.push(Type.positiveFloat)
              types.push(Type.positiveZero)
            }
          }
        }

        if (a.intersects(Type.negativeFloat)) {
          if (b.intersects(Type.positiveInteger)) {
            types.push(Type.infinity)
          }
          if (b.intersects(Type.float) && !b.isInteger()) {
            types.push(Type.nan)
          }
          if (a.isInteger()) {
            if (b.intersects(Type.positiveInteger)) {
              types.push(Type.positiveInteger)
              types.push(Type.negativeInteger)
            }
            if (b.intersects(Type.negativeInteger)) {
              types.push(Type.positiveFloat)
              types.push(Type.negativeFloat)
              types.push(Type.zero)
            }
          } else {
            if (b.intersects(Type.nonZeroInteger)) {
              types.push(Type.positiveFloat)
              types.push(Type.negativeFloat)
            }
          }
        }

        if (ones > 0 && types.every(t => t.equals(Type.positiveInteger))) {
          if (types.length === ones) {
            return 1
          }
        }
        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `pow`, debugInfo),
  },

  round: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [value, decimals] = params

        if (!number.is(value)) {
          return NaN
        }

        if (params.length === 1 || decimals === 0) {
          return Math.round(value)
        }

        if (!number.is(decimals)) {
          return NaN
        }

        if (!number.is(decimals, { integer: true, nonNegative: true })) {
          return NaN
        }
        const factor = Math.pow(10, decimals)
        if (factor === Infinity) {
          return value
        }
        return Math.round(value * factor) / factor
      } else {
        const a = asValue(Type.of(params[0]))
        const b = asValue(Type.of(params[1] ?? 0))

        const types: Type[] = []

        if (a.or(b).intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (!b.isInteger() || b.intersects(Type.negativeFloat)) {
          types.push(Type.nan)
        }
        if (a.intersects(Type.positiveInfinity) && !b.intersects(Type.nan.or(Type.infinity))) {
          types.push(Type.positiveInfinity)
        }
        if (a.intersects(Type.negativeInfinity) && !b.intersects(Type.nan.or(Type.infinity))) {
          types.push(Type.negativeInfinity)
        }
        if (a.intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (b.intersects(Type.nan.or(Type.infinity))) {
          types.push(Type.nan)
        }

        if (b.intersects(Type.nonNegativeInteger)) {
          if (a.intersects(Type.zero)) {
            types.push(Type.zero)
          }

          if (a.intersects(Type.positiveFloat)) {
            if (a.isInteger()) {
              types.push(Type.positiveInteger)
            } else {
              if (b.intersects(Type.zero)) {
                types.push(Type.nonNegativeInteger)
              }
              if (b.intersects(Type.nonZeroFloat)) {
                types.push(Type.nonNegativeFloat)
              }
            }
          }

          if (a.intersects(Type.negativeFloat)) {
            if (a.isInteger()) {
              types.push(Type.negativeInteger)
            } else {
              if (b.intersects(Type.zero)) {
                types.push(Type.nonPositiveInteger)
              }
              if (b.intersects(Type.nonZeroFloat)) {
                types.push(Type.nonPositiveFloat)
              }
            }
          }
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `round`, debugInfo),
  },

  trunc: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }

        return Math.trunc(first)
      } else {
        const a = Type.of(first)

        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (a.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }
        if (a.intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.zero)) {
          types.push(Type.zero)
        }
        if (a.intersects(Type.positiveFloat)) {
          if (a.isInteger()) {
            types.push(Type.positiveInteger)
          } else {
            types.push(Type.nonNegativeInteger)
          }
        }
        if (a.intersects(Type.negativeFloat)) {
          if (a.isInteger()) {
            types.push(Type.negativeInteger)
          } else {
            types.push(Type.nonPositiveInteger)
          }
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `trunc`, debugInfo),
  },

  floor: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }

        return Math.floor(first)
      } else {
        const a = Type.of(first)
        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (a.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }
        if (a.intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.zero)) {
          types.push(Type.zero)
        }
        if (a.intersects(Type.positiveFloat)) {
          if (a.isInteger()) {
            types.push(Type.positiveInteger)
          } else {
            types.push(Type.nonNegativeInteger)
          }
        }
        if (a.intersects(Type.negativeFloat)) {
          types.push(Type.negativeInteger)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `floor`, debugInfo),
  },

  ceil: {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }

        return Math.ceil(first)
      } else {
        const a = Type.of(first)
        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (a.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }
        if (a.intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.zero)) {
          types.push(Type.zero)
        }
        if (a.intersects(Type.positiveFloat)) {
          types.push(Type.positiveInteger)
        }
        if (a.intersects(Type.negativeFloat)) {
          if (a.isInteger()) {
            types.push(Type.negativeInteger)
          } else {
            types.push(Type.nonPositiveInteger)
          }
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `ceil`, debugInfo),
  },

  'rand!': {
    evaluate: (): number => {
      return Math.random()
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `rand!`, debugInfo),
  },

  'rand-int!': {
    evaluate: ([first]): number | Type => {
      if (Type.isNotType(first)) {
        if (!number.is(first)) {
          return NaN
        }

        return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first)
      } else {
        const a = Type.of(first)
        const types: Type[] = []

        if (a.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (a.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }
        if (a.intersects(Type.nan)) {
          types.push(Type.nan)
        }

        if (a.intersects(Type.zero)) {
          types.push(Type.positiveZero)
        }
        if (a.intersects(Type.positiveFloat)) {
          types.push(Type.nonNegativeInteger)
        }
        if (a.intersects(Type.negativeFloat)) {
          types.push(Type.nonPositiveInteger)
        }
        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `rand-int!`, debugInfo),
  },

  min: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [first, ...rest] = params
        if (!number.is(first)) {
          return NaN
        }

        if (rest.length === 0) {
          return first
        }

        return rest.reduce((min: number, value) => {
          if (!number.is(value)) {
            return NaN
          }

          return Math.min(min, value)
        }, first)
      } else {
        const paramTypes = params.map(Type.of)

        const combinedType = Type.or(...paramTypes)
        const hasNonNumber = combinedType.intersectsNonNumber()
        const hasNan = combinedType.intersects(Type.nan)

        const numberTypes = paramTypes.map(t => t.and(Type.number))

        // If an argument is nan (and nan only) or a non number
        if (numberTypes.some(t => t.is(Type.nan) || t.is(Type.never))) {
          return NaN
        }

        type SmallestMax = -2 | -1 | 0 | 1 | 2
        const smallestMax: SmallestMax = numberTypes.reduce((result: SmallestMax, t) => {
          const max: SmallestMax = t.intersects(Type.positiveInfinity)
            ? 2
            : t.intersects(Type.positiveFloat)
            ? 1
            : t.intersects(Type.zero)
            ? 0
            : t.intersects(Type.negativeFloat)
            ? -1
            : -2
          return Math.min(result, max) as SmallestMax
        }, 2)

        const exclude = Type.or(
          smallestMax < 2 ? Type.positiveInfinity : Type.never,
          smallestMax < 1 ? Type.positiveFloat : Type.never,
          smallestMax < 0 ? Type.zero : Type.never,
          smallestMax < -1 ? Type.negativeFloat : Type.never,
        )

        const result = Type.or(...numberTypes).exclude(exclude)

        return (hasNonNumber || hasNan ? result.or(Type.nan) : result).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `min`, debugInfo),
  },

  max: {
    evaluate: (params): number | Type => {
      if (params.every(Type.isNotType)) {
        const [first, ...rest] = params

        if (!number.is(first)) {
          return NaN
        }

        if (rest.length === 0) {
          return first
        }

        return rest.reduce((min: number, value) => {
          if (!number.is(value)) {
            return NaN
          }

          return Math.max(min, value)
        }, first)
      } else {
        const paramTypes = params.map(Type.of)

        const combinedType = Type.or(...paramTypes)
        const hasNonNumber = combinedType.intersectsNonNumber()
        const hasNan = combinedType.intersects(Type.nan)

        const numberTypes = paramTypes.map(t => t.and(Type.number))

        // If an argument is nan (and nan only) or a non number
        if (numberTypes.some(t => t.is(Type.nan) || t.is(Type.never))) {
          return NaN
        }

        type LargestMin = -2 | -1 | 0 | 1 | 2
        const largestMin: LargestMin = numberTypes.reduce((result: LargestMin, t) => {
          const min: LargestMin = t.intersects(Type.negativeInfinity)
            ? -2
            : t.intersects(Type.negativeFloat)
            ? -1
            : t.intersects(Type.zero)
            ? 0
            : t.intersects(Type.positiveFloat)
            ? 1
            : 2
          return Math.max(result, min) as LargestMin
        }, -2)

        const exclude = Type.or(
          largestMin > -2 ? Type.negativeInfinity : Type.never,
          largestMin > -1 ? Type.negativeFloat : Type.never,
          largestMin > 0 ? Type.zero : Type.never,
          largestMin > 1 ? Type.positiveFloat : Type.never,
        )

        const result = Type.or(...numberTypes).exclude(exclude)

        return (hasNonNumber || hasNan ? result.or(Type.nan) : result).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1 }, arity, `max`, debugInfo),
  },

  abs: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.abs(value)
      } else {
        const paramType = Type.of(value)
        const types: Type[] = []

        if (paramType.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        const numberType = paramType.and(Type.number)
        const absType = numberType.or(numberType.negateNumber()).exclude(Type.negativeFloat.or(Type.negativeInfinity))

        types.push(absType)

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `abs`, debugInfo),
  },

  sign: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.sign(value)
      } else {
        const paramType = Type.of(value)
        const types: Type[] = []

        if (paramType.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (paramType.intersects(Type.negativeNumber)) {
          types.push(Type.negativeInteger)
        }
        if (paramType.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (paramType.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }
        if (paramType.intersects(Type.positiveNumber)) {
          types.push(Type.positiveInteger)
        }

        const resultType = Type.or(...types)
        if (resultType.equals(Type.positiveInteger)) {
          return 1
        }
        if (resultType.equals(Type.negativeInteger)) {
          return -1
        }
        return resultType.toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `sign`, debugInfo),
  },

  'max-value': {
    evaluate: (): number => {
      return MAX_NUMBER
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `max-value`, debugInfo),
  },

  'min-value': {
    evaluate: (): number => {
      return MIN_NUMBER
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `min-value`, debugInfo),
  },

  infinity: {
    evaluate: (): number => {
      return Infinity
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `positive-infinity`, debugInfo),
  },

  nan: {
    evaluate: (): number => {
      return NaN
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `nan`, debugInfo),
  },

  e: {
    evaluate: (): number => {
      return Math.E
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `e`, debugInfo),
  },

  pi: {
    evaluate: (): number => {
      return Math.PI
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(0, arity, `pi`, debugInfo),
  },

  exp: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.exp(value)
      } else {
        const paramType = Type.of(value)

        if (paramType.is(Type.zero)) {
          return 1
        }

        const types: Type[] = []

        if (paramType.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (paramType.intersects(Type.negativeInfinity)) {
          types.push(Type.positiveZero)
        }
        if (paramType.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (paramType.intersects(Type.negativeFloat)) {
          types.push(Type.nonNegativeFloat)
        }
        if (paramType.intersects(Type.zero)) {
          types.push(Type.positiveInteger)
        }
        if (paramType.intersects(Type.positiveFloat)) {
          types.push(Type.positiveNumber)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `exp`, debugInfo),
  },

  log: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.log(value)
      } else {
        return evaluateLogType(Type.of(value))
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `log`, debugInfo),
  },

  log2: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.log2(value)
      } else {
        return evaluateLogType(Type.of(value))
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `log2`, debugInfo),
  },

  log10: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.log10(value)
      } else {
        return evaluateLogType(Type.of(value))
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `log10`, debugInfo),
  },

  sin: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.sin(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }
        if (type.intersects(Type.nonZeroFloat)) {
          types.push(Type.nonZeroFloat) // Math.PI only close to real pi, hence never 0
        }
        if (type.intersects(Type.infinity)) {
          types.push(Type.nan)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `sin`, debugInfo),
  },

  asin: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.asin(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }
        if (type.intersects(Type.nonZeroNumber)) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.positiveFloat)
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.negativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `asin`, debugInfo),
  },

  sinh: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.sinh(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }
        if (type.intersects(Type.positiveNumber)) {
          types.push(Type.positiveInfinity)
        }
        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.positiveFloat)
        }
        if (type.intersects(Type.negativeNumber)) {
          types.push(Type.negativeInfinity)
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.negativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `sinh`, debugInfo),
  },

  asinh: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.asinh(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (type.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInfinity)
        }

        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }

        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.positiveFloat)
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.negativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `asinh`, debugInfo),
  },

  cos: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.cos(value)
      } else {
        const type = Type.of(value)

        if (type.is(Type.zero)) {
          return 1
        }

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.float)) {
          types.push(Type.nonZeroFloat) // Math.PI only close to real pi, hence never 0
        }
        if (type.intersects(Type.infinity)) {
          types.push(Type.nan)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `cos`, debugInfo),
  },

  acos: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.acos(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.zero)) {
          types.push(Type.positiveFloat)
        }
        if (type.intersects(Type.nonZeroNumber)) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.nonNegativeFloat)
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.positiveFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `acos`, debugInfo),
  },

  cosh: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.cosh(value)
      } else {
        const type = Type.of(value)

        if (type.is(Type.zero)) {
          return 1
        }

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.zero)) {
          types.push(Type.positiveInteger)
        }
        if (type.intersects(Type.nonZeroNumber)) {
          types.push(Type.positiveInfinity)
        }
        if (type.intersects(Type.nonZeroFloat)) {
          types.push(Type.positiveFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `cosh`, debugInfo),
  },

  acosh: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.acosh(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInfinity)
        }
        if (type.intersects(Type.negativeInfinity)) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.zero)) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.nonNegativeFloat)
          if (!type.isInteger()) {
            types.push(Type.nan)
          }
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.nan)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `acosh`, debugInfo),
  },

  tan: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.tan(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }
        if (type.intersects(Type.nonZeroFloat)) {
          types.push(Type.nonZeroFloat) // Math.PI only close to real pi, hence never 0
        }
        if (type.intersects(Type.infinity)) {
          types.push(Type.nan)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `tan`, debugInfo),
  },

  atan: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.atan(value)
      } else {
        const type = Type.of(value)

        if (type.equals(Type.positiveInfinity)) {
          return Math.PI / 2
        }

        if (type.equals(Type.negativeInfinity)) {
          return -Math.PI / 2
        }

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }

        if (type.intersects(Type.positiveNumber)) {
          types.push(Type.positiveFloat)
        }
        if (type.intersects(Type.negativeNumber)) {
          types.push(Type.negativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `atan`, debugInfo),
  },

  tanh: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.tanh(value)
      } else {
        const type = Type.of(value)

        if (type.equals(Type.positiveInfinity)) {
          return 1
        }
        if (type.equals(Type.negativeInfinity)) {
          return -1
        }

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }
        if (type.intersects(Type.positiveInfinity)) {
          types.push(Type.positiveInteger)
        }
        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.positiveFloat)
        }
        if (type.intersects(Type.negativeInfinity)) {
          types.push(Type.negativeInteger)
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.negativeFloat)
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `tanh`, debugInfo),
  },

  atanh: {
    evaluate: ([value]): number | Type => {
      if (Type.isNotType(value)) {
        if (!number.is(value)) {
          return NaN
        }

        return Math.atanh(value)
      } else {
        const type = Type.of(value)

        const types: Type[] = []
        if (type.intersectsNonNumber()) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveInfinity)) {
          types.push(Type.nan)
        }
        if (type.intersects(Type.negativeInfinity)) {
          types.push(Type.nan)
        }

        if (type.intersects(Type.positiveZero)) {
          types.push(Type.positiveZero)
        }
        if (type.intersects(Type.negativeZero)) {
          types.push(Type.negativeZero)
        }

        if (type.intersects(Type.positiveFloat)) {
          types.push(Type.nan)
          types.push(Type.positiveInfinity)
          if (!type.isInteger()) {
            types.push(Type.positiveFloat)
          }
        }
        if (type.intersects(Type.negativeFloat)) {
          types.push(Type.nan)
          types.push(Type.negativeInfinity)
          if (!type.isInteger()) {
            types.push(Type.negativeFloat)
          }
        }

        return Type.or(...types).toNumberValue()
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `atanh`, debugInfo),
  },
}

function getTypeOfSum(paramTypes: Arr, debugInfo: DebugInfo | undefined): Type | number {
  paramTypes.sort((a, b) => (Type.isType(a) ? 1 : Type.isType(b) ? -1 : 0))
  const first = paramTypes[0]
  any.assert(first)
  if (paramTypes.length === 1) {
    Type.assertType(first, debugInfo)
    return first.toNumberValue()
  }
  const rest = paramTypes.slice(1)
  const sum = rest.reduce((a: unknown | number, b) => getTypeOfBinarySum(a, b), first)

  if (Type.isType(sum)) {
    return sum.toNumberValue()
  }
  number.assert(sum, debugInfo)
  return sum
}

function getTypeOfBinarySum(a: unknown, b: unknown): Type | number {
  any.assert(a)
  any.assert(b)

  const aVal = Type.toValue(a)
  const bVal = Type.toValue(b)

  if (number.is(aVal) && number.is(bVal)) {
    return aVal + bVal
  }

  const aType = Type.of(a)
  const bType = Type.of(b)

  if (aType.is(Type.zero) && number.is(bVal) && bVal !== 0) {
    return bVal
  }

  if (number.is(aVal) && aVal !== 0 && bType.is(Type.zero)) {
    return aVal
  }

  const types: Type[] = []

  if (aType.or(bType).intersectsNonNumber()) {
    types.push(Type.nan)
  }

  if (aType.intersects(Type.positiveInfinity)) {
    if (bType.intersects(Type.positiveInfinity)) {
      types.push(Type.positiveInfinity)
    }
    if (bType.intersects(Type.negativeInfinity)) {
      types.push(Type.nan)
    }
    if (bType.intersects(Type.float)) {
      types.push(Type.positiveInfinity)
    }
  }
  if (aType.intersects(Type.negativeInfinity)) {
    if (bType.intersects(Type.positiveInfinity)) {
      types.push(Type.nan)
    }
    if (bType.intersects(Type.negativeInfinity)) {
      types.push(Type.negativeInfinity)
    }
    if (bType.intersects(Type.float)) {
      types.push(Type.negativeInfinity)
    }
  }
  if (bType.intersects(Type.positiveInfinity)) {
    if (aType.intersects(Type.float)) {
      types.push(Type.positiveInfinity)
    }
  }
  if (bType.intersects(Type.negativeInfinity)) {
    if (aType.intersects(Type.float)) {
      types.push(Type.negativeInfinity)
    }
  }

  const baseType = aType.isInteger() && bType.isInteger() ? Type.integer : Type.float

  if (aType.intersects(Type.positiveFloat)) {
    if (bType.intersects(Type.zero)) {
      types.push(Type.positiveFloat.and(baseType))
    }
    if (bType.intersects(Type.positiveFloat)) {
      types.push(Type.positiveFloat.and(baseType))
      types.push(Type.positiveInfinity)
    }
    if (bType.intersects(Type.negativeFloat)) {
      types.push(Type.float.and(baseType))
    }
  }

  if (aType.intersects(Type.negativeFloat)) {
    if (bType.intersects(Type.zero)) {
      types.push(Type.negativeFloat.and(baseType))
    }
    if (bType.intersects(Type.positiveFloat)) {
      types.push(Type.float.and(baseType))
    }
    if (bType.intersects(Type.negativeFloat)) {
      types.push(Type.negativeFloat.and(baseType))
      types.push(Type.negativeInfinity)
    }
  }

  if (aType.intersects(Type.zero)) {
    if (bType.intersects(Type.zero)) {
      if (aType.intersects(Type.positiveZero) && bType.intersects(Type.positiveZero)) {
        types.push(Type.positiveZero)
      }
      if (aType.intersects(Type.positiveZero) && bType.intersects(Type.negativeZero)) {
        types.push(Type.positiveZero)
      }
      if (aType.intersects(Type.negativeZero) && bType.intersects(Type.positiveZero)) {
        types.push(Type.positiveZero)
      }
      if (aType.intersects(Type.negativeZero) && bType.intersects(Type.negativeZero)) {
        types.push(Type.negativeZero)
      }
    }
    if (bType.intersects(Type.positiveFloat)) {
      types.push(Type.positiveFloat.and(baseType))
    }
    if (bType.intersects(Type.negativeFloat)) {
      types.push(Type.negativeFloat.and(baseType))
    }
  }

  return Type.or(...types)
}

function getTypeOfProduct(paramTypes: Type[]): Type | number {
  const first = asValue(paramTypes[0])
  if (paramTypes.length === 1) {
    return first.toNumberValue()
  }

  return paramTypes
    .slice(1)
    .reduce((a: Type, b) => getTypeOfBinaryProduct(a, b), first)
    .toNumberValue()
}

function getTypeOfBinaryProduct(a: Type, b: Type): Type {
  const types: Type[] = []
  if (a.or(b).intersectsNonNumber()) {
    types.push(Type.nan)
  }

  if (a.intersects(Type.infinity) && b.intersects(Type.zero)) {
    types.push(Type.nan)
  }
  if (b.intersects(Type.infinity) && a.intersects(Type.zero)) {
    types.push(Type.nan)
  }

  if (a.intersects(Type.positiveInfinity)) {
    if (b.intersects(Type.positiveInfinity) || b.intersects(Type.positiveFloat)) {
      types.push(Type.positiveInfinity)
    }
    if (b.intersects(Type.negativeInfinity) || b.intersects(Type.negativeFloat)) {
      types.push(Type.negativeInfinity)
    }
  }

  if (a.intersects(Type.negativeInfinity)) {
    if (b.intersects(Type.negativeInfinity) || b.intersects(Type.negativeFloat)) {
      types.push(Type.positiveInfinity)
    }
    if (b.intersects(Type.positiveInfinity) || b.intersects(Type.positiveFloat)) {
      types.push(Type.negativeInfinity)
    }
  }

  if (b.intersects(Type.positiveInfinity)) {
    if (a.intersects(Type.positiveFloat)) {
      types.push(Type.positiveInfinity)
    }
    if (a.intersects(Type.negativeFloat)) {
      types.push(Type.negativeInfinity)
    }
  }

  if (b.intersects(Type.negativeInfinity)) {
    if (a.intersects(Type.positiveFloat)) {
      types.push(Type.negativeInfinity)
    }
    if (a.intersects(Type.negativeFloat)) {
      types.push(Type.positiveInfinity)
    }
  }

  if (a.intersects(Type.positiveZero)) {
    if (b.intersects(Type.positiveZero) || b.intersects(Type.positiveFloat)) {
      types.push(Type.positiveZero)
    }
    if (b.intersects(Type.negativeZero) || b.intersects(Type.negativeFloat)) {
      types.push(Type.negativeZero)
    }
  }

  if (a.intersects(Type.negativeZero)) {
    if (b.intersects(Type.positiveZero) || b.intersects(Type.positiveFloat)) {
      types.push(Type.negativeZero)
    }
    if (b.intersects(Type.negativeZero) || b.intersects(Type.negativeFloat)) {
      types.push(Type.positiveZero)
    }
  }

  if (b.intersects(Type.positiveZero)) {
    if (a.intersects(Type.positiveFloat)) {
      types.push(Type.positiveZero)
    }
    if (a.intersects(Type.negativeFloat)) {
      types.push(Type.negativeZero)
    }
  }

  if (b.intersects(Type.negativeZero)) {
    if (a.intersects(Type.positiveFloat)) {
      types.push(Type.negativeZero)
    }
    if (a.intersects(Type.negativeFloat)) {
      types.push(Type.positiveZero)
    }
  }

  const baseType = a.isInteger() && b.isInteger() ? Type.integer : Type.float

  const aNeg = a.intersects(Type.negativeFloat)
  const aPos = a.intersects(Type.positiveFloat)
  const bNeg = b.intersects(Type.negativeFloat)
  const bPos = b.intersects(Type.positiveFloat)

  if ((aNeg && bNeg) || (aPos && bPos)) {
    types.push(Type.positiveFloat.and(baseType))
    types.push(Type.positiveInfinity)
  }

  if ((aNeg && bPos) || (aPos && bNeg)) {
    types.push(Type.negativeFloat.and(baseType))
    types.push(Type.negativeInfinity)
  }

  return Type.or(...types)
}

function getTypeOfDivision(paramTypes: Type[]): Type {
  if (paramTypes.length === 1) {
    paramTypes.unshift(Type.positiveInteger)
  }

  const first = asValue(paramTypes[0])
  const nanType = first.exclude(Type.nan).intersectsNonNumber() ? Type.nan : Type.never

  return paramTypes
    .slice(1)
    .reduce((a: Type, b) => getTypeOfBinaryDivision(a, b), first)
    .or(nanType)
}

function getTypeOfBinaryDivision(a: Type, b: Type): Type {
  const types: Type[] = []

  if (a.intersectsNonNumber() || b.intersectsNonNumber()) {
    types.push(Type.nan)
  }

  if (a.intersects(Type.zero) && b.intersects(Type.zero)) {
    types.push(Type.nan)
  }

  if (a.intersects(Type.positiveZero)) {
    if (b.intersects(Type.positiveNumber)) {
      types.push(Type.positiveZero)
    }
    if (b.intersects(Type.negativeNumber)) {
      types.push(Type.negativeZero)
    }
  }
  if (a.intersects(Type.negativeZero)) {
    if (b.intersects(Type.positiveNumber)) {
      types.push(Type.negativeZero)
    }
    if (b.intersects(Type.negativeNumber)) {
      types.push(Type.positiveZero)
    }
  }

  if (b.intersects(Type.positiveZero)) {
    if (a.intersects(Type.positiveFloat)) {
      types.push(Type.positiveInfinity)
    }
    if (a.intersects(Type.negativeFloat)) {
      types.push(Type.negativeInfinity)
    }
  }

  if (b.intersects(Type.negativeZero)) {
    if (a.intersects(Type.positiveFloat)) {
      types.push(Type.negativeInfinity)
    }
    if (a.intersects(Type.negativeFloat)) {
      types.push(Type.positiveInfinity)
    }
  }

  if (a.intersects(Type.positiveInfinity)) {
    if (b.intersects(Type.positiveZero) || b.intersects(Type.positiveFloat)) {
      types.push(Type.positiveInfinity)
    }
    if (b.intersects(Type.negativeZero) || b.intersects(Type.negativeFloat)) {
      types.push(Type.negativeInfinity)
    }
  }

  if (a.intersects(Type.negativeInfinity)) {
    if (b.intersects(Type.positiveZero) || b.intersects(Type.positiveFloat)) {
      types.push(Type.negativeInfinity)
    }
    if (b.intersects(Type.negativeZero) || b.intersects(Type.negativeFloat)) {
      types.push(Type.positiveInfinity)
    }
  }

  if (b.intersects(Type.positiveInfinity)) {
    if (a.intersects(Type.positiveNumber) || a.intersects(Type.positiveZero)) {
      types.push(Type.positiveZero)
    }
    if (a.intersects(Type.negativeNumber) || a.intersects(Type.positiveZero)) {
      types.push(Type.negativeZero)
    }
  }
  if (b.intersects(Type.negativeInfinity)) {
    if (a.intersects(Type.positiveNumber) || a.intersects(Type.positiveZero)) {
      types.push(Type.negativeZero)
    }
    if (a.intersects(Type.negativeNumber) || a.intersects(Type.positiveZero)) {
      types.push(Type.positiveZero)
    }
  }

  const aNeg = a.intersects(Type.negativeFloat)
  const aPos = a.intersects(Type.positiveFloat)
  const bNeg = b.intersects(Type.negativeFloat)
  const bPos = b.intersects(Type.positiveFloat)

  if ((aNeg && bNeg) || (aPos && bPos)) {
    types.push(Type.positiveFloat)
    if (!b.isInteger()) {
      types.push(Type.positiveInfinity)
    }
  }

  if ((aNeg && bPos) || (aPos && bNeg)) {
    types.push(Type.negativeFloat)
    if (!b.isInteger()) {
      types.push(Type.negativeInfinity)
    }
  }

  return Type.or(...types)
}

function negate(value: unknown): number | Type {
  return Type.isType(value) ? value.negateNumber().toNumberValue() : number.is(value) ? -value : NaN
}

function evaluateLogType(paramType: Type): number | Type {
  const types: Type[] = []

  if (paramType.intersectsNonNumber()) {
    types.push(Type.nan)
  }

  if (paramType.intersects(Type.negativeNumber)) {
    types.push(Type.nan)
  }

  if (paramType.intersects(Type.zero)) {
    types.push(Type.negativeInfinity)
  }
  if (paramType.intersects(Type.positiveFloat)) {
    types.push(Type.float)
  }
  if (paramType.intersects(Type.positiveInfinity)) {
    types.push(Type.positiveInfinity)
  }

  return Type.or(...types).toNumberValue()
}
