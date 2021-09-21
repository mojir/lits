import { assertInteger, assertLength } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  lognot: {
    evaluate: ([int32]: unknown[]): unknown => {
      assertInteger(int32)
      return ~int32
    },
    validate: ({ params }) => assertLength(1, params),
  },
  logand: {
    evaluate: (params: unknown[]): unknown => {
      const [first, ...rest] = params
      if (params.length === 0) {
        return -1
      }
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result & value
      }, first)
    },
  },
  logor: {
    evaluate: (params: unknown[]): unknown => {
      const [first, ...rest] = params
      if (params.length === 0) {
        return 0
      }
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result | value
      }, first)
    },
  },
  logxor: {
    evaluate: ([first, second]: unknown[]): unknown => {
      assertInteger(first)
      assertInteger(second)
      return first ^ second
    },
    validate: ({ params }) => assertLength(2, params),
  },
}
