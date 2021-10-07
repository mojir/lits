import { assertCollection, assertLength } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
export const collectionNormalExpression: BuiltinNormalExpressions = {
  count: {
    evaluate: ([first]: unknown[]): unknown => {
      assertCollection(first)
      if (Array.isArray(first)) {
        return first.length
      }
      return Object.keys(first).length
    },
    validate: node => assertLength(1, node),
  },
}
