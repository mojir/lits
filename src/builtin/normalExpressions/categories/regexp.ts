import { assertLength, assertRegExp } from '../../../utils'
import { string } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: (params, sourceCodeInfo): RegExp => {
      const [first, second] = params
      string.assert(first, sourceCodeInfo)

      if (params.length === 1) {
        return new RegExp(first)
      }

      string.assert(second, sourceCodeInfo)
      return new RegExp(first, second)
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  match: {
    evaluate: ([first, second], sourceCodeInfo): string[] | null => {
      assertRegExp(first, sourceCodeInfo)
      string.assert(second, sourceCodeInfo)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return null
    },
    validate: node => assertLength(2, node),
  },
  replace: {
    evaluate: ([str, regexp, value], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      assertRegExp(regexp, sourceCodeInfo)
      string.assert(value, sourceCodeInfo)

      return str.replace(regexp, value)
    },
    validate: node => assertLength(3, node),
  },
}
