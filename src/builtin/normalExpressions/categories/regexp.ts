import { assertLength, assertRegExp, assertString } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: (params, sourceCodeInfo): RegExp => {
      const [first, second] = params
      assertString(first, sourceCodeInfo)

      if (params.length === 1) {
        return new RegExp(first)
      }

      assertString(second, sourceCodeInfo)
      return new RegExp(first, second)
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  match: {
    evaluate: ([first, second], sourceCodeInfo): string[] | null => {
      assertRegExp(first, sourceCodeInfo)
      assertString(second, sourceCodeInfo)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return null
    },
    validate: node => assertLength(2, node),
  },
  replace: {
    evaluate: ([string, regexp, value], sourceCodeInfo): string => {
      assertString(string, sourceCodeInfo)
      assertRegExp(regexp, sourceCodeInfo)
      assertString(value, sourceCodeInfo)

      return string.replace(regexp, value)
    },
    validate: node => assertLength(3, node),
  },
}
