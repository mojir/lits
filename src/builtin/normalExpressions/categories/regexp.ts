import { assertLength, assertRegExp, assertString } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: (params, meta): RegExp => {
      const [first, second] = params
      assertString(first, meta)

      if (params.length === 1) {
        return new RegExp(first)
      }

      assertString(second, meta)
      return new RegExp(first, second)
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  match: {
    evaluate: ([first, second], meta): string[] | null => {
      assertRegExp(first, meta)
      assertString(second, meta)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return null
    },
    validate: node => assertLength(2, node),
  },
  replace: {
    evaluate: ([string, regexp, value], meta): string => {
      assertString(string, meta)
      assertRegExp(regexp, meta)
      assertString(value, meta)

      return string.replace(regexp, value)
    },
    validate: node => assertLength(3, node),
  },
}
