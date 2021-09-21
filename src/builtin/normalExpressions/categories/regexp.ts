import { assertLength, assertRegExp, assertString } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: (params: unknown[]): RegExp => {
      const [first, second] = params
      assertString(first)

      if (params.length === 1) {
        return new RegExp(first)
      }

      assertString(second)
      return new RegExp(first, second)
    },
    validate: ({ params }) => assertLength({ min: 1, max: 2 }, params),
  },
  match: {
    evaluate: ([first, second]: unknown[]): string[] | undefined => {
      assertRegExp(first)
      assertString(second)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return undefined
    },
    validate: ({ params }) => assertLength(2, params),
  },
  replace: {
    evaluate: ([string, regexp, value]: unknown[]): string => {
      assertString(string)
      assertRegExp(regexp)
      assertString(value)

      return string.replace(regexp, value)
    },
    validate: ({ params }) => assertLength(3, params),
  },
}
