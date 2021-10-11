import { Arr } from '../../../interface'
import { assertLength, assertRegExp, assertString } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: (params: Arr): RegExp => {
      const [first, second] = params
      assertString(first)

      if (params.length === 1) {
        return new RegExp(first)
      }

      assertString(second)
      return new RegExp(first, second)
    },
    validate: node => assertLength({ min: 1, max: 2 }, node),
  },
  match: {
    evaluate: ([first, second]: Arr): string[] | undefined => {
      assertRegExp(first)
      assertString(second)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return undefined
    },
    validate: node => assertLength(2, node),
  },
  replace: {
    evaluate: ([string, regexp, value]: Arr): string => {
      assertString(string)
      assertRegExp(regexp)
      assertString(value)

      return string.replace(regexp, value)
    },
    validate: node => assertLength(3, node),
  },
}
