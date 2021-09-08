import { assertLengthOneOrTwo, assertLengthTwo, assertRegExp, assertString } from '../../utils'
import { BuiltinNormalExpressions } from './interface'

export const regexp: BuiltinNormalExpressions = {
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
    validate: ({ params }) => assertLengthOneOrTwo(params),
  },
  match: {
    evaluate: ([first, second]: unknown[]): RegExpExecArray | undefined => {
      assertRegExp(first)
      assertString(second)

      return first.exec(second) ?? undefined
    },
    validate: ({ params }) => assertLengthTwo(params),
  },
}
