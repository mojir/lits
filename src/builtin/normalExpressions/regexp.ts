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
    evaluate: ([first, second]: unknown[]): string[] | undefined => {
      assertRegExp(first)
      assertString(second)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return undefined
    },
    validate: ({ params }) => assertLengthTwo(params),
  },
}
