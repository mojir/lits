import { assertNumberOfParams, regExp, string } from '../../../utils/assertion'
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
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  match: {
    evaluate: ([first, second], sourceCodeInfo): string[] | null => {
      regExp.assert(first, sourceCodeInfo)
      string.assert(second, sourceCodeInfo)

      const match = first.exec(second)
      if (match) {
        return [...match]
      }
      return null
    },
    validate: node => assertNumberOfParams(2, node),
  },
  replace: {
    evaluate: ([str, regexp, value], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      regExp.assert(regexp, sourceCodeInfo)
      string.assert(value, sourceCodeInfo)

      return str.replace(regexp, value)
    },
    validate: node => assertNumberOfParams(3, node),
  },
}
