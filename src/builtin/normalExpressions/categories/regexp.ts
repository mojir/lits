import { Type } from '../../../types/Type'
import { REGEXP_SYMBOL, RegularExpression } from '../../../parser/interface'
import { assertNumberOfParams, regularExpression, string } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: (params, debugInfo): RegularExpression | Type => {
      if (params.every(Type.isNotType)) {
        const [sourceArg, flagsArg] = params

        string.assert(sourceArg, debugInfo)
        const source = sourceArg || `(?:)`
        const flags = string.is(flagsArg) ? flagsArg : ``
        new RegExp(source, flags) // Throws if invalid regexp
        return {
          [REGEXP_SYMBOL]: true,
          debugInfo,
          source,
          flags,
        }
      } else {
        return Type.regexp
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `regexp`, debugInfo),
  },
  match: {
    evaluate: ([regexp, text], debugInfo): string[] | null => {
      regularExpression.assert(regexp, debugInfo)
      string.assert(text, debugInfo)
      const regExp = new RegExp(regexp.source, regexp.flags)

      const match = regExp.exec(text)
      if (match) {
        return [...match]
      }
      return null
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `match`, debugInfo),
  },
  replace: {
    evaluate: ([str, regexp, value], debugInfo): string => {
      string.assert(str, debugInfo)
      regularExpression.assert(regexp, debugInfo)
      string.assert(value, debugInfo)

      const regExp = new RegExp(regexp.source, regexp.flags)
      return str.replace(regExp, value)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(3, arity, `replace`, debugInfo),
  },
}
