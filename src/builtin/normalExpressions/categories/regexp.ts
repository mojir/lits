import { REGEXP_SYMBOL, RegularExpression } from '../../../parser/interface'
import { assertNumberOfParams, regularExpression, string } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: ([sourceArg, flagsArg], debugInfo): RegularExpression => {
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
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
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
    validate: node => assertNumberOfParams(2, node),
  },
  replace: {
    evaluate: ([str, regexp, value], debugInfo): string => {
      string.assert(str, debugInfo)
      regularExpression.assert(regexp, debugInfo)
      string.assert(value, debugInfo)

      const regExp = new RegExp(regexp.source, regexp.flags)
      return str.replace(regExp, value)
    },
    validate: node => assertNumberOfParams(3, node),
  },
}
