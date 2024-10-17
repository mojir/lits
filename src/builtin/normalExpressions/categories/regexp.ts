import type { RegularExpression } from '../../../parser/interface'
import { assertRegularExpression } from '../../../typeGuards/lits'
import { assertString, isString } from '../../../typeGuards/string'
import { assertNumberOfParams } from '../../../typeGuards'
import { REGEXP_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  regexp: {
    evaluate: ([sourceArg, flagsArg], sourceCodeInfo): RegularExpression => {
      assertString(sourceArg, sourceCodeInfo)
      const source = sourceArg || '(?:)'
      const flags = typeof flagsArg === 'string' ? flagsArg : ''
      // eslint-disable-next-line no-new
      new RegExp(source, flags) // Throws if invalid regexp
      return {
        [REGEXP_SYMBOL]: true,
        sourceCodeInfo,
        s: source,
        f: flags,
      }
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  match: {
    evaluate: ([regexp, text], sourceCodeInfo): string[] | null => {
      assertRegularExpression(regexp, sourceCodeInfo)
      if (!isString(text))
        return null

      const regExp = new RegExp(regexp.s, regexp.f)

      const match = regExp.exec(text)
      if (match)
        return [...match]

      return null
    },
    validate: node => assertNumberOfParams(2, node),
  },
  replace: {
    evaluate: ([str, regexp, value], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertRegularExpression(regexp, sourceCodeInfo)
      assertString(value, sourceCodeInfo)

      const regExp = new RegExp(regexp.s, regexp.f)
      return str.replace(regExp, value)
    },
    validate: node => assertNumberOfParams(3, node),
  },
}
