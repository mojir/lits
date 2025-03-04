import type { RegularExpression } from '../../../parser/interface'
import { assertRegularExpression, assertStringOrRegularExpression, isRegularExpression } from '../../../typeGuards/lits'
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
    evaluate: ([text, regexp], sourceCodeInfo): string[] | null => {
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
      assertStringOrRegularExpression(regexp, sourceCodeInfo)
      assertString(value, sourceCodeInfo)

      const matcher = isRegularExpression(regexp) ? new RegExp(regexp.s, `${regexp.f}`) : regexp
      return str.replace(matcher, value)
    },
    validate: node => assertNumberOfParams(3, node),
  },
  replace_all: {
    evaluate: ([str, regexp, value], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertStringOrRegularExpression(regexp, sourceCodeInfo)
      assertString(value, sourceCodeInfo)
      const matcher = isRegularExpression(regexp) ? new RegExp(regexp.s, `${regexp.f.includes('g') ? regexp.f : `${regexp.f}g`}`) : regexp

      return str.replaceAll(matcher, value)
    },
    validate: node => assertNumberOfParams(3, node),
  },
}
