import { LitsError } from '../../errors'
import type { RegularExpression } from '../../parser/types'
import { assertRegularExpression, assertStringOrRegularExpression, isRegularExpression } from '../../typeGuards/lits'
import { assertString, isString } from '../../typeGuards/string'
import { toFixedArity } from '../../utils/arity'
import { REGEXP_SYMBOL } from '../../utils/symbols'
import type { BuiltinNormalExpressions } from '../interface'

export const regexpNormalExpression: BuiltinNormalExpressions = {
  'regexp': {
    evaluate: ([sourceArg, flagsArg], sourceCodeInfo): RegularExpression => {
      assertString(sourceArg, sourceCodeInfo)
      const source = sourceArg || '(?:)'
      const flags = typeof flagsArg === 'string' ? flagsArg : ''

      try {
        // eslint-disable-next-line no-new
        new RegExp(source, flags) // Throws if invalid regexp
      }
      catch (e) {
        throw new LitsError(`Invalid regular expression: ${source} ${flags}`, sourceCodeInfo)
      }
      return {
        [REGEXP_SYMBOL]: true,
        sourceCodeInfo,
        s: source,
        f: flags,
      }
    },
    arity: { min: 1, max: 2 },
    docs: {
      category: 'regular-expression',
      returns: { type: 'regexp' },
      args: {
        pattern: { type: 'string' },
        flags: { type: 'string', description: 'Optional flags for the regular expression. Possible values are the same as Javascript RegExp takes.' },
      },
      variants: [
        { argumentNames: ['pattern'] },
        { argumentNames: ['pattern', 'flags'] },
      ],
      description: 'Creates a RegExp from $pattern and $flags.',
      examples: [
        'regexp("^\\s*(.*)$")',
        '#"^\\s*(.*)$"',
        'regexp("albert", "ig")',
        '#"albert"ig',
      ],
      seeAlso: ['-short-regexp', 're-match', 'replace', 'replace-all', 'regexp?'],
      hideOperatorForm: true,
    },
  },
  're-match': {
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
    arity: toFixedArity(2),
    docs: {
      category: 'regular-expression',
      returns: { type: 'any', array: true },
      args: {
        a: { type: 'regexp' },
        b: { type: 'string' },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: `Matches $b against regular expression $a.
If $b is a string and matches the regular expression, a \`re-match\`-array is returned, otherwise \`null\` is returned.`,
      seeAlso: ['regexp', 'replace', 'replace-all', '-short-regexp', 'regexp?'],
      examples: [
        're-match("  A string", regexp("^\\\\s*(.*)$"))',
        're-match("  A string", #"^\\s*(.*)$")',
        're-match("My name is Albert", #"albert"i)',
        're-match("My name is Ben", #"albert"i)',
        're-match(null, #"albert"i)',
        're-match(1, #"albert"i)',
        're-match({}, #"albert"i)',
      ],
    },
  },
  'replace': {
    evaluate: ([str, regexp, value], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertStringOrRegularExpression(regexp, sourceCodeInfo)
      assertString(value, sourceCodeInfo)

      const matcher = isRegularExpression(regexp) ? new RegExp(regexp.s, `${regexp.f}`) : regexp
      return str.replace(matcher, value)
    },
    arity: toFixedArity(3),
    docs: {
      category: 'regular-expression',
      returns: { type: 'any', array: true },
      args: {
        a: { type: 'string' },
        b: { type: ['regexp', 'string'] },
        x: { type: 'string' },
      },
      variants: [{ argumentNames: ['a', 'b', 'x'] }],
      description: 'Returns a new string with first match of regular expression $b replaced by $x.',
      seeAlso: ['replace-all', 'regexp', 're-match', '-short-regexp'],
      examples: [
        'replace("Duck duck", "u", "i")',
        'replace("Duck duck", #"u", "i")',
        'replace("abcABC", regexp("a", "i"), "-")',
        'replace("abcABC", regexp("a", "gi"), "-")',
        'replace("abcABC", #"a"i, "-")',
        'replace("abcABC", #"a"gi, "-")',
      ],
    },
  },
  'replace-all': {
    evaluate: ([str, regexp, value], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertStringOrRegularExpression(regexp, sourceCodeInfo)
      assertString(value, sourceCodeInfo)
      const matcher = isRegularExpression(regexp) ? new RegExp(regexp.s, `${regexp.f.includes('g') ? regexp.f : `${regexp.f}g`}`) : regexp
      return str.replaceAll(matcher, value)
    },
    arity: toFixedArity(3),
    docs: {
      category: 'regular-expression',
      returns: { type: 'any', array: true },
      args: {
        a: { type: 'string' },
        b: { type: ['regexp', 'string'] },
        x: { type: 'string' },
      },
      variants: [{ argumentNames: ['a', 'b', 'x'] }],
      description: 'Returns a new string with all matches of regular expression $b replaced by $x.',
      seeAlso: ['replace', 'regexp', 're-match', '-short-regexp'],
      examples: [
        'replace-all("Duck duck", "u", "i")',
        'replace-all("Duck duck", regexp("u"), "i")',
        'replace-all("abcABC", regexp("a", "i"), "-")',
        'replace-all("abcABC", regexp("a", "gi"), "-")',
        'replace-all("abcABC", #"a"i, "-")',
        'replace-all("abcABC", #"a"gi, "-")',
      ],
    },
  },
}
