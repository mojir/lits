import type { FunctionReference } from '..'
import { type RegularExpressionApiName, getOperatorArgs } from '../api'

export const regularExpressionReference: Record<RegularExpressionApiName, FunctionReference<'Regular expression'>> = {
  'regexp': {
    title: 'regexp',
    category: 'Regular expression',
    linkName: 'regexp',
    returns: {
      type: 'regexp',
    },
    args: {
      pattern: {
        type: 'string',
      },
      flags: {
        type: 'string',
        description: 'Optional flags for the regular expression. Possible values are the same as Javascript RegExp takes.',
      },
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
    noOperatorDocumentation: true,
  },
  'match': {
    title: 'match',
    category: 'Regular expression',
    linkName: 'match',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      ...getOperatorArgs('regexp', 'string'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: `Matches $b against regular expression $a.
If $b is a string and matches the regular expression, a \`match\`-array is returned, otherwise \`null\` is returned.`,
    examples: [
      'match("  A string", regexp("^\\\\s*(.*)$"))',
      'match("  A string", #"^\\s*(.*)$")',
      'match("My name is Albert", #"albert"i)',
      'match("My name is Ben", #"albert"i)',
      'match(null, #"albert"i)',
      'match(1, #"albert"i)',
      'match({}, #"albert"i)',
    ],
  },
  'replace': {
    title: 'replace',
    category: 'Regular expression',
    linkName: 'replace',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      ...getOperatorArgs('string', ['regexp', 'string']),
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['a', 'b', 'x'] },
    ],
    description: 'Returns a new string with first match of regular expression $b replaced by $x.',
    examples: [
      'replace("Duck duck", "u", "i")',
      'replace("Duck duck", #"u", "i")',
      'replace("abcABC", regexp("a", "i"), "-")',
      'replace("abcABC", regexp("a", "gi"), "-")',
      'replace("abcABC", #"a"i, "-")',
      'replace("abcABC", #"a"gi, "-")',
    ],
  },
  'replace-all': {
    title: 'replace-all',
    category: 'Regular expression',
    linkName: 'replace-all',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      ...getOperatorArgs('string', ['regexp', 'string']),
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['a', 'b', 'x'] },
    ],
    description: 'Returns a new string with all matches of regular expression $b replaced by $x.',
    examples: [
      'replace-all("Duck duck", "u", "i")',
      'replace-all("Duck duck", regexp("u"), "i")',
      'replace-all("abcABC", regexp("a", "i"), "-")',
      'replace-all("abcABC", regexp("a", "gi"), "-")',
      'replace-all("abcABC", #"a"i, "-")',
      'replace-all("abcABC", #"a"gi, "-")',
    ],
  },
}
