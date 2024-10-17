import type { FunctionReference } from '..'
import type { RegularExpressionApiName } from '../api'

export const regularExpressionReference: Record<RegularExpressionApiName, FunctionReference<'Regular expression'>> = {
  regexp: {
    title: 'regexp',
    category: 'Regular expression',
    linkName: 'regexp',
    clojureDocs: null,
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
      '(regexp "^\\s*(.*)$")',
      '#"^\\s*(.*)$"',
      '(regexp "albert" :i)',
      '#"albert"ig',
    ],
  },
  match: {
    title: 'match',
    category: 'Regular expression',
    linkName: 'match',
    clojureDocs: null,
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      r: {
        type: 'regexp',
      },
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['r', 's'] },
    ],
    description: `Matches $s against regular expression $r.
If $s is a string and matches the regular expression, a \`match\`-array is returned, otherwise \`nil\` is returned.`,
    examples: [
      '(match (regexp "^\\s*(.*)$") "  A string")',
      '(match #"albert"i "My name is Albert")',
      '(match #"albert"i "My name is Ben")',
      '(match #"albert"i nil)',
      '(match #"albert"i 1)',
      '(match #"albert"i {})',
    ],
  },
  replace: {
    title: 'replace',
    category: 'Regular expression',
    linkName: 'replace',
    clojureDocs: null,
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      s: {
        type: 'string',
      },
      r: {
        type: 'regexp',
      },
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s', 'r', 'x'] },
    ],
    description: 'Returns a new string with some or all matches of regular expression $r replaced by $x.',
    examples: [
      '(replace "Duck" (regexp :u) :i)',
      '(replace "abcABC" (regexp :a "gi") "-")',
      '(replace "abcABC" #"a"gi "-")',
    ],
  },
}
