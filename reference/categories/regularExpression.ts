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
If $s is a string and matches the regular expression, a \`match\`-array is returned, otherwise \`null\` is returned.`,
    examples: [
      '(match "  A string" (regexp "^\\s*(.*)$"))',
      '(match "My name is Albert" #"albert"i)',
      '(match "My name is Ben" #"albert"i)',
      '(match null #"albert"i)',
      '(match 1 #"albert"i)',
      '(match {} #"albert"i)',
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
        type: ['regexp', 'string'],
      },
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s', 'r', 'x'] },
    ],
    description: 'Returns a new string with first match of regular expression $r replaced by $x.',
    examples: [
      '(replace "Duck duck" "u" :i)',
      '(replace "Duck duck" (regexp :u) :i)',
      '(replace "abcABC" (regexp :a "i") "-")',
      '(replace "abcABC" (regexp :a "gi") "-")',
      '(replace "abcABC" #"a"i "-")',
      '(replace "abcABC" #"a"gi "-")',
    ],
  },
  replace_all: {
    title: 'replace_all',
    category: 'Regular expression',
    linkName: 'replace_all',
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
        type: ['regexp', 'string'],
      },
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s', 'r', 'x'] },
    ],
    description: 'Returns a new string with all matches of regular expression $r replaced by $x.',
    examples: [
      '(replace_all "Duck duck" "u" :i)',
      '(replace_all "Duck duck" (regexp :u) :i)',
      '(replace_all "abcABC" (regexp :a "i") "-")',
      '(replace_all "abcABC" (regexp :a "gi") "-")',
      '(replace_all "abcABC" #"a"i "-")',
      '(replace_all "abcABC" #"a"gi "-")',
    ],
  },
}
