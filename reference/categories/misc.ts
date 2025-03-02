import type { FunctionReference } from '..'
import type { MiscApiName } from '../api'

export const miscReference: Record<MiscApiName, FunctionReference<'Misc'>> = {
  '!=': {
    title: '!=',
    category: 'Misc',
    clojureDocs: 'not=',
    linkName: '-exclamation-equal',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
      ys: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Result is `true` if no two `values` are equal to each other, otherwise result is `false`. Note that only two argument version result is negation of `=` function, that is `(!= a b)` is same as `(! (== a b))`.',
    examples: ['(!= 3)', '(!= 3 2)', '(!= :3 3)', '(!= 3 3 2)', '(!= :3 :2 :1 :0)', '(!= 0 -0)'],
  },
  '==': {
    title: '==',
    category: 'Misc',
    linkName: '-equal-equal',
    clojureDocs: '=',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
      ys: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Compares `values` according to \'equal\' predicate. Result is `true` if every specified value is equal to each other, otherwise result is `false`.',
    examples: ['(== 1 1)', '(== 1.01 1)', '(== :1 1)', '(== :2 :2 :2 :2)', '(== 2 2 1 2)'],
  },
  '<': {
    title: '<',
    category: 'Misc',
    linkName: '-lt',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
      ys: {
        type: 'number',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if the number $x and $ys are in increasing order, `false` otherwise.',
    examples: ['(< 0 1)', '(< 1 1.01)', '(< 1 1)', '(< 1 2 2 3)', '(< :a :b)', '(< [9] [1 2])'],
  },
  '>': {
    title: '>',
    category: 'Misc',
    linkName: '-gt',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
      ys: {
        type: 'number',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if the number $x and $ys are in decreasing order, `false` otherwise.',
    examples: ['(> 1 0)', '(> 1.01 1)', '(> 1 1)', '(> 4 3 2 1)', '(> 3 2 2 1)'],
  },
  '<=': {
    title: '<=',
    category: 'Misc',
    linkName: '-lte',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
      ys: {
        type: 'number',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if the number $x and $ys are in non decreasing order, `false` otherwise.',
    examples: ['(<= 0 1)', '(<= 1 1.01)', '(<= 1 1)', '(<= 1 2 3 4)', '(<= 1 2 2 3)'],
  },
  '>=': {
    title: '>=',
    category: 'Misc',
    linkName: '-gte',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'number',
      },
      ys: {
        type: 'number',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'ys'] },
    ],
    description: 'Returns `true` if the number $x and $ys are in non increasing order, `false` otherwise.',
    examples: ['(>= 1 0)', '(>= 1.01 1)', '(>= 1 1)', '(>= 4 3 2 1)', '(>= 3 2 2 1)'],
  },
  '!': {
    title: '!',
    category: 'Misc',
    linkName: '-exclamation',
    clojureDocs: 'not',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Computes logical negation. Note that any other $x than `false`, `0`, `nil` and `\'\'` is truthy.',
    examples: ['(! 3)', '(! true)', '(! "A string")', '(! 0)', '(! false)', '(! nil)', '(! "")'],
  },
  'write!': {
    title: 'write!',
    category: 'Misc',
    linkName: 'write-exclamation',
    clojureDocs: null,
    returns: {
      type: 'any',
    },
    args: {
      values: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['values'] },
    ],
    description: 'It logs the $values and then returns the last argument. If called with no arguments `nil` is returned.',
    examples: [
      '(write! "A string")',
      '(write! 100 "items")',
      '(write! (object :a 10))',
      '(write! [:a :b :c])',
      '(write! #"^start")',
      '(write! nil true false)',
    ],
  },
  'iso_date>epoch': {
    title: 'iso_date>epoch',
    category: 'Misc',
    linkName: 'iso_date-gtepoch',
    returns: {
      type: 'number',
    },
    args: {
      iso: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['iso'] },
    ],
    description: 'Returns milliseconds elapsed since the UNIX epoch to `iso`.',
    examples: [
      '(iso_date>epoch "2022-04-12T09:37:10.899Z")',
      '(iso_date>epoch "1980-01-01")',
    ],
  },
  'epoch>iso_date': {
    title: 'epoch>iso_date',
    category: 'Misc',
    linkName: 'epoch-gtiso_date',
    returns: {
      type: 'string',
    },
    args: {
      ms: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['ms'] },
    ],
    description: 'Returns IOS date time string from `ms` (milliseconds elapsed since the UNIX epoch).',
    examples: [
      '(epoch>iso_date 1649756230899)',
      '(epoch>iso_date 0)',
    ],
  },
  'boolean': {
    title: 'boolean',
    category: 'Misc',
    linkName: 'boolean',
    returns: {
      type: 'boolean',
    },
    args: {
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Coerces $x to boolean.',
    examples: [
      '(boolean 0)',
      '(boolean 1)',
      '(boolean nil)',
      '(boolean "Albert")',
    ],
  },
  'compare': {
    title: 'compare',
    category: 'Misc',
    linkName: 'compare',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'any',
      },
      b: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Compares two values. Returns `-1` if $a < $b, `1` if $a > $b and `0` if $a and $b have the same sort order.',
    examples: [
      '(compare 0 1)',
      '(compare "Albert" "Mojir")',
      '(compare 1 :1)',
      '(compare [1 2 3] [2 3])',
      '(compare [1 2 3] [2 3 4])',
      '(compare {:a 1 :b 2} {:a 1})',
      '(compare {:a 1} [2 3])',
      '(compare + -)',
    ],
  },
  'uuid!': {
    title: 'uuid!',
    category: 'Misc',
    linkName: 'uuid-exclamation',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns random UUID string.',
    examples: ['(uuid!)'],
  },
  'equal?': {
    title: 'equal?',
    category: 'Misc',
    linkName: 'equal-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'any',
      },
      b: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns true if $a and $b are structually equal.',
    examples: [
      '(equal? {:a 10 :b 20} {:b 20 :a 10})',
      '(equal? [1 true nil] [1 true nil])',
      '(equal? {:a 10 :b [1 2 {:b 20}]} {:b [1 2 {:b 20}] :a 10})',
      '(equal? {:a 10 :b [1 2 {:b 20}]} {:b [1 2 {:b 21}] :a 10})',
      '(== 0.3 (+ 0.1 0.2))',
      '(equal? 0.3 (+ 0.1 0.2))',
    ],
  },
  'json_parse': {
    title: 'json_parse',
    category: 'Misc',
    linkName: 'json_parse',
    clojureDocs: null,
    returns: {
      type: 'any',
    },
    args: {
      x: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `JSON.parse(`$x`)`.',
    examples: [
      '(json_parse "[1, 2, 3]")',
    ],
  },
  'json_stringify': {
    title: 'json_stringify',
    category: 'Misc',
    linkName: 'json_stringify',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      x: {
        type: 'any',
      },
      indent: {
        type: 'integer',
        description: 'Number of spaces to use for indentation.',
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'indent'] },
    ],
    description: 'Returns `JSON.stringify(`$x`)`. If second argument is provided, returns `JSON.stringify(`$x`, null, `$indent`)`.',
    examples: [
      '(json_stringify [1, 2, 3])',
      '(json_stringify {:a {:b 10}} 2)',
    ],
  },
}
