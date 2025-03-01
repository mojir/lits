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
    description: 'Result is `true` if no two `values` are equal to each other, otherwise result is `false`. Note that only two argument version result is negation of `=` function, that is `(!= a b)` is same as `(not (= a b))`.',
    examples: ['(!= 3)', '(!= 3 2)', '(!= :3 3)', '(!= 3 3 2)', '(!= :3 :2 :1 :0)', '(!= 0 -0)'],
  },
  '=': {
    title: '=',
    category: 'Misc',
    linkName: '-equal',
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
    examples: ['(= 1 1)', '(= 1.01 1)', '(= :1 1)', '(= :2 :2 :2 :2)', '(= 2 2 1 2)'],
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
  'not': {
    title: 'not',
    category: 'Misc',
    linkName: 'not',
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
    examples: ['(not 3)', '(not true)', '(not "A string")', '(not 0)', '(not false)', '(not nil)', '(not "")'],
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
  'inst-ms!': {
    title: 'inst-ms!',
    category: 'Misc',
    linkName: 'inst-ms-exclamation',
    clojureDocs: 'inst-ms',
    returns: {
      type: 'number',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns milliseconds elapsed since the UNIX epoch.',
    examples: ['(inst-ms!)'],
  },
  'iso-date-time->inst-ms': {
    title: 'iso-date-time->inst-ms',
    category: 'Misc',
    linkName: 'iso-date-time--gtinst-ms',
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
      '(iso-date-time->inst-ms "2022-04-12T09:37:10.899Z")',
      '(iso-date-time->inst-ms "1980-01-01")',
    ],
  },
  'inst-ms->iso-date-time': {
    title: 'inst-ms->iso-date-time',
    category: 'Misc',
    linkName: 'inst-ms--gtiso-date-time',
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
      '(inst-ms->iso-date-time 1649756230899)',
      '(inst-ms->iso-date-time 0)',
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
  'lits-version!': {
    title: 'lits-version!',
    category: 'Misc',
    linkName: 'lits-version-exclamation',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns the lits version.',
    examples: ['(lits-version!)'],
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
      '(= 0.3 (+ 0.1 0.2))',
      '(equal? 0.3 (+ 0.1 0.2))',
    ],
  },
  'json-parse': {
    title: 'json-parse',
    category: 'Misc',
    linkName: 'json-parse',
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
      '(json-parse "[1, 2, 3]")',
    ],
  },
  'json-stringify': {
    title: 'json-stringify',
    category: 'Misc',
    linkName: 'json-stringify',
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
      '(json-stringify [1, 2, 3])',
      '(json-stringify {:a {:b 10}} 2)',
    ],
  },
}
