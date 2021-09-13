const functionReference = {
  '+': {
    name: '+',
    category: 'Math',
    linkName: '_plus',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Computes sum of `numbers`.',
    longDescription: 'Computes sum of `numbers`.',
    examples: ['(+)', '(+ 1)', '(+ 2 4)', '(+ 1 2 3 4)', '(+ (+ 2 3) (+ 5 6))'],
    specialExpression: false,
    sideEffects: [],
  },
  '-': {
    name: '-',
    category: 'Math',
    linkName: '_minus',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Computes difference between first value and sum of the rest.',
    longDescription:
      'Computes difference between first value and sum of the rest. When called with only one argument, it does negation.',
    examples: ['(-)', '(- 1)', '(- 2 4)', '(- 4 3 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '*': {
    name: '*',
    category: 'Math',
    linkName: '_star',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Computes product of `numbers`.',
    longDescription: 'Computes product of `numbers`.',
    examples: ['(*)', '(* 2)', '(* 2 4)', '(* 1 2 3 4)'],
    specialExpression: false,
    sideEffects: [],
  },
  '/': {
    name: '/',
    category: 'Math',
    linkName: '_slash',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Computes division or reciprocal.',
    longDescription:
      'Computes division or reciprocal. When called with one argument it computes reciprocal. When called with two or more arguments it does compute division of the first by the all remaining `numbers`.',
    examples: ['(/)', '(/ 2)', '(/ 2 4)', '(/ 4 3 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '%': {
    name: '%',
    category: 'Math',
    linkName: '_percent',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
      {
        name: 'divisor',
        type: 'number',
      },
    ],
    shortDescription: 'Returns modulus of two number arguments.',
    longDescription: 'Returns modulus of two number arguments.',
    examples: ['(% 5 3)', '(% 5.2 3.1)', '(% -5 3)', '(% 5 -3)', '(% -5 -3)'],
    specialExpression: false,
    sideEffects: [],
  },
  '!=': {
    name: '!=',
    category: 'Misc',
    linkName: '_notequal',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'values',
        type: 'any[]',
      },
    ],
    shortDescription: 'Result is `true` if no two `values` are equal to each other.',
    longDescription:
      'Result is `true` if no two `values` are equal to each other, otherwise result is `false`. Note that only two argument version result is negation of `=` function, that is `(!= a b)` is same as `(! (= a b))`.',
    examples: ['(!= 3)', '(!= 3 2)', '(!= "3" 3)', '(!= 3 3 2)', '(!= "3" "2" "1" "0")', '(!= 0 -0)'],
    specialExpression: false,
    sideEffects: [],
  },
  '=': {
    name: '=',
    category: 'Misc',
    linkName: '_equal',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'values',
        type: 'any[]',
      },
    ],
    shortDescription: 'Compares `values` according to "equal" predicate.',
    longDescription:
      'Compares `values` according to "equal" predicate. Result is `true` if every specified value is equal to each other, otherwise result is `false`.',
    examples: ['(= 1 1)', '(= 1.01 1)', '(= "1" 1)', '(= "2" "2" "2" "2")', '(= 2 2 1 2)'],
    specialExpression: false,
    sideEffects: [],
  },
  not: {
    name: 'not',
    category: 'Misc',
    linkName: 'not',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Computes logical negation.',
    longDescription:
      'Computes logical negation. Note that any other `value` than `false`, `0`, `null`, `undefined` and `""` is considered as `true`.',
    examples: [
      '(not 3)',
      '(not true)',
      '(not "A string")',
      '(not 0)',
      '(not false)',
      '(not null)',
      '(not undefined)',
      '(not "")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  '1+': {
    name: '1+',
    category: 'Math',
    linkName: '1_plus',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Adds one to `number`.',
    longDescription: 'Adds one to `number`.',
    examples: ['(1+ 0)', '(1+ 1)', '(1+ 100.1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '1-': {
    name: '1-',
    category: 'Math',
    linkName: '1_minus',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Subtracts one from `number`.',
    longDescription: 'Subtracts one from `number`.',
    examples: ['(1- 0)', '(1- 1)', '(1- 100.1)'],
    specialExpression: false,
    sideEffects: [],
  },
  sqrt: {
    name: 'sqrt',
    category: 'Math',
    linkName: 'sqrt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Computes square root of `number`.',
    longDescription: 'Computes square root of `number`.',
    examples: ['(sqrt 0)', '(sqrt 9)', '(sqrt 2)', '(sqrt -1)'],
    specialExpression: false,
    sideEffects: [],
  },
  expt: {
    name: 'expt',
    category: 'Math',
    linkName: 'expt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'base-number',
        type: 'number',
      },
      {
        name: 'power-number',
        type: 'number',
      },
    ],
    shortDescription: 'Computes returns `base-number` raised to the `power-number`.',
    longDescription: 'Computes returns `base-number` raised to the `power-number`.',
    examples: ['(expt 2 3)', '(expt 2 0)', '(expt 2 -3)', '(expt -2 3)', '(expt -2 -3)', '(expt -2)'],
    specialExpression: false,
    sideEffects: [],
  },
  round: {
    name: 'round',
    category: 'Math',
    linkName: 'round',
    returns: {
      type: 'integer',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns `number` rounded to the nearest `integer`.',
    longDescription: 'Returns `number` rounded to the nearest `integer`.',
    examples: ['(round 2)', '(round 2.49)', '(round 2.5)', '(round -2.49)', '(round -2.5)', '(round -2.501)'],
    specialExpression: false,
    sideEffects: [],
  },
  floor: {
    name: 'floor',
    category: 'Math',
    linkName: 'floor',
    returns: {
      type: 'integer',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the largest `integer` less than or equal to `number`.',
    longDescription: 'Returns the largest `integer` less than or equal to `number`.',
    examples: ['(floor 2)', '(floor 2.49)', '(floor 2.5)', '(floor -2.49)', '(floor -2.5)', '(floor -2.501)'],
    specialExpression: false,
    sideEffects: [],
  },
  ceil: {
    name: 'ceil',
    category: 'Math',
    linkName: 'ceil',
    returns: {
      type: 'integer',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the smallest `integer` larger than or equal to `number`.',
    longDescription: 'Returns the smallest `integer` larger than or equal to `number`.',
    examples: ['(ceil 2)', '(ceil 2.49)', '(ceil 2.5)', '(ceil -2.49)', '(ceil -2.5)', '(ceil -2.501)'],
    specialExpression: false,
    sideEffects: [],
  },
  random: {
    name: 'random',
    category: 'Math',
    linkName: 'random',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'positive number',
      },
    ],
    shortDescription: 'Returns a semi random number between `0` (inclusive) and `number` (exclusive).',
    longDescription: 'Returns a semi random number between `0` (inclusive) and `number` (exclusive).',
    examples: ['(random 1)', '(random 0.01)', '(random 2.5)', '(random 0)', '(random -1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '<': {
    name: '<',
    category: 'Math',
    linkName: '_lt',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares `numbers` according to "less than" predicate.',
    longDescription:
      'Compares `numbers` according to "less than" predicate. Each (overlapping) pair of the `numbers` is compared by it. The result is `true` if all compared pairs satisfy comparison.',
    examples: ['(< 0 1)', '(< 1 1.01)', '(< 1 1)', '(< 1 2 3 4)', '(< 1 2 2 3)'],
    specialExpression: false,
    sideEffects: [],
  },
  '>': {
    name: '>',
    category: 'Math',
    linkName: '_gt',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares `numbers` according to "greater than" predicate.',
    longDescription:
      'Compares `numbers` according to "greater than" predicate. Each (overlapping) pair of the `numbers` is compared by it. The result is `true` if all compared pairs satisfy comparison.',
    examples: ['(> 1 0)', '(> 1.01 1)', '(> 1 1)', '(> 4 3 2 1)', '(> 3 2 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '<=': {
    name: '<=',
    category: 'Math',
    linkName: '_lte',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares `numbers` according to "less than or equal" predicate.',
    longDescription:
      'Compares `numbers` according to "less than or equal" predicate. Each (overlapping) pair of the `numbers` is compared by it. The result is `true` if all compared pairs satisfy comparison.',
    examples: ['(<= 0 1)', '(<= 1 1.01)', '(<= 1 1)', '(<= 1 2 3 4)', '(<= 1 2 2 3)'],
    specialExpression: false,
    sideEffects: [],
  },
  '>=': {
    name: '>=',
    category: 'Math',
    linkName: '_gte',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares `numbers` according to "greater than or equal" predicate.',
    longDescription:
      'Compares `numbers` according to "greater than or equal" predicate. Each (overlapping) pair of the `numbers` is compared by it. The result is `true` if all compared pairs satisfy comparison.',
    examples: ['(>= 1 0)', '(>= 1.01 1)', '(>= 1 1)', '(>= 4 3 2 1)', '(>= 3 2 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  and: {
    name: 'and',
    category: 'Special expression',
    linkName: 'and',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'forms',
        type: 'form[]',
      },
    ],
    shortDescription: 'Computes logical "and" function.',
    longDescription:
      'Computes logical "and" function. `forms` evaluation starts from left. Value from the first form that decides result is returned so `forms` at end of argument list may not evaluated.',
    examples: [
      '(and 1 1)',
      '(and (> 3 2) "string")',
      '(and (< 3 2) "string")',
      '(and true true true true)',
      '(and true true 0 true)',
    ],
    specialExpression: true,
    sideEffects: [],
  },
  or: {
    name: 'or',
    category: 'Special expression',
    linkName: 'or',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'forms',
        type: 'form[]',
      },
    ],
    shortDescription: 'Computes logical "or" function.',
    longDescription:
      'Computes logical "or" function. `forms` evaluation starts from left. Value from the first form that decides result is returned so forms at end of argument list may not evaluated.',
    examples: [
      '(or 1 1)',
      '(or (> 3 2) "string")',
      '(or (< 3 2) "string")',
      '(or true true true true)',
      '(or 1 2 3 4)',
    ],
    specialExpression: true,
    sideEffects: [],
  },
  append: {
    name: 'append',
    category: 'List',
    linkName: 'append',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'lists',
        type: 'list[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Concatenates list arguments into one list.',
    longDescription: 'Concatenates list arguments into one list. Resulting list is shallow copy of specified `lists`.',
    examples: [
      '(append (list 1 2) (list 3 4))',
      '(append (list) (list 3 4))',
      '(append (list 1 2) (list))',
      '(append (list 1 2) (list 3 4) (list 5 6))',
      '(append (list))',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  aref: {
    name: 'aref',
    category: 'String',
    linkName: 'aref',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'index',
        type: 'integer',
      },
    ],
    shortDescription: 'Accesses specified element of a string.',
    longDescription:
      'Accesses specified element of a string. `index` is counted from `0`. Accessing out-of-bounds indices returns `undefined`.',
    examples: ['(aref "A string" 0)', '(aref "A string" 2)', '(aref "A string" 20)', '(aref "A string" -1)'],
    specialExpression: false,
    sideEffects: [],
  },
  'boolean?': {
    name: 'boolean?',
    category: 'Predicate',
    linkName: 'boolean_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is a `boolean`, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is a `boolean`, otherwise it returns `false`.',
    examples: [
      '(boolean? true)',
      '(boolean? false)',
      '(boolean? (list 1 2 3))',
      '(boolean? 0)',
      '(boolean? "A string")',
      '(boolean?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'null?': {
    name: 'null?',
    category: 'Predicate',
    linkName: 'null_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is `null`, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is `null`, otherwise it returns `false`.',
    examples: ['(null? null)', '(null? false)', '(null? (list 1 2 3))', '(null? 0)', '(null? "A string")', '(null?)'],
    specialExpression: false,
    sideEffects: [],
  },
  'undefined?': {
    name: 'undefined?',
    category: 'Predicate',
    linkName: 'undefined_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is `undefined`, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is `undefined`, otherwise it returns `false`.',
    examples: [
      '(undefined? undefined)',
      '(undefined? false)',
      '(undefined? null)',
      '(undefined? (list 1 2 3))',
      '(undefined? 0)',
      '(undefined? "A string")',
      '(undefined?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'number?': {
    name: 'number?',
    category: 'Predicate',
    linkName: 'number_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is a number, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is a number, otherwise it returns `false`.',
    examples: [
      '(number? 0)',
      '(number? 2)',
      '(number? -0.12)',
      '(number? false)',
      '(number? (list 1 2 3))',
      '(number? "A string")',
      '(number?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string?': {
    name: 'string?',
    category: 'Predicate',
    linkName: 'string_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is a string, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is a string, otherwise it returns `false`.',
    examples: [
      '(string? "")',
      '(string? "A string")',
      '(string? (if true "A string" false))',
      '(string? false)',
      '(string? (list 1 2 3))',
      '(string? 100)',
      '(string?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'function?': {
    name: 'function?',
    category: 'Predicate',
    linkName: 'function_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is a function, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is a function, otherwise it returns `false`.',
    examples: [
      `(function? #'+)`,
      '(function? (function /))',
      '(function? (lambda (x y) (+ x y)))',
      '(function? false)',
      '(function? "false")',
      '(function? (list 1 2 3))',
      '(function? 100)',
      '(function?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'integer?': {
    name: 'integer?',
    category: 'Predicate',
    linkName: 'integer_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is an integer, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is an integer, otherwise it returns `false`.',
    examples: [
      `(integer? 0)`,
      `(integer? -12)`,
      `(integer? 42)`,
      '(integer? 10.1)',
      '(integer? (lambda (x y) (+ x y)))',
      '(integer? false)',
      '(integer? "false")',
      '(integer? (list 1 2 3))',
      '(integer?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'list?': {
    name: 'list?',
    category: 'Predicate',
    linkName: 'list_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is a list, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is a list, otherwise it returns `false`.',
    examples: [
      `(list? (list))`,
      `(list? (list 1 2 3))`,
      `(list? (object "a" 10))`,
      `(list? 42)`,
      '(list? 10.1)',
      '(list? (lambda (x y) (+ x y)))',
      '(list?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'object?': {
    name: 'object?',
    category: 'Predicate',
    linkName: 'object_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is an object, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is an object, otherwise it returns `false`.',
    examples: [
      `(object? (object "a" 10))`,
      `(object? (object))`,
      `(object? 42)`,
      '(object? 10.1)',
      '(object? (lambda (x y) (+ x y)))',
      '(object? (regexp "^start"))',
      '(object? "false")',
      '(object? (list 1 2 3))',
      '(object?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'regexp?': {
    name: 'regexp?',
    category: 'Predicate',
    linkName: 'regexp_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Returns `true` if `value` is a regexp, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `value` is a regexp, otherwise it returns `false`.',
    examples: [
      '(regexp? (regexp "^start"))',
      `(regexp? -12)`,
      `(regexp? (object))`,
      '(regexp? 10.1)',
      '(regexp? (lambda (x y) (+ x y)))',
      '(regexp? false)',
      '(regexp? "false")',
      '(regexp? (list 1 2 3))',
      '(regexp?)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'zero?': {
    name: 'zero?',
    category: 'Predicate',
    linkName: 'zero_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns `true` if `number` is `0`, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `number` is `0`, otherwise it returns `false`.',
    examples: ['(zero? 0)', `(zero? -0.0)`, '(zero? 1)', `(zero? 0.1)`, '(zero? "10")', '(zero?)'],
    specialExpression: false,
    sideEffects: [],
  },
  'even?': {
    name: 'even?',
    category: 'Predicate',
    linkName: 'even_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns `true` if `number` is even, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `number` is even, otherwise it returns `false`.',
    examples: ['(even? 0)', `(even? -0.0)`, '(even? -1)', `(even? 2.1)`, '(even? "10")', '(even?)'],
    specialExpression: false,
    sideEffects: [],
  },
  'odd?': {
    name: 'odd?',
    category: 'Predicate',
    linkName: 'odd_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns `true` if `number` is odd, otherwise it returns `false`.',
    longDescription: 'Returns `true` if `number` is odd, otherwise it returns `false`.',
    examples: ['(odd? 1.0)', `(odd? 1.001)`, '(odd? -1)', `(odd? 2.1)`, '(odd? "10")', '(odd?)'],
    specialExpression: false,
    sideEffects: [],
  },
  substring: {
    name: 'substring',
    category: 'String',
    linkName: 'substring',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'indexStart',
        type: 'integer',
      },
      {
        name: 'indexEnd',
        type: 'integer',
        description: 'optional',
      },
    ],
    shortDescription: 'Extracts characters from `indexStart` up to but not including `indexEnd`.',
    longDescription: 'Extracts characters from `indexStart` up to but not including `indexEnd`.',
    examples: [
      '(substring "A string" 2)',
      '(substring "A string" 2 5)',
      '(substring "A string" 2 100)',
      '(substring "A string" 100)',
      '(substring "A string" 5 2)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string-length': {
    name: 'string-length',
    category: 'String',
    linkName: 'string-length',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns length of `string`.',
    longDescription: 'Returns length of `string`.',
    examples: ['(string-length "A string")', '(string-length "")'],
    specialExpression: false,
    sideEffects: [],
  },
  concat: {
    name: 'concat',
    category: 'String',
    linkName: 'concat',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'strings',
        type: 'string[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Concatenats `strings` into one `string`.',
    longDescription: 'Concatenats `strings` into one `string`.',
    examples: ['(concat "A string" ", and another string" " ...and more")', '(concat "Just one string")', '(concat)'],
    specialExpression: false,
    sideEffects: [],
  },
  'string>': {
    name: 'string>',
    category: 'String',
    linkName: 'string_gt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than `string2`, otherwise result is `false`.',
    examples: [
      '(string> "A string" "Another string")',
      '(string> "Albert Mojir" "Albert")',
      '(string> "Albert" "Albert")',
      '(string> "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string>=': {
    name: 'string>=',
    category: 'String',
    linkName: 'string_gte',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than or equal to `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than or equal to `string2`, otherwise result is `false`.',
    examples: [
      '(string>= "A string" "Another string")',
      '(string>= "Albert Mojir" "Albert")',
      '(string>= "Albert" "Albert")',
      '(string>= "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string<': {
    name: 'string<',
    category: 'String',
    linkName: 'string_lt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than `string2`, otherwise result is `false`.',
    examples: [
      '(string< "A string" "Another string")',
      '(string< "Albert Mojir" "Albert")',
      '(string< "Albert" "Albert")',
      '(string< "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string<=': {
    name: 'string<=',
    category: 'String',
    linkName: 'string_lte',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than or equal to `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than or equal to `string2`, otherwise result is `false`.',
    examples: [
      '(string<= "A string" "Another string")',
      '(string<= "Albert Mojir" "Albert")',
      '(string<= "Albert" "Albert")',
      '(string<= "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  write: {
    name: 'write',
    category: 'Misc',
    linkName: 'write',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'values',
        type: 'any[]',
      },
    ],
    shortDescription: 'It console.log the `values` and then returns the last element of the `values` list.',
    longDescription:
      'It console.log the `values` and then returns the last element of the `values` list.. If called with no arguments `undefined` is returned.',
    examples: [
      '(write "A string")',
      '(write 100 "items")',
      '(write (object "a" 10))',
      '(write (list "a" "b" "c"))',
      '(write (regexp "^start"))',
      '(write null undefined true false)',
    ],
    specialExpression: false,
    sideEffects: ['Does a console.log'],
  },
  'has-attr': {
    name: 'has-attr',
    category: 'Object',
    linkName: 'has-attr',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
    ],
    shortDescription: 'Returns `true` if `object` has an attribute named `attr`, otherwise returns `false`.',
    longDescription: 'Returns `true` if `object` has an attribute named `attr`, otherwise returns `false`.',
    examples: [
      '(has-attr (object "a" 10 "b" 20) "a")',
      '(has-attr (object "a" 10 "b" undefined) "b")',
      '(has-attr (object "a" 10 "b" undefined) "c")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'get-attr': {
    name: 'get-attr',
    category: 'Object',
    linkName: 'get-attr',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
    ],
    shortDescription:
      "Returns the value of `object`'s attribute `attr`. Returns `undefined` if `object` has no attribute named `attr`.",
    longDescription:
      "Returns the value of `object`'s attribute `attr`. Returns `undefined` if `object` has no attribute named `attr`.",
    examples: [
      '(get-attr (object "a" 10 "b" 20) "a")',
      '(get-attr (object "a" 10 "b" undefined) "b")',
      '(get-attr (object "a" 10 "b" undefined) "c")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'del-attr': {
    name: 'del-attr',
    category: 'Object',
    linkName: 'del-attr',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
    ],
    shortDescription: 'Deletes the attribute `attr` from `object`.',
    longDescription: 'Deletes the attribute `attr` from `object`.',
    examples: [
      '(del-attr (object "x" 10) "x")',
      '(del-attr (object "x" 10) "y")',
      '(setq o (object "a" 5)) (del-attr o "a") o',
      '(setq o (object "a" 5)) (del-attr o "b") o',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'set-attr': {
    name: 'set-attr',
    category: 'Object',
    linkName: 'set-attr',
    returns: {
      type: 'value',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
      {
        name: 'attr',
        type: 'string',
      },
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Sets an attribute on `object`. Returns `value`.',
    longDescription: 'Sets an attribute on `object`. Returns `value`.',
    examples: [
      '(set-attr (object "x" 10) "a" 10)',
      '(setq o (object)) (set-attr o "a" 10) o',
      '(setq o (object "a" 5)) (set-attr o "a" 10) o',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  list: {
    name: 'list',
    category: 'List',
    linkName: 'list',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'values',
        type: 'any[]',
      },
    ],
    shortDescription: 'Makes new list from `values`.',
    longDescription: 'Makes new list from `values`.',
    examples: ['(list 1 2 3)', '(list (list null undefined false true))', '(list)'],
    specialExpression: false,
    sideEffects: [],
  },
  listf: {
    name: 'listf',
    category: 'List',
    linkName: 'listf',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'length',
        type: 'number',
      },
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Creates a list with `length` number of elements and sets all elements to "value".',
    longDescription: 'Creates a list with `length` number of elements and sets all elements to "value".',
    examples: ['(listf 10 null)', '(listf 0 100)'],
    specialExpression: false,
    sideEffects: [],
  },
  range: {
    name: 'range',
    category: 'List',
    linkName: 'range',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'a',
        type: 'number',
      },
      {
        name: 'length',
        type: 'number',
        description: 'optional',
      },
      {
        name: 'length',
        type: 'number',
        description: 'optional',
      },
    ],
    shortDescription: 'Create a list with a range of numbers.',
    longDescription:
      'Create a list with a range of numbers. If only one argument: `0...a`, otherwise: `a...b`. `step` defaults to `1`.',
    examples: ['(range 4)', '(range 1 4)', '(range 0.4 4.9)', '(range 0.25 1 0.25)'],
    specialExpression: false,
    sideEffects: [],
  },
  length: {
    name: 'length',
    category: 'List',
    linkName: 'length',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns length of `list`.',
    longDescription: 'Returns length of `list`.',
    examples: ['(length (list 1 2 3)', '(length (list))'],
    specialExpression: false,
    sideEffects: [],
  },
  elt: {
    name: 'elt',
    category: 'List',
    linkName: 'elt',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'index',
        type: 'non negative integer',
      },
    ],
    shortDescription: 'Accesses specified element of `list`.',
    longDescription:
      'Accesses specified element of `list`. The `index` is counted from `0`. Accessing out-of-bounds indices returns `undefined`.',
    examples: ['(elt (list 1 2 3) 1)', '(elt (list 1 2 3) 3)'],
    specialExpression: false,
    sideEffects: [],
  },
}

const categoryOrder = [
  'Special expression',
  'Math',
  'Predicate',
  'String',
  'List',
  'Object',
  'Regular expression',
  'Misc',
]

const categories = Object.values(functionReference)
  .reduce((result, item) => {
    if (!result.includes(item.category)) {
      result.push(item.category)
    }
    return result
  }, [])
  .sort((a, b) => categoryOrder.indexOf(a) - categoryOrder.indexOf(b))

module.exports = {
  functionReference,
  categories,
}
