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
        type: 'list',
      },
    ],
    shortDescription: 'Result is `true` if no two `values` are equal to each other.',
    longDescription:
      'Result is `true` if no two `values` are equal to each other, otherwise result is `false`. Note that only two argument version result is negation of `=` function, that is `(!= a b)` is same as `(not (= a b))`.',
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
        type: 'list',
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
  apply: {
    name: 'apply',
    category: 'Misc',
    linkName: 'apply',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'fn',
        type: 'function',
      },
      {
        name: 'args',
        type: 'list',
      },
    ],
    shortDescription: 'Call supplied function with specified arguments.',
    longDescription: 'Call supplied function with specified arguments.',
    examples: [`(apply #'+ (list 1 2 3))`, `(apply (lambda (x y) (sqrt (+ (* x x) (* y y)))) (list 3 4))`],
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
  cbrt: {
    name: 'cbrt',
    category: 'Math',
    linkName: 'cbrt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Computes cube root of `number`.',
    longDescription: 'Computes cube root of `number`.',
    examples: ['(cbrt 0)', '(cbrt 27)', '(cbrt 2)', '(cbrt -1)'],
    specialExpression: false,
    sideEffects: [],
  },
  pow: {
    name: 'pow',
    category: 'Math',
    linkName: 'pow',
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
    examples: ['(pow 2 3)', '(pow 2 0)', '(pow 2 -3)', '(pow -2 3)', '(pow -2 -3)'],
    specialExpression: false,
    sideEffects: [],
  },
  exp: {
    name: 'exp',
    category: 'Math',
    linkName: 'exp',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'power-number',
        type: 'number',
      },
    ],
    shortDescription: 'Computes `e` rasied to the `power-number`.',
    longDescription: 'Computes `e` rasied to the `power-number`.',
    examples: ['(exp 3)', '(exp 0)', '(exp -3)', '(exp 3)'],
    specialExpression: false,
    sideEffects: [],
  },
  round: {
    name: 'round',
    category: 'Math',
    linkName: 'round',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
      {
        name: 'decimals',
        type: 'integer',
        description: 'optional',
      },
    ],
    shortDescription: 'Returns rounded `number`.',
    longDescription: 'Returns rounded `number`. If `decimals` is provided it return a number with that many decimals.',
    examples: [
      '(round 2)',
      '(round 2.49)',
      '(round 2.5)',
      '(round -2.49)',
      '(round -2.5)',
      '(round -2.501)',
      '(round 1.23456789 4)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  trunc: {
    name: 'trunc',
    category: 'Math',
    linkName: 'trunc',
    returns: {
      type: 'integer',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the integer part of `number` by removing any fractional digits.',
    longDescription: 'Returns the integer part of `number` by removing any fractional digits.',
    examples: ['(trunc 2)', '(trunc 2.49)', '(trunc 2.5)', '(trunc -2.49)', '(trunc -2.5)', '(trunc -2.501)'],
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
  min: {
    name: 'min',
    category: 'Math',
    linkName: 'min',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
        description: '(one or many)',
      },
    ],
    shortDescription: 'Returns the smallest number of the arguments.',
    longDescription: 'Returns the smallest number of the arguments.',
    examples: ['(min 2 0 1)', '(min 2 -1 1)', '(min 2.5)'],
    specialExpression: false,
    sideEffects: [],
  },
  max: {
    name: 'max',
    category: 'Math',
    linkName: 'max',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
        description: '(one or many)',
      },
    ],
    shortDescription: 'Returns the largest number of the arguments.',
    longDescription: 'Returns the largest number of the arguments.',
    examples: ['(max 2 0 1)', '(max 2 -1 1)', '(max 2.5)'],
    specialExpression: false,
    sideEffects: [],
  },
  abs: {
    name: 'abs',
    category: 'Math',
    linkName: 'abs',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the absolute value of `number`.',
    longDescription: 'Returns the absolute value of `number`.',
    examples: ['(abs -2.3)', '(abs 0)', '(abs 2.5)'],
    specialExpression: false,
    sideEffects: [],
  },
  sign: {
    name: 'sign',
    category: 'Math',
    linkName: 'sign',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription:
      'Returns `1` if `number > 0`, `-1` if `number < 0`, `0` if `number = 0` or `-0` if `number = -0`.',
    longDescription: 'Returns `1` if `number > 0`, `-1` if `number < 0`, `0` if `number = 0` or `-0` if `number = -0`.',
    examples: ['(sign -2.3)', '(sign -0)', '(sign 0)', '(sign 12312)'],
    specialExpression: false,
    sideEffects: [],
  },
  e: {
    name: 'e',
    category: 'Math',
    linkName: 'e',
    returns: {
      type: 'number',
    },
    arguments: [],
    shortDescription: "Returns Euler's number.",
    longDescription: "Returns Euler's number, the base of natural logarithms, e.",
    examples: ['(e)'],
    specialExpression: false,
    sideEffects: [],
  },
  pi: {
    name: 'pi',
    category: 'Math',
    linkName: 'pi',
    returns: {
      type: 'number',
    },
    arguments: [],
    shortDescription: 'Returns Pi.',
    longDescription: 'Returns Pi, the ratio of the circumference of a circle to its diameter.',
    examples: ['(pi)'],
    specialExpression: false,
    sideEffects: [],
  },
  log: {
    name: 'log',
    category: 'Math',
    linkName: 'log',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the natural logarithm (base e) of `number`.',
    longDescription: 'Returns the natural logarithm (base e) of `number`.',
    examples: ['(log 0.01)', '(log (exp 12))', '(log 2.5)'],
    specialExpression: false,
    sideEffects: [],
  },
  log2: {
    name: 'log2',
    category: 'Math',
    linkName: 'log2',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the base 2 logarithm of a number.',
    longDescription: 'Returns the base 2 logarithm of a number.',
    examples: ['(log2 0.01)', '(log2 (pow 2 12))', '(log2 2.5)'],
    specialExpression: false,
    sideEffects: [],
  },
  log10: {
    name: 'log10',
    category: 'Math',
    linkName: 'log10',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the base 2 logarithm of a number.',
    longDescription: 'Returns the base 2 logarithm of a number.',
    examples: ['(log10 0.01)', '(log10 (pow 10 12))', '(log10 2.5)'],
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
    shortDescription: 'Returns `true` if `value` is a `boolean`, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is a `boolean`, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is `null`, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is `null`, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is `undefined`, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is `undefined`, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is a number, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is a number, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is a string, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is a string, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is a function, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is a function, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is an integer, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is an integer, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is a list, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is a list, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is an object, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is an object, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `value` is a regexp, otherwise `false`.',
    longDescription: 'Returns `true` if `value` is a regexp, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `number` is `0`, otherwise `false`.',
    longDescription: 'Returns `true` if `number` is `0`, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `number` is even, otherwise `false`.',
    longDescription: 'Returns `true` if `number` is even, otherwise `false`.',
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
    shortDescription: 'Returns `true` if `number` is odd, otherwise `false`.',
    longDescription: 'Returns `true` if `number` is odd, otherwise `false`.',
    examples: ['(odd? 1.0)', `(odd? 1.001)`, '(odd? -1)', `(odd? 2.1)`, '(odd? "10")', '(odd?)'],
    specialExpression: false,
    sideEffects: [],
  },
  'empty?': {
    name: 'empty?',
    category: 'Predicate',
    linkName: 'empty_question',
    returns: {
      type: 'boolean',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns `true` if `list` is empty, otherwise `false`.',
    longDescription: 'Returns `true` if `list` is empty, otherwise `false`.',
    examples: ['(empty? (list))', `(empty? (list 1 2 3))`],
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
  'string-repeat': {
    name: 'string-repeat',
    category: 'String',
    linkName: 'string-repeat',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'count',
        type: 'integer',
      },
    ],
    shortDescription: 'Repeates `string` `count` times.',
    longDescription: 'Repeates `string` `count` times.',
    examples: ['(string-repeat "*" 10)', '(string-repeat "***" 0)'],
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
  'string-to-number': {
    name: 'string-to-number',
    category: 'String',
    linkName: 'string-to-number',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Parses `string` to a number.',
    longDescription: 'Parses `string` to a number.',
    examples: [
      '(string-to-number "10")',
      '(string-to-number "010")',
      '(string-to-number "-1.01")',
      '(string-to-number "a10")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'number-to-string': {
    name: 'number-to-string',
    category: 'String',
    linkName: 'number-to-string',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Converts `number` to a string.',
    longDescription: 'Converts `number` to a string.',
    examples: ['(number-to-string 10)', '(number-to-string -1.01)'],
    specialExpression: false,
    sideEffects: [],
  },
  'string-reverse': {
    name: 'string-reverse',
    category: 'String',
    linkName: 'string-reverse',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Reverses a string.',
    longDescription: 'Reverses a string.',
    examples: ['(string-reverse "Albert")', '(string-reverse "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'lower-case': {
    name: 'lower-case',
    category: 'String',
    linkName: 'lower-case',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns `string` converted to lower case.',
    longDescription: 'Returns `string` converted to lower case.',
    examples: ['(lower-case "Albert")', '(lower-case "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'upper-case': {
    name: 'upper-case',
    category: 'String',
    linkName: 'upper-case',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns `string` converted to upper case.',
    longDescription: 'Returns `string` converted to upper case.',
    examples: ['(upper-case "Albert")', '(upper-case "")'],
    specialExpression: false,
    sideEffects: [],
  },
  capitalize: {
    name: 'capitalize',
    category: 'String',
    linkName: 'capitalize',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: "Returns copy of `string` with `string`'s first character converted to upper case.",
    longDescription: "Returns copy of `string` with `string`'s first character converted to upper case.",
    examples: ['(capitalize "albert")', '(capitalize " albert")', '(capitalize "")'],
    specialExpression: false,
    sideEffects: [],
  },
  trim: {
    name: 'trim',
    category: 'String',
    linkName: 'trim',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string with leading and trailing whitespaces removed.',
    longDescription: 'Returns a new string with leading and trailing whitespaces removed.',
    examples: ['(trim "  albert  ")', '(trim "   ")', '(trim "")'],
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
        type: 'list',
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
        type: 'list',
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
    examples: ['(length (list 1 2 3))', '(length (list))'],
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
  selt: {
    name: 'selt',
    category: 'List',
    linkName: 'selt',
    returns: {
      type: 'list',
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
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Sets `value` on specified element of `list`.',
    longDescription:
      'Sets `value` on specified element of `list`. The `index` is counted from `0`. Accessing out-of-bounds indices returns throws `Error`.',
    examples: ['(selt (list 1 2 3) 1 "two")', '(selt (list 1 2 3) 3 "Four")'],
    specialExpression: false,
    sideEffects: ['Mutates list'],
  },
  push: {
    name: 'push',
    category: 'List',
    linkName: 'push',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'values',
        type: 'list',
        description: 'one or more',
      },
    ],
    shortDescription: 'Pushes `values` to the end of `list`.',
    longDescription: 'Pushes `values` to the end of `list`.',
    examples: ['(push (list 1 2 3) 4)', '(push (list 1 2 3) 4 5 6)', '(setq l (list 1 2 3)) (push l 4) l'],
    specialExpression: false,
    sideEffects: ['Mutates list'],
  },
  pop: {
    name: 'pop',
    category: 'List',
    linkName: 'pop',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Removes and returns the last item of `list`.',
    longDescription: 'Removes and returns then last item of `list`. If `list` is empty, `undefined` is returned.',
    examples: ['(pop (list 1 2 3))', '(pop (list))'],
    specialExpression: false,
    sideEffects: ['Mutates list'],
  },
  unshift: {
    name: 'unshift',
    category: 'List',
    linkName: 'unshift',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'values',
        type: 'list',
        description: 'one or more',
      },
    ],
    shortDescription: 'Inserts `values` at the beginning of `list`.',
    longDescription: 'Inserts `values` at the beginning of `list`.',
    examples: ['(unshift (list 1 2 3) 4)', '(unshift (list 1 2 3) 4 5 6)', '(setq l (list 1 2 3)) (unshift l 4) l'],
    specialExpression: false,
    sideEffects: ['Mutates list'],
  },
  shift: {
    name: 'shift',
    category: 'List',
    linkName: 'shift',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Removes and returns the first item of `list`.',
    longDescription: 'Removes and returns the first item of `list`. If `list` is empty, `undefined` is returned.',
    examples: ['(shift (list 1 2 3))', '(shift (list))'],
    specialExpression: false,
    sideEffects: ['Mutates list'],
  },
  slice: {
    name: 'slice',
    category: 'List',
    linkName: 'slice',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'start',
        type: 'number',
        description: 'optional',
      },
      {
        name: 'end',
        type: 'number',
        description: 'optional',
      },
    ],
    shortDescription:
      'Returns a shallow copy of a portion of `list` into a new list selected from index `start` (inclusive) to index `end` (exclusive).',
    longDescription:
      'Returns a shallow copy of a portion of `list` into a new list selected from index `start` (inclusive) to index `end` (exclusive). If `start` is not provided it defaults to `0`. If `end` is not provided, the rest of the list will be copied.',
    examples: ['(slice (list 1 2 3 4 5) 2 4)', '(slice (list 1 2 3 4 5) 2)'],
    specialExpression: false,
    sideEffects: [],
  },
  splice: {
    name: 'splice',
    category: 'List',
    linkName: 'splice',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'start',
        type: 'number',
        description: 'optional',
      },
      {
        name: 'deleteCount',
        type: 'number',
        description: 'optional',
      },
      {
        name: '...values',
        type: 'any[]',
        description: 'optional',
      },
    ],
    shortDescription:
      'Changes the contents of a list by removing or replacing existing elements and/or adding new elements.',
    longDescription:
      'Changes the contents of a list by removing or replacing existing elements and/or adding new elements. Returns a list of the removed values.',
    examples: [
      '(splice (list 1 2 3 4 5) 2 2)',
      '(splice (list 1 2 3 4 5) 1 4 "3" "4")',
      '(setq l (list 1 2 3 4 5)) (splice l 2 2 "3" "4") l',
    ],
    specialExpression: false,
    sideEffects: ['Mutating list'],
  },
  reduce: {
    name: 'reduce',
    category: 'List',
    linkName: 'reduce',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'reducer',
        type: 'function',
      },
      {
        name: 'startValue',
        type: 'any',
      },
    ],
    shortDescription:
      'Runs `reducer` function on each element of the `list`, passing in the return value from the calculation on the preceding element.',
    longDescription:
      'Runs `reducer` function on each element of the `list`, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the array is a single value.',
    examples: [
      `(reduce #'+ (list 1 2 3) 0)`,
      `(reduce #'+ (list) 0)`,
      `(reduce (lambda (result value) (+ result (if (even? value) value 0))) (list 1 2 3 4 5 6 7 8 9) 0)`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'reduce-right': {
    name: 'reduce-right',
    category: 'List',
    linkName: 'reduce-right',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'reducer',
        type: 'function',
      },
      {
        name: 'startValue',
        type: 'any',
      },
    ],
    shortDescription:
      'Runs `reducer` function on each element of the `list` (starting from the last item), passing in the return value from the calculation on the preceding element.',
    longDescription:
      'Runs `reducer` function on each element of the `list` (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the array is a single value.',
    examples: [`(reduce-right #'concat (list "A" "B" "C") "")`],
    specialExpression: false,
    sideEffects: [],
  },
  map: {
    name: 'map',
    category: 'List',
    linkName: 'map',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'mapper',
        type: 'function',
      },
    ],
    shortDescription:
      'Creates a new list populated with the results of calling `mapper` on every element in the calling list.',
    longDescription:
      'Creates a new list populated with the results of calling `mapper` on every element in the calling list.',
    examples: [`(map #'string-reverse (list "Albert" "Mojir"))`, `(map #'string-reverse (list))`],
    specialExpression: false,
    sideEffects: [],
  },
  filter: {
    name: 'filter',
    category: 'List',
    linkName: 'filter',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'filter',
        type: 'function',
      },
    ],
    shortDescription: 'Creates a new list with all elements that pass the test implemented by `filter`.',
    longDescription: 'Creates a new list with all elements that pass the test implemented by `filter`.',
    examples: [
      `(filter #'string? (list "Albert" "Mojir" 160 (list 1 2)))`,
      `(filter (lambda (x) (> x 10)) (list 5 10 15 20))`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  reverse: {
    name: 'reverse',
    category: 'List',
    linkName: 'reverse',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'reverse',
        type: 'function',
      },
    ],
    shortDescription: 'Creates a new list with the elements from `list` in reversed order.',
    longDescription: 'Creates a new list with the elements from `list` in reversed order.',
    examples: [`(reverse (list "Albert" "Mojir" 160 (list 1 2)))`, `(reverse (list))`],
    specialExpression: false,
    sideEffects: [],
  },
  first: {
    name: 'first',
    category: 'List',
    linkName: 'first',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns the first element of `list`.',
    longDescription: 'Returns the first element of `list`. If `list` is empty, `undefined` is returned.',
    examples: [`(first (list "Albert" "Mojir" 160 (list 1 2)))`, `(first (list))`],
    specialExpression: false,
    sideEffects: [],
  },
  second: {
    name: 'second',
    category: 'List',
    linkName: 'second',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns the second element of `list`.',
    longDescription:
      'Returns the second element of `list`. If `list` has less than two elements, `undefined` is returned.',
    examples: [`(second (list "Albert" "Mojir" 160 (list 1 2)))`, `(second (list 1))`, `(second (list))`],
    specialExpression: false,
    sideEffects: [],
  },
  last: {
    name: 'last',
    category: 'List',
    linkName: 'last',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns the last element of `list`.',
    longDescription: 'Returns the last element of `list`. If `list` is empty, `undefined` is returned.',
    examples: [`(last (list "Albert" "Mojir" 160 (list 1 2)))`, `(last (list))`],
    specialExpression: false,
    sideEffects: [],
  },
  rest: {
    name: 'rest',
    category: 'List',
    linkName: 'rest',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns a new list with all but the first element from `list`.',
    longDescription:
      'Returns a new list with all but the first element from `list`. If `list` has less than two elements, an empty list is returned.',
    examples: [`(rest (list "Albert" "Mojir" 160 (list 1 2)))`, `(rest (list "Albert"))`, `(rest (list))`],
    specialExpression: false,
    sideEffects: [],
  },
  cons: {
    name: 'cons',
    category: 'List',
    linkName: 'cons',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Constructs a new list with `element` as first element and `rest` as the rest.',
    longDescription: 'Constructs a new list with `element` as first element and `rest` as the rest.',
    examples: [`(cons "Hi" (list "Albert" "Mojir" 160 (list 1 2)))`, `(cons "Hi" (list))`, `(cons "Hi")`],
    specialExpression: false,
    sideEffects: [],
  },
  object: {
    name: 'object',
    category: 'Object',
    linkName: 'object',
    returns: {
      type: 'object',
    },
    arguments: [
      {
        name: '[key value]',
        type: '[string any]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Constructs a new object. Object members are created from `key` - `value` pairs.',
    longDescription:
      'Constructs a new object. Object members are created from `key` - `value` pairs. Requires an even number of arguments.',
    examples: [`(object)`, `(object "x" 10 "y" true "z" "A string")`],
    specialExpression: false,
    sideEffects: [],
  },
  keys: {
    name: 'keys',
    category: 'Object',
    linkName: 'keys',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
    ],
    shortDescription: 'Returns list of all keys in `object`.',
    longDescription: 'Returns list of all keys in `object`.',
    examples: [`(keys (object))`, `(keys (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  values: {
    name: 'values',
    category: 'Object',
    linkName: 'values',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
    ],
    shortDescription: 'Returns list of all values in `object`.',
    longDescription: 'Returns list of all values in `object`.',
    examples: [`(values (object))`, `(values (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  entries: {
    name: 'entries',
    category: 'Object',
    linkName: 'entries',
    returns: {
      type: 'list of [key value] - paris',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
      },
    ],
    shortDescription: 'Returns nested list of all key - value pairs in `object`.',
    longDescription: 'Returns nested list of all key - value pairs in `object`.',
    examples: [`(entries (object))`, `(entries (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  merge: {
    name: 'merge',
    category: 'Object',
    linkName: 'merge',
    returns: {
      type: 'object',
    },
    arguments: [
      {
        name: 'object',
        type: 'object',
        description: 'one or many',
      },
    ],
    shortDescription: 'Returns a new object created by merging together all arguments.',
    longDescription: 'Returns a new object created by merging together all arguments.',
    examples: [`(merge (object "x" 10) (object "y" 20))`, `(merge (object "x" 10) (object "x" 15 "y" 20))`],
    specialExpression: false,
    sideEffects: [],
  },
  setq: {
    name: 'setq',
    category: 'Special expression',
    linkName: 'setq',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'variable',
        type: 'name',
      },
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Bind `value` to `variable`. If a variable name is not know, A global variable is created.',
    longDescription: 'Bind `value` to `variable`. If a variable name is not know, A global variable is created.',
    examples: [`(setq (object))`, `(setq (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: true,
    sideEffects: [],
  },
  let: {
    name: 'let',
    category: 'Special expression',
    linkName: 'let',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'bindings',
        type: 'bindings',
      },
      {
        name: 'body',
        type: 'lisp expressions',
      },
    ],
    shortDescription: 'Binds local variables. The variables lives only within the body.',
    longDescription:
      'Binds local variables. The variables lives only within the body. It returns evaluation of the last expression in the body.',
    examples: [`(let ((a (+ 1 2 3 4)) (b (* 1 2 3 4))) (write a b))`],
    specialExpression: true,
    sideEffects: [],
  },
  lambda: {
    name: 'lambda',
    category: 'Special expression',
    linkName: 'lambda',
    returns: {
      type: 'function',
    },
    arguments: [
      {
        name: 'arguments',
        type: 'arguments',
      },
      {
        name: 'body',
        type: 'lisp expressions',
      },
    ],
    shortDescription: 'Creates a function. When called, evaluation of the last expression in the body is returned.',
    longDescription: 'Creates a function. When called, evaluation of the last expression in the body is returned.',
    examples: [`(lambda (a b) (sqrt (+ (* a a) (* b b))))`, `((lambda (a b) (sqrt (+ (* a a) (* b b)))) 3 4)`],
    specialExpression: true,
    sideEffects: [],
  },
  defun: {
    name: 'defun',
    category: 'Special expression',
    linkName: 'defun',
    returns: {
      type: 'function',
    },
    arguments: [
      {
        name: 'name',
        type: 'name',
      },
      {
        name: 'arguments',
        type: 'arguments',
      },
      {
        name: 'body',
        type: 'lisp expressions',
      },
    ],
    shortDescription:
      'Creates a named global function. When called, evaluation of the last expression in the body is returned.',
    longDescription:
      'Creates a named global function. When called, evaluation of the last expression in the body is returned.',
    examples: [
      `(defun hyp (a b) (sqrt (+ (* a a) (* b b)))) #'hyp`,
      `(defun hyp (a b) (sqrt (+ (* a a) (* b b)))) (hyp 3 4)`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  function: {
    name: 'function',
    category: 'Special expression',
    linkName: 'function',
    returns: {
      type: 'function',
    },
    arguments: [
      {
        name: 'name',
        type: 'name',
      },
      {
        name: 'arguments',
        type: 'arguments',
      },
      {
        name: 'body',
        type: 'lisp expressions',
      },
    ],
    shortDescription: "Accessing namespace of functions. Shortform `#'` is equivalent.",
    longDescription: "Accessing namespace of functions. Shortform `#'` is equivalent.",
    examples: [`(function +)`, `#'+`, `(defun hyp (a b) (sqrt (+ (* a a) (* b b)))) (function hyp)`],
    specialExpression: true,
    sideEffects: [],
  },
  if: {
    name: 'if',
    category: 'Special expression',
    linkName: 'if',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'test',
        type: 'any',
      },
      {
        name: 'then',
        type: 'any',
      },
      {
        name: 'else',
        type: 'any',
      },
    ],
    shortDescription: 'Either `then` or `else` branch is taken. Then branch is selected when `test` result is truthy.',
    longDescription: 'Either `then` or `else` branch is taken. Then branch is selected when `test` result is truthy.',
    examples: [`(if true (write "TRUE") (write "FALSE"))`, `(if false (write "TRUE") (write "FALSE"))`],
    specialExpression: true,
    sideEffects: [],
  },
  cond: {
    name: 'cond',
    category: 'Special expression',
    linkName: 'cond',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'variants',
        type: 'variants',
      },
    ],
    shortDescription: 'Used for branching. Variants are tested sequentially from the top.',
    longDescription:
      'Used for branching. Variants are tested sequentially from the top. I no branch is tested truthy, `undefined` is returned.',
    examples: [
      `(cond (false (write "FALSE")) (null (write "NULL")) (true (write "TRUE")))`,
      `(cond (false (write "FALSE")) (null (write "NULL")))`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  regexp: {
    name: 'regexp',
    category: 'Regular expression',
    linkName: 'regexp',
    returns: {
      type: 'RegExp',
    },
    arguments: [
      {
        name: 'regexp-string',
        type: 'string',
      },
      {
        name: 'flags',
        type: 'string',
      },
    ],
    shortDescription: 'Creates a RegExp from `regexp-string` and `flags`.',
    longDescription: 'Creates a RegExp from `regexp-string` and `flags`.',
    examples: [`(regexp "^\\s*(.*)$")`, `(regexp "albert" "i")`],
    specialExpression: false,
    sideEffects: [],
  },
  match: {
    name: 'match',
    category: 'Regular expression',
    linkName: 'match',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'regexp',
        type: 'RegExp',
      },
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription:
      'Matches `string` against `regexp`. If `string` matches, a *match*-list is returned, otherwise `undefined`.',
    longDescription:
      'Matches `string` against `regexp`. If `string` matches, a *match*-list is returned, otherwise `undefined`.',
    examples: [
      `(match (regexp "^\\s*(.*)$") "  A string")`,
      `(match (regexp "albert" "i") "My name is Albert")`,
      `(match (regexp "albert" "i") "My name is Ben")`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  replace: {
    name: 'replace',
    category: 'Regular expression',
    linkName: 'replace',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'regexp',
        type: 'RegExp',
      },
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string with some or all matches of a pattern replaced by a replacement.',
    longDescription: 'Returns a new string with some or all matches of a pattern replaced by a replacement.',
    examples: [`(replace "Duck" (regexp "u") "i")`, `(replace "abcABC" (regexp "a" "gi") "-")`],
    specialExpression: false,
    sideEffects: [],
  },
  now: {
    name: 'now',
    category: 'Misc',
    linkName: 'now',
    returns: {
      type: 'number',
    },
    arguments: [],
    shortDescription: 'Returns milliseconds elapsed since the UNIX epoch.',
    longDescription: 'Returns milliseconds elapsed since the UNIX epoch.',
    examples: [`(now)`],
    specialExpression: false,
    sideEffects: [],
  },
  progn: {
    name: 'progn',
    category: 'Misc',
    linkName: 'progn',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'forms',
        type: 'form[]',
      },
    ],
    shortDescription:
      'Calls `forms` in the order they have been written. Resulting value is the value of the last form.',
    longDescription:
      'Calls `forms` in the order they have been written. Resulting value is the value of the last form.',
    examples: [`(progn (write "Hi") (write "Albert"))`, `(progn)`],
    specialExpression: false,
    sideEffects: [],
  },
  'get-path': {
    name: 'get-path',
    category: 'Misc',
    linkName: 'get-path',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'object',
        type: 'object | list',
      },
      {
        name: 'path',
        type: 'string',
      },
    ],
    shortDescription: 'Is used to get the value at `path` of object or list.',
    longDescription: 'Is used to get the value at `path` of object or list.',
    examples: [
      `(get-path (write (object "a" (object "x" (list 1 2 3)))) "a.x[2]")`,
      `(get-path (write (object "a" (object "x" (list 1 2 3)))) "b.z[10]")`,
      `(get-path (write (list (object "x" (list 1 2 3)) (object "x" (list 4 5 6)))) "[1].x[2]")`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  sin: {
    name: 'sin',
    category: 'Math',
    linkName: 'sin',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'angle',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the sine of `angle`.',
    longDescription: 'Returns the sine of `angle`. `angle` must be specified in radians.',
    examples: [`(sin 0)`, `(sin 1)`, `(sin (pi))`, `(sin -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  cos: {
    name: 'cos',
    category: 'Math',
    linkName: 'cos',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'angle',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the cosine of `angle`.',
    longDescription: 'Returns the cosine of `angle`. `angle` must be specified in radians.',
    examples: [`(cos 0)`, `(cos 1)`, `(cos (pi))`, `(cos -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  tan: {
    name: 'tan',
    category: 'Math',
    linkName: 'tan',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'angle',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the tangent of `angle`.',
    longDescription: 'Returns the tangent of `angle`. `angle` must be specified in radians.',
    examples: [`(tan 0)`, `(tan 1)`, `(tan (pi))`, `(tan -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  asin: {
    name: 'asin',
    category: 'Math',
    linkName: 'asin',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the arcsine (in radians) of `value`.',
    longDescription: 'Returns the arcsine (in radians) of `value`.',
    examples: [`(asin 0)`, `(asin 1)`, `(asin -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  acos: {
    name: 'acos',
    category: 'Math',
    linkName: 'acos',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the arccosine (in radians) of `value`.',
    longDescription: 'Returns the arccosine (in radians) of `value`.',
    examples: [`(acos 0)`, `(acos 1)`, `(acos -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  atan: {
    name: 'atan',
    category: 'Math',
    linkName: 'atan',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the arctangent (in radians) of `value`.',
    longDescription: 'Returns the arctangent (in radians) of `value`.',
    examples: [`(atan 0)`, `(atan 1)`, `(atan -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  sinh: {
    name: 'sinh',
    category: 'Math',
    linkName: 'sinh',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the hyperbolic sine of `value`.',
    longDescription: 'Returns the hyperbolic sine of `value`.',
    examples: [`(sinh 0)`, `(sinh 1)`, `(sinh -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  cosh: {
    name: 'cosh',
    category: 'Math',
    linkName: 'cosh',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the hyperbolic cosine of `value`.',
    longDescription: 'Returns the hyperbolic cosine of `value`.',
    examples: [`(cosh 0)`, `(cosh 1)`, `(cosh -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  tanh: {
    name: 'tanh',
    category: 'Math',
    linkName: 'tanh',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the hyperbolic tangent of `value`.',
    longDescription: 'Returns the hyperbolic tangent of `value`.',
    examples: [`(tanh 0)`, `(tanh 1)`, `(tanh -0.5)`, '(tanh 50)'],
    specialExpression: false,
    sideEffects: [],
  },
  asinh: {
    name: 'asinh',
    category: 'Math',
    linkName: 'asinh',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the hyperbolic arcsine of `value`.',
    longDescription: 'Returns the hyperbolic arcsine of `value`.',
    examples: [`(asinh 0)`, `(asinh 0.9)`, `(asinh -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  acosh: {
    name: 'acosh',
    category: 'Math',
    linkName: 'acosh',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the hyperbolic arccosine of `value`.',
    longDescription: 'Returns the hyperbolic arccosine of `value`.',
    examples: [`(acosh 1)`, `(acosh 2)`, `(acosh 100)`],
    specialExpression: false,
    sideEffects: [],
  },
  atanh: {
    name: 'atanh',
    category: 'Math',
    linkName: 'atanh',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'value',
        type: 'number',
      },
    ],
    shortDescription: 'Returns the hyperbolic arctangent of `value`.',
    longDescription: 'Returns the hyperbolic arctangent of `value`.',
    examples: [`(atanh 0)`, `(atanh 0.9)`, `(atanh -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
}

const categoryOrder = [
  'Special expression',
  'Predicate',
  'List',
  'Object',
  'String',
  'Math',
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
