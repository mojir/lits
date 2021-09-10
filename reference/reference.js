module.exports = {
  '+': {
    name: '+',
    linkName: '_plus',
    syntax: '+ numbers(zero or more) => number',
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
      },
    ],
    shortDescription: 'Computes sum of numbers',
    longDescription: 'Computes sum of numbers',
    examples: ['(+)', '(+ 1)', '(+ 2 4)', '(+ 1 2 3 4)'],
    specialExpression: false,
    sideEffects: [],
  },
  '-': {
    name: '-',
    linkName: '_minus',
    syntax: '- numbers(zero or more) => number',
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
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
    linkName: '_star',
    syntax: '* numbers(zero or more) => number',
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
      },
    ],
    shortDescription: 'Computes product of numbers.',
    longDescription: 'Computes product of numbers.',
    examples: ['(*)', '(* 2)', '(* 2 4)', '(* 1 2 3 4)'],
    specialExpression: false,
    sideEffects: [],
  },
  '/': {
    name: '/',
    linkName: '_slash',
    syntax: '/ numbers(zero or more) => number',
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
      },
    ],
    shortDescription: 'Computes division or reciprocal.',
    longDescription:
      'Computes division or reciprocal. When called with one argument it computes reciprocal. When called with two or more arguments it does compute division of the first by the all remaining number.',
    examples: ['(/)', '(/ 2)', '(/ 2 4)', '(/ 4 3 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '%': {
    name: '%',
    linkName: '_percent',
    syntax: '% number divisor => number',
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
    linkName: '_notequal',
    syntax: '!= numbers (one or more) => true | false',
    arguments: [
      {
        name: 'numbers',
        type: 'number[]',
      },
    ],
    shortDescription: 'Result is true if no two numbers are equal to each other.',
    longDescription:
      'Result is true if no two numbers are equal to each other, otherwise result is false. Note that only two argument version result is negation of = function, that is (!= a b) is same as (! (= a b)).',
    examples: ['(!= 3)', '(!= 3 2)', '(!= 3 3)', '(!= 3 3 2)', '(!= 3 2 1 0)', '(!= 0 -0)'],
    specialExpression: false,
    sideEffects: [],
  },
  not: {
    name: 'not',
    linkName: 'not',
    syntax: 'not value => true | false',
    arguments: [
      {
        name: 'value',
        type: 'any',
      },
    ],
    shortDescription: 'Computes logical negation.',
    longDescription:
      'Computes logical negation. Note that any other value than false, 0, null, undefined and "" is considered as true.',
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
    linkName: '_1plus',
    syntax: '1+ number => number',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Adds one to the argument.',
    longDescription: 'Adds one to the argument.',
    examples: ['(1+ 0)', '(1+ 1)', '(1+ 100.1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '1-': {
    name: '1-',
    linkName: '_1minus',
    syntax: '1- number => number',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Subtracts one from the argument.',
    longDescription: 'Subtracts one from the argument.',
    examples: ['(1- 0)', '(1- 1)', '(1- 100.1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '<': {
    name: '<',
    linkName: '_lt',
    syntax: '< number => true | false',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares numbers according to "less than" predicate.',
    longDescription:
      'Compares numbers according to "less than" predicate. Each (overlapping) pair of the numbers is compared by it. The result is true if all compared pairs satisfy comparison.',
    examples: ['(< 0 1)', '(< 1 1.01)', '(< 1 1)', '(< 1 2 3 4)', '(< 1 2 2 3)'],
    specialExpression: false,
    sideEffects: [],
  },
  '>': {
    name: '>',
    linkName: '_gt',
    syntax: '> number => true | false',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares numbers according to "greater than" predicate.',
    longDescription:
      'Compares numbers according to "greater than" predicate. Each (overlapping) pair of the numbers is compared by it. The result is true if all compared pairs satisfy comparison.',
    examples: ['(> 1 0)', '(> 1.01 1)', '(> 1 1)', '(> 4 3 2 1)', '(> 3 2 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '<=': {
    name: '<=',
    linkName: '_lte',
    syntax: '<= number => true | false',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares numbers according to "less than or equal" predicate.',
    longDescription:
      'Compares numbers according to "less than or equal" predicate. Each (overlapping) pair of the numbers is compared by it. The result is true if all compared pairs satisfy comparison.',
    examples: ['(<= 0 1)', '(<= 1 1.01)', '(<= 1 1)', '(<= 1 2 3 4)', '(<= 1 2 2 3)'],
    specialExpression: false,
    sideEffects: [],
  },
  '>=': {
    name: '>=',
    linkName: '_gt',
    syntax: '>= number => true | false',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares numbers according to "greater than or equal" predicate.',
    longDescription:
      'Compares numbers according to "greater than or equal" predicate. Each (overlapping) pair of the numbers is compared by it. The result is true if all compared pairs satisfy comparison.',
    examples: ['(>= 1 0)', '(>= 1.01 1)', '(>= 1 1)', '(>= 4 3 2 1)', '(>= 3 2 2 1)'],
    specialExpression: false,
    sideEffects: [],
  },
  '=': {
    name: '=',
    linkName: '_equal',
    syntax: '= number => true | false',
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
    ],
    shortDescription: 'Compares numbers according to "equal" predicate.',
    longDescription:
      'Compares numbers according to "equal" predicate. Result is true if every specified number is equal to each other, otherwise result is false.',
    examples: ['(= 1 1)', '(= 1.01 1)', '(= 1 1)', '(= 2 2 2 2)', '(= 2 2 1 2)'],
    specialExpression: false,
    sideEffects: [],
  },
  and: {
    name: 'and',
    linkName: 'and',
    syntax: 'and forms => true | false',
    arguments: [
      {
        name: 'forms',
        type: 'form[]',
      },
    ],
    shortDescription: 'Computes logical "and" function.',
    longDescription:
      'Computes logical "and" function. Forms evaluation starts from left. Value from the first form that decides result is returned so forms at end of argument list may not evaluated.',
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
    linkName: 'or',
    syntax: 'or forms => true | false',
    arguments: [
      {
        name: 'forms',
        type: 'form[]',
      },
    ],
    shortDescription: 'Computes logical "or" function.',
    longDescription:
      'Computes logical "or" function. Forms evaluation starts from left. Value from the first form that decides result is returned so forms at end of argument list may not evaluated.',
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
    linkName: 'append',
    syntax: 'append lists (zero or more) => list',
    arguments: [
      {
        name: 'lists',
        type: 'list[]',
      },
    ],
    shortDescription: 'Concatenates list arguments into one list.',
    longDescription: 'Concatenates list arguments into one list. Resulting list is shallow copy of specified lists.',
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
}
