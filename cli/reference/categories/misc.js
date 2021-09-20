module.exports = {
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
}