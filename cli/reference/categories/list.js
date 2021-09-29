module.exports = {
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
      `(append '(1 2) '(3 4))`,
      `(append '() '(3 4))`,
      `(append '(1 2) '())`,
      `(append '(1 2) '(3 4) '(5 6))`,
      `(append '())`,
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
    examples: [
      `(list 1 2 3)`,
      `(list (list null undefined false true))`,
      `'()`,
      `'(1 2 3)`,
      `'('(null undefined false true))`,
      `'()`,
    ],
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
    examples: [`(listf 10 null)`, `(listf 0 100)`],
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
    examples: [`(range 4)`, `(range 1 4)`, `(range 0.4 4.9)`, `(range 0.25 1 0.25)`],
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
    examples: [`(length '(1 2 3))`, `(length '())`],
    specialExpression: false,
    sideEffects: [],
  },
  at: {
    name: 'at',
    category: 'List',
    linkName: 'at',
    returns: {
      type: 'any',
    },
    arguments: [
      {
        name: 'input',
        type: 'string | list',
      },
      {
        name: 'index',
        type: 'integer',
      },
    ],
    shortDescription: 'Accesses element `index` of `input`. Negative `index` counts backwards.',
    longDescription:
      'Accesses element `index` of `input`. Negative `index` counts backwards. Accessing out-of-bounds indices returns `undefined`.',
    examples: [
      `(at '(1 2 3) 1)`,
      `(at '(1 2 3) 3)`,
      `(at '(1 2 3) -1)`,
      `(at '(1 2 3) -1)`,
      `(at "A string" 1)`,
      `(at "A string" 3)`,
      `(at "A string" -3)`,
      `(at "A string" 30)`,
      `(at "A string" -30)`,
    ],
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
    examples: [`(selt '(1 2 3) 1 "two")`, `(selt '(1 2 3) 3 "Four")`],
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
    examples: [`(push '(1 2 3) 4)`, `(push '(1 2 3) 4 5 6)`, `(setq l '(1 2 3)) (push l 4) l`],
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
    examples: [`(pop '(1 2 3))`, `(pop '())`],
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
    examples: [`(unshift '(1 2 3) 4)`, `(unshift '(1 2 3) 4 5 6)`, `(setq l '(1 2 3)) (unshift l 4) l`],
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
    examples: [`(shift '(1 2 3))`, `(shift '())`],
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
    examples: [`(slice '(1 2 3 4 5) 2 4)`, `(slice '(1 2 3 4 5) 2)`],
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
      `(splice '(1 2 3 4 5) 2 2)`,
      `(splice '(1 2 3 4 5) 1 4 "3" "4")`,
      `(setq l '(1 2 3 4 5)) (splice l 2 2 "3" "4") l`,
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
      `(reduce #'+ '(1 2 3) 0)`,
      `(reduce #'+ '() 0)`,
      `(reduce (lambda (result value) (+ result (if (even? value) value 0))) '(1 2 3 4 5 6 7 8 9) 0)`,
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
    examples: [`(reduce-right #'concat '("A" "B" "C") "")`],
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
        name: 'mapper',
        type: 'function',
      },
      {
        name: 'list',
        type: 'list',
        description: 'one or many',
      },
    ],
    shortDescription:
      'Creates a new list populated with the results of calling `mapper` on every elements in the calling `list`s.',
    longDescription:
      'Creates a new list populated with the results of calling `mapper` on every elements in the calling `list`s.',
    examples: [
      `(map #'string-reverse '("Albert" "Mojir"))`,
      `(map #'string-reverse '())`,
      `(map #'+ '(1 2 3) '(1 2 3))`,
      `(map #'max '(2 6 3) '(2 4 7) '(1 6 2))`,
    ],
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
    examples: [`(filter #'string? '("Albert" "Mojir" 160 '(1 2)))`, `(filter (lambda (x) (> x 10)) '(5 10 15 20))`],
    specialExpression: false,
    sideEffects: [],
  },
  find: {
    name: 'find',
    category: 'List',
    linkName: 'find',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'finder',
        type: 'function',
      },
    ],
    shortDescription: 'Returns the first elements that pass the test implemented by `finder`.',
    longDescription:
      'Returns the first elements that pass the test implemented by `finder`. I no element was found, `undefined` is returned.',
    examples: [`(find #'string? '("Albert" "Mojir" 160 '(1 2)))`, `(find (lambda (x) (> x 10)) '(5 10 15 20))`],
    specialExpression: false,
    sideEffects: [],
  },
  position: {
    name: 'position',
    category: 'List',
    linkName: 'position',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'finder',
        type: 'function',
      },
    ],
    shortDescription: 'Returns the index of the first elements that pass the test implemented by `finder`.',
    longDescription:
      'Returns the index of the first elements that pass the test implemented by `finder`. I no element was found, `undefined` is returned.',
    examples: [
      `(position #'string? '("Albert" "Mojir" 160 '(1 2)))`,
      `(position (lambda (x) (> x 10)) '(5 10 15 20))`,
      `(position (lambda (x) (> x 100)) '(5 10 15 20))`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  some: {
    name: 'some',
    category: 'List',
    linkName: 'some',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'finder',
        type: 'function',
      },
    ],
    shortDescription:
      'Returns true if at least one element pass the test implemented by `finder`, otherwise returns false.',
    longDescription:
      'Returns true if at least one element pass the test implemented by `finder`, otherwise returns false.',
    examples: [
      `(some #'string? '("Albert" "Mojir" 160 '(1 2)))`,
      `(some (lambda (x) (> x 10)) '(5 10 15 20))`,
      `(some (lambda (x) (> x 10)) '(1 2 3 4))`,
      `(some (lambda (x) (> x 10)) '())`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  every: {
    name: 'every',
    category: 'List',
    linkName: 'every',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'finder',
        type: 'function',
      },
    ],
    shortDescription: 'Returns true if all elements pass the test implemented by `finder`, otherwise returns false.',
    longDescription: 'Returns true if all elements pass the test implemented by `finder`, otherwise returns false.',
    examples: [
      `(every #'string? '("Albert" "Mojir" 160 '(1 2)))`,
      `(every (lambda (x) (> x 10)) '(50 100 150 200))`,
      `(every (lambda (x) (> x 10)) '())`,
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
    examples: [`(reverse '("Albert" "Mojir" 160 '(1 2)))`, `(reverse '())`],
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
    examples: [`(first '("Albert" "Mojir" 160 '(1 2)))`, `(first '())`],
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
    examples: [`(second '("Albert" "Mojir" 160 '(1 2)))`, `(second '(1))`, `(second '())`],
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
    examples: [`(last '("Albert" "Mojir" 160 '(1 2)))`, `(last '(1 2))`, `(last '(1))`, `(last '())`],
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
      'Returns a new list with all but the first element from `list`. If `list` has less than two elements, undefined is returned.',
    examples: [`(rest '("Albert" "Mojir" 160 '(1 2)))`, `(rest '("Albert"))`, `(rest '())`],
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
    examples: [`(cons "Hi" '("Albert" "Mojir" 160 '(1 2)))`, `(cons "Hi" '())`, `(cons "Hi")`],
    specialExpression: false,
    sideEffects: [],
  },
  take: {
    name: 'take',
    category: 'List',
    linkName: 'take',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'list',
        type: 'list',
      },
      {
        name: 'count',
        type: 'integer',
      },
    ],
    shortDescription: 'Constructs a new list with the `count` first elements from `list`.',
    longDescription: 'Constructs a new list with the `count` first elements from `list`.',
    examples: [`(take '(1 2 3 4 5) 3)`, `(take '(1 2 3 4 5) 0)`],
    specialExpression: false,
    sideEffects: [],
  },

  'take-while': {
    name: 'take-while',
    category: 'List',
    linkName: 'take-while',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'predicate',
        type: 'function',
      },
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription:
      'Returns the members of `list` in order, stopping before the first one for which `predicate` returns a falsy value.',
    longDescription:
      'Returns the members of `list` in order, stopping before the first one for which `predicate` returns a falsy value.',
    examples: [`(take-while (lambda (x) (< x 3)) '(1 2 3 2 1))`, `(take-while (lambda (x) (> x 3)) '(1 2 3 2 1))`],
    specialExpression: false,
    sideEffects: [],
  },

  sort: {
    name: 'sort',
    category: 'List',
    linkName: 'sort',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'comparer',
        type: 'function',
      },
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns a new list with the elements from `list` sorted according to `comparer`.',
    longDescription: 'Returns a new list with the elements from `list` sorted according to `comparer`.',
    examples: [
      `(sort (lambda (a b) (cond ((< a b) -1) ((> a b) 1) (true -1))) '(3 1 2))`,
      `(sort (lambda (a b) (cond ((> a b) -1) ((< a b) 1) (true -1))) '(3 1 2))`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  join: {
    name: 'join',
    category: 'List',
    linkName: 'join',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'list',
        type: 'list of strings',
      },
      {
        name: 'delimiter',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string by concatenating all of the elements in `list`, separated by `delimiter`.',
    longDescription: 'Returns a new string by concatenating all of the elements in `list`, separated by `delimiter`.',
    examples: [`(join '("Albert" "Mojir") " ")`, `(join (map #'number-to-string '(0 1 2 3 4 5 6 7 8 9)) ", ")`],
    specialExpression: false,
    sideEffects: [],
  },
  includes: {
    name: 'includes',
    category: 'List',
    linkName: 'includes',
    returns: {
      type: 'true | false',
    },
    arguments: [
      {
        name: 'elem',
        type: 'any',
      },
      {
        name: 'list',
        type: 'list',
      },
    ],
    shortDescription: 'Returns `true` if `list` contains `elem`, otherwise returns `false`.',
    longDescription: 'Returns `true` if `list` contains `elem`, otherwise returns `false`.',
    examples: [
      `(includes "Mojir" '("Albert" "Mojir"))`,
      `(includes 42 '("Albert" "Mojir" 42))`,
      `(includes 43 '("Albert" "Mojir" 42))`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
}
