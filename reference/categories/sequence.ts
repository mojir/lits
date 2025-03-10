import type { FunctionReference } from '..'
import type { SequenceApiName } from '../api'

export const sequenceReference: Record<SequenceApiName, FunctionReference<'Sequence'>> = {
  'nth': {
    title: 'nth',
    category: 'Sequence',
    linkName: 'nth',
    returns: {
      type: 'any',
    },
    args: {
      'seq': {
        type: ['sequence', 'null'],
      },
      'n': {
        type: 'integer',
      },
      'not-found': {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
      { argumentNames: ['seq', 'n', 'not-found'] },
    ],
    description: 'Accesses element $n of $seq. Accessing out-of-bounds indices returns $not-found, if present, else `null`.',
    examples: [
      '(nth [1 2 3] 1)',
      '(nth [1 2 3] 3)',
      '(nth [1 2 3] -1)',
      '(nth [1 2 3] 3 99)',
      '(nth "A string" 1)',
      '(nth "A string" 3)',
      '(nth "A string" -3)',
      '(nth "A string" 30 :X)',
      '(nth null 1)',
      '(nth null 1 "Default value")',
    ],
  },
  'push': {
    title: 'push',
    category: 'Sequence',
    linkName: 'push',
    clojureDocs: null,
    returns: {
      type: 'sequence',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
        array: true,
      },
      values: {
        type: 'any',
        rest: true,
        description: 'At least one.',
      },
    },
    variants: [
      { argumentNames: ['seq', 'values'] },
    ],
    description: 'Returns copy of $seq with $values added to the end of it.',
    examples: [
      '(push [1 2 3] 4)',
      '(push [1 2 3] 4 5 6)',
      '(def l [1 2 3]) (push l 4) l',
    ],
  },
  'pop': {
    title: 'pop',
    category: 'Sequence',
    linkName: 'pop',
    returns: {
      type: ['sequence', 'null'],
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
        array: true,
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns a copy of $seq with last element removed. If $seq is empty `null` is returned.',
    examples: [
      '(pop [1 2 3])',
      '(pop [])',
    ],
  },
  'unshift': {
    title: 'unshift',
    category: 'Sequence',
    linkName: 'unshift',
    clojureDocs: null,
    returns: {
      type: 'sequence',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
        array: true,
      },
      values: {
        type: 'any',
        array: true,
      },
    },
    variants: [
      { argumentNames: ['seq', 'values'] },
    ],
    description: 'Returns copy of $seq with $values added to the beginning.',
    examples: [
      '(unshift [1 2 3] 4)',
      '(unshift [1 2 3] 4 5 6)',
      `
(def l [1 2 3])
(unshift l 4)
l`,
    ],
  },
  'shift': {
    title: 'shift',
    category: 'Sequence',
    linkName: 'shift',
    clojureDocs: null,
    returns: {
      type: ['sequence', 'null'],
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
        array: true,
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns a copy of $seq with first element removed. If $seq is empty `null` is returned.',
    examples: [
      '(shift [1 2 3])',
      '(shift [])',
    ],
  },
  'slice': {
    title: 'slice',
    category: 'Sequence',
    linkName: 'slice',
    clojureDocs: null,
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
        array: true,
      },
      start: {
        type: 'integer',
        description: 'Defaults to `0`.',
      },
      stop: {
        type: 'integer',
        description: 'Defaults lenght of sequence + 1.',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
      { argumentNames: ['seq', 'start'] },
      { argumentNames: ['seq', 'start', 'stop'] },
    ],
    description: 'Returns a copy of a portion of $seq from index $start (inclusive) to $stop (exclusive).',
    examples: [
      '(slice [1 2 3 4 5] 2 4)',
      '(slice [1 2 3 4 5] 2)',
    ],
  },
  'reductions': {
    title: 'reductions',
    category: 'Sequence',
    linkName: 'reductions',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
        array: true,
      },
      start: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
      { argumentNames: ['fn', 'start', 'seq'] },
    ],
    description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $seq by $fn.',
    examples: [
      '(reductions + [1 2 3])',
      '(reductions + 10 [1 2 3])',
      '(reductions + 0 [])',
      `
(reductions
  (fn [result value] (+ result (if (even? value) value 0)))
  0
  [1 2 3 4 5 6 7 8 9])`,
    ],
  },
  'reduce': {
    title: 'reduce',
    category: 'Sequence',
    linkName: 'reduce',
    returns: {
      type: 'any',
    },
    args: {
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
      start: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
      { argumentNames: ['fn', 'start', 'seq'] },
    ],
    description: 'Runs $fn function on each element of the $seq, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $seq is a single value.',
    examples: [
      '(reduce + [1 2 3])',
      '(reduce + 0 [1 2 3])',
      '(reduce + 0 [])',
      `
(reduce
  (fn [result value] (+ result (if (even? value) value 0)))
  0
  [1 2 3 4 5 6 7 8 9])`,
    ],
  },
  'reduce-right': {
    title: 'reduce-right',
    category: 'Sequence',
    linkName: 'reduce-right',
    clojureDocs: null,
    returns: {
      type: 'sequence',
    },
    args: {
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
      start: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
      { argumentNames: ['fn', 'start', 'seq'] },
    ],
    description: 'Runs $fn function on each element of the $seq (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $seq is a single value.',
    examples: [
      '(reduce-right str [:A :B :C] "")',
    ],
  },
  'map': {
    title: 'map',
    category: 'Sequence',
    linkName: 'map',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Creates a new array populated with the results of calling $fn on every elements in $seq.',
    examples: [
      '(map ["Albert" "Mojir" 42] str)',
      '(map [1 2 3] inc)',
    ],
  },
  'filter': {
    title: 'filter',
    category: 'Sequence',
    linkName: 'filter',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Creates a new array with all elements that pass the test implemented by $fn.',
    examples: [
      `
(filter
  ["Albert" "Mojir" 160 [1 2]]
  string?)`,
      `
(filter
[5 10 15 20]
  (fn [x] (> x 10)))`,
    ],
  },
  'position': {
    title: 'position',
    category: 'Sequence',
    linkName: 'position',
    clojureDocs: null,
    returns: {
      type: ['number', 'null'],
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns the index of the first elements that passes the test implemented by $fn. If no element was found, `null` is returned.',
    examples: [
      `
(position
  ["Albert" "Mojir" 160 [1 2]]
  string?)`,
      `
(position
  [5 10 15 20]
  (fn [x] (> x 10)))`,
      `
(position
  [5 10 15 20]
  (fn [x] (> x 100)))`,
      `
(position
  (fn [x] (> x 100))
  null)`,
    ],
  },
  'index-of': {
    title: 'index-of',
    category: 'Sequence',
    linkName: 'index-of',
    clojureDocs: null,
    returns: {
      type: ['number', 'null'],
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['seq', 'x'] },
    ],
    description: 'Returns the index of $x in $seq. If element is not present in $seq `null` is returned.',
    examples: [
      '(index-of ["Albert" "Mojir" 160 [1 2]] "Mojir")',
      '(index-of [5 10 15 20] 15)',
      '(index-of [5 10 15 20] 1)',
      '(index-of null 1)',
    ],
  },
  'last-index-of': {
    title: 'last-index-of',
    category: 'Sequence',
    linkName: 'last-index-of',
    clojureDocs: null,
    returns: {
      type: ['number', 'null'],
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['seq', 'x'] },
    ],
    description: 'Returns the last index of $x in $seq. If element is not present in $seq `null` is returned.',
    examples: [
      '(last-index-of ["Albert" "Mojir" 160 [1 2]] "Mojir")',
      '(last-index-of [5 10 15 20] 15)',
      '(last-index-of [5 10 15 20] 1)',
      '(last-index-of null 1)',
    ],
  },
  'some': {
    title: 'some',
    category: 'Sequence',
    linkName: 'some',
    returns: {
      type: 'any',
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns the first element that passes the test implemented by $fn. I no element was found, `null` is returned.',
    examples: [
      `
(some
  ["Albert" "Mojir" 160 [1 2]]
  string?)`,
      `
(some
  [5 10 15 20]
  (fn [x] (> x 10)))`,
      `
(some
  [1 2 3 4]
  (fn [x] (> x 10)))`,
      `
(some
  []
  (fn [x] (> x 10)))`,
      `
(some
  null
  (fn [x] (> x 10)))`,
    ],
  },
  'reverse': {
    title: 'reverse',
    category: 'Sequence',
    linkName: 'reverse',
    returns: {
      type: ['sequence', 'null'],
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'If $seq is an array, creates a new array with the elements from $seq in reversed order. If $seq is a string, returns new reversed string.',
    examples: [
      '(reverse ["Albert" "Mojir" 160 [1 2]])',
      '(reverse [])',
      '(reverse "Albert")',
      '(reverse null)',
    ],
  },
  'first': {
    title: 'first',
    category: 'Sequence',
    linkName: 'first',
    returns: {
      type: 'any',
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns the first element of $seq. If $seq is empty or `null`, `null` is returned.',
    examples: [
      '(first ["Albert" "Mojir" 160 [1 2]])',
      '(first [])',
      '(first null)',
    ],
  },
  'second': {
    title: 'second',
    category: 'Sequence',
    linkName: 'second',
    returns: {
      type: 'any',
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns the second element of $seq. If $seq has less than two elements or is `null`, `null` is returned.',
    examples: [
      '(second ["Albert" "Mojir" 160 [1 2]])',
      '(second [1])',
      '(second [])',
      '(second null)',
    ],
  },
  'last': {
    title: 'last',
    category: 'Sequence',
    linkName: 'last',
    returns: {
      type: 'any',
    },
    args: {
      seq: {
        type: ['sequence', 'null'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns the last element of $seq. If $seq is empty, `null` is returned.',
    examples: [
      '(last ["Albert" "Mojir" 160 [1 2]])',
      '(last [1 2])',
      '(last [1])',
      '(last [])',
      '(last null)',
    ],
  },
  'rest': {
    title: 'rest',
    category: 'Sequence',
    linkName: 'rest',
    returns: {
      type: ['sequence', 'null'],
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: `If $seq is an array, returns a new array with all but the first element from $seq.
If $seq has less than two elements, an empty array is returned.
For string $seq returns all but the first characters in $seq.`,
    examples: [
      '(rest ["Albert" "Mojir" 160 [1 2]])',
      '(rest ["Albert"])',
      '(rest [])',
      '(rest "Albert")',
      '(rest :A)',
      '(rest "")',
    ],
  },
  'nthrest': {
    title: 'nthrest',
    category: 'Sequence',
    linkName: 'nthrest',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'If $seq is an array, returns a new array with all but the first $n elements from $seq. For string $seq returns all but the first $n characters in $seq.',
    examples: [
      '(nthrest ["Albert" "Mojir" 160 [1 2]] 2)',
      '(nthrest "Albert" 3)',
      '(nthrest "Albert" 10)',
      '(nthrest [] 0)',
      '(nthrest "" 0)',
    ],
  },
  'next': {
    title: 'next',
    category: 'Sequence',
    linkName: 'next',
    returns: {
      type: ['sequence', 'null'],
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'If $seq is an array, returns a new array with all but the first element from $seq. If $seq has less than two elements, `null` is returned. For string $seq returns all but the first characters in $seq. If length of string $seq is less than two, `null` is returned.',
    examples: [
      '(next ["Albert" "Mojir" 160 [1 2]])',
      '(next ["Albert"])',
      '(next [])',
      '(next "Albert")',
      '(next :A)',
      '(next "")',
    ],
  },
  'nthnext': {
    title: 'nthnext',
    category: 'Sequence',
    linkName: 'nthnext',
    returns: {
      type: ['sequence', 'null'],
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'If $seq is an array, returns a new array with all but the first $n elements from $seq. If $seq has less or equal than $n elements, `null` returned. For string $seq returns all but the first $n characters in $seq. If length of string $seq is less or equal than $n, `null` is returned.',
    examples: [
      '(nthnext ["Albert" "Mojir" 160 [1 2]] 2)',
      '(nthnext "Albert" 3)',
      '(nthnext "Albert" 6)',
      '(nthnext [] 0)',
      '(nthnext "" 0)',
    ],
  },
  'take': {
    title: 'take',
    category: 'Sequence',
    linkName: 'take',
    returns: {
      type: 'sequence',
    },
    args: {
      n: {
        type: 'integer',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['n', 'seq'] },
    ],
    description: 'Constructs a new array/string with the $n first elements from $seq.',
    examples: [
      '(take 3 [1 2 3 4 5])',
      '(take 0 [1 2 3 4 5])',
      '(take 2 "Albert")',
      '(take 50 "Albert")',
    ],
  },
  'take-last': {
    title: 'take-last',
    category: 'Sequence',
    linkName: 'take-last',
    returns: {
      type: 'sequence',
    },
    args: {
      n: {
        type: 'integer',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['n', 'seq'] },
    ],
    description: 'Constructs a new array with the $n last elements from $seq.',
    examples: [
      '(take-last 3 [1 2 3 4 5])',
      '(take-last 0 [1 2 3 4 5])',
    ],
  },
  'take-while': {
    title: 'take-while',
    category: 'Sequence',
    linkName: 'take-while',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns the members of $seq in order, stopping before the first one for which `predicate` returns a falsy value.',
    examples: [
      `
(take-while
  [1 2 3 2 1]
  (fn [x] (< x 3)))`,
      `
(take-while
  [1 2 3 2 1]
  (fn [x] (> x 3)))`,
    ],
  },
  'drop': {
    title: 'drop',
    category: 'Sequence',
    linkName: 'drop',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'Constructs a new array/string with the $n first elements dropped from $seq.',
    examples: [
      '(drop [1 2 3 4 5] 3)',
      '(drop [1 2 3 4 5] 0)',
      '(drop "Albert" 2)',
      '(drop "Albert" 50)',
    ],
  },
  'drop-last': {
    title: 'drop-last',
    category: 'Sequence',
    linkName: 'drop-last',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'Constructs a new array with the $n last elements dropped from $seq.',
    examples: [
      '(drop-last [1 2 3 4 5] 3)',
      '(drop-last [1 2 3 4 5] 0)',
    ],
  },
  'drop-while': {
    title: 'drop-while',
    category: 'Sequence',
    linkName: 'drop-while',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns the members of $seq in order, skipping the fist elements for witch the `predicate` returns a truethy value.',
    examples: [
      `
(drop-while
  [1 2 3 2 1]
  (fn [x] (< x 3)))`,
      `
(drop-while
  [1 2 3 2 1]
  (fn [x] (> x 3)))`,
    ],
  },
  'sort': {
    title: 'sort',
    category: 'Sequence',
    linkName: 'sort',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns a new sequence with the elements from $seq sorted according to $fn. If no $fn is supplied, builtin `compare` will be used.',
    examples: [
      '(sort [3 1 2])',
      `
(sort
  [3 1 2]
  (fn [a b] (cond (< a b) -1 (> a b) 1 true -1)))`,
      `
(sort
  [3 1 2]
  (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)))`,
    ],
  },
  'sort-by': {
    title: 'sort-by',
    category: 'Sequence',
    linkName: 'sort-by',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      keyfn: {
        type: 'function',
      },
      comp: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'keyfn'] },
      { argumentNames: ['seq', 'keyfn', 'comp'] },
    ],
    description: 'Returns a sorted sequence of the items in $seq, where the sort order is determined by comparing `(keyfn item)`. If no $comp is supplied, uses builtin `compare`.',
    examples: [
      '(sort-by ["Albert" "Mojir" "Nina"] count)',
      '(sort-by "Albert" lower-case #(compare %2 %1))',
    ],
  },
  'distinct': {
    title: 'distinct',
    category: 'Sequence',
    linkName: 'distinct',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns a copy of $seq with no duplicates.',
    examples: [
      '(distinct [1 2 3 1 3 5])',
      '(distinct "Albert Mojir")',
      '(distinct [])',
      '(distinct "")',
    ],
  },
  'remove': {
    title: 'remove',
    category: 'Sequence',
    linkName: 'remove',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns a new sequence of items in $seq for witch `pred(item)` returns a falsy value.',
    examples: [
      '(remove [1 2 3 1 3 5] even?)',
      '(remove "Albert Mojir" #(contains? "aoueiyAOUEIY" %1))',
    ],
  },
  'remove-at': {
    title: 'remove-at',
    category: 'Sequence',
    linkName: 'remove-at',
    clojureDocs: null,
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'Returns a new sequence of all items in $seq except item at position $n.',
    examples: [
      '(remove-at [1 2 3 1 3 5] 0)',
      '(remove-at [1 2 3 1 3 5] -1)',
      '(remove-at "Albert Mojir" 6)',
    ],
  },
  'split-at': {
    title: 'split-at',
    category: 'Sequence',
    linkName: 'split-at',
    returns: {
      type: 'sequence',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'Returns a pair of sequence `[take(pos input), drop(pos input)]`.',
    examples: [
      '(split-at [1 2 3 4 5] 2)',
      '(split-at "Albert" 2)',
    ],
  },
  'split-with': {
    title: 'split-with',
    category: 'Sequence',
    linkName: 'split-with',
    returns: {
      type: 'sequence',
      array: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns a pair of sequences `[take-while(input, fn), drop-while(input, fn)]`.',
    examples: [
      '(split-with [1 2 3 4 5] #(> %1 3))',
      '(split-with "Albert" #(<= %1 :Z))',
    ],
  },
  'frequencies': {
    title: 'frequencies',
    category: 'Sequence',
    linkName: 'frequencies',
    returns: {
      type: 'object',
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns an object from distinct items in $seq to the number of times they appear. Note that all items in $seq must be valid object keys i.e. strings.',
    examples: [
      '(frequencies ["Albert" "Mojir" "Nina" "Mojir"])',
      '(frequencies "Pneumonoultramicroscopicsilicovolcanoconiosis")',
    ],
  },
  'group-by': {
    title: 'group-by',
    category: 'Sequence',
    linkName: 'group-by',
    returns: {
      type: 'object',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Returns an object of the elements of $seq keyed by the result of $fn on each element. The value at each key will be an array of the corresponding elements.',
    examples: [
      '(group-by [{"name" "Albert"} {"name" "Albert"} {"name" "Mojir"}] "name")',
      '(group-by "Albert Mojir" (fn [char] (if (contains? "aoueiAOUEI" char) "vowel" "other")))',
    ],
  },
  'partition': {
    title: 'partition',
    category: 'Sequence',
    linkName: 'partition',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'number',
      },
      step: {
        type: 'number',
      },
      pad: {
        type: 'array',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
      { argumentNames: ['seq', 'n', 'step'] },
      { argumentNames: ['seq', 'n', 'step', 'pad'] },
    ],
    description: 'Returns an array of sequences of $n items each, at offsets $step apart. If $step is not supplied, defaults to $n. If a $pad array is supplied, use its elements as necessary to complete last partition upto $n items. In case there are not enough padding elements, return a partition with less than $n items.',
    examples: [
      '(partition (range 20) 4)',
      '(partition (range 22) 4)',
      '(partition (range 20) 4 6)',
      '(partition (range 20) 4 3)',
      '(partition (range 20) 3 6 [:a])',
      '(partition (range 20) 4 6 [:a])',
      '(partition (range 20) 4 6 [:a :b :c :d])',
      '(partition [:a :b :c :d :e :f] 3 1)',
      '(partition [1 2 3 4] 10)',
      '(partition [1 2 3 4] 10 10)',
      '(partition [1 2 3 4] 10 10 [])',
      '(partition [1 2 3 4] 10 10 null)',
      '(partition "superfragilistic" 5)',
      '(partition "superfragilistic" 5 5 null)',
      '(def foo [5 6 7 8]) (partition foo 2 1 foo)',
    ],
  },
  'partition-all': {
    title: 'partition-all',
    category: 'Sequence',
    linkName: 'partition-all',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      n: {
        type: 'number',
      },
      step: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
      { argumentNames: ['seq', 'n', 'step'] },
    ],
    description: 'Returns an array of sequences like partition, but may include partitions with fewer than n items at the end.',
    examples: [
      '(partition-all [0 1 2 3 4 5 6 7 8 9] 4)',
      '(partition [0 1 2 3 4 5 6 7 8 9] 4)',
      '(partition-all [0 1 2 3 4 5 6 7 8 9] 2 4)',
    ],
  },
  'partition-by': {
    title: 'partition-by',
    category: 'Sequence',
    linkName: 'partition-by',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fn'] },
    ],
    description: 'Applies $fn to each value in $seq, splitting it each time $fn returns a new value. Returns an array of sequences.',
    examples: [
      '(partition-by [1 2 3 4 5] #(= 3 %1))',
      '(partition-by [1 1 1 2 2 3 3] odd?)',
      '(partition-by "Leeeeeerrroyyy" identity)',
    ],
  },
  'starts-with?': {
    title: 'starts-with?',
    category: 'Sequence',
    linkName: 'starts-with-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      prefix: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq', 'prefix'] },
    ],
    description: 'Returns `true` if $seq starts with $prefix, otherwise `false`.',
    examples: [
      '(starts-with? [1 2 3 4 5] 1)',
      '(starts-with? [1 2 3 4 5] [1])',
      '(starts-with? "Albert" "Al")',
      '(starts-with? "Albert" "al")',
    ],
  },
  'ends-with?': {
    title: 'ends-with?',
    category: 'Sequence',
    linkName: 'ends-with-question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      suffix: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq', 'suffix'] },
    ],
    description: 'Returns `true` if $seq ends with $suffix, otherwise `false`.',
    examples: [
      '(ends-with? [1 2 3 4 5] 5)',
      '(ends-with? [1 2 3 4 5] [5])',
      '(ends-with? "Albert" "rt")',
      '(ends-with? "Albert" "RT")',
    ],
  },
  'interleave': {
    title: 'interleave',
    category: 'Sequence',
    linkName: 'interleave',
    clojureDocs: null,
    returns: {
      type: 'sequence',
    },
    args: {
      seqs: {
        type: 'sequence',
        array: true,
      },
    },
    variants: [
      { argumentNames: ['seqs'] },
    ],
    description: 'Returns a sequence of the first item from each of the $seqs, then the second item from each of the $seqs, until all items from the shortest seq are exhausted.',
    examples: [
      '(interleave [1 2 3] [4 5 6])',
      '(interleave [1 2 3] [4 5 6] [7 8 9])',
      '(interleave [1 2 3] [4 5 6] [7 8])',
      '(interleave [1 2 3] [4 5 6] [7])',
      '(interleave [1 2 3] [4 5 6] [])',
      '(interleave [1 2 3] [])',
      '(interleave [])',
    ],
  },
  'interpose': {
    title: 'interpose',
    category: 'Sequence',
    linkName: 'interpose',
    clojureDocs: null,
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
      },
      separator: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['seq', 'separator'] },
    ],
    description: 'Returns a sequence of the elements of $seq separated by $separator. If $seq is a string, the separator must be a string.',
    examples: [
      '(interpose :a [1 2 3 4 5])',
      '(interpose " " ["Albert" "Mojir" "Nina"])',
      '(interpose "." "Albert")',
    ],
  },
}
