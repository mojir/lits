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
        type: ['sequence', 'nil'],
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
    description: 'Accesses element $n of $seq. Accessing out-of-bounds indices returns $not-found, if present, else `nil`.',
    examples: [
      '(nth [1 2 3] 1)',
      '(nth [1 2 3] 3)',
      '(nth [1 2 3] -1)',
      '(nth [1 2 3] 3 99)',
      '(nth "A string" 1)',
      '(nth "A string" 3)',
      '(nth "A string" -3)',
      '(nth "A string" 30 :X)',
      '(nth nil 1)',
      '(nth nil 1 "Default value")',
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
      type: ['sequence', 'nil'],
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
    description: 'Returns a copy of $seq with last element removed. If $seq is empty `nil` is returned.',
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
      type: ['sequence', 'nil'],
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
    description: 'Returns a copy of $seq with first element removed. If $seq is empty `nil` is returned.',
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
      end: {
        type: 'integer',
        description: 'Defaults lenght of sequence + 1.',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
      { argumentNames: ['seq', 'start'] },
      { argumentNames: ['seq', 'start', 'end'] },
    ],
    description: 'Returns a copy of a portion of $seq from index $start (inclusive) to $end (exclusive).',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Creates a new array populated with the results of calling $fn on every elements in $seq.',
    examples: [
      '(map str ["Albert" "Mojir" 42])',
      '(map str [])',
      '(map + [1 2 3] [1 2 3])',
      '(map max [2 6 3] [2 4 7] [1 6 2])',
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
      { argumentNames: ['fn', 'seq'] },
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
      type: ['number', 'nil'],
    },
    args: {
      fn: {
        type: 'function',
      },
      seq: {
        type: ['sequence', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns the index of the first elements that passes the test implemented by $fn. If no element was found, `nil` is returned.',
    examples: [
      `
(position
  string?
  ["Albert" "Mojir" 160 [1 2]])`,
      `
(position
  (fn [x] (> x 10))
  [5 10 15 20])`,
      `
(position
  (fn [x] (> x 100))
  [5 10 15 20])`,
      `
(position
  (fn [x] (> x 100))
  nil)`,
    ],
  },
  'index-of': {
    title: 'index-of',
    category: 'Sequence',
    linkName: 'index-of',
    clojureDocs: null,
    returns: {
      type: ['number', 'nil'],
    },
    args: {
      seq: {
        type: ['sequence', 'nil'],
      },
      x: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['seq', 'x'] },
    ],
    description: 'Returns the index of $x in $seq. If element is not present in $seq `nil` is returned.',
    examples: [
      '(index-of ["Albert" "Mojir" 160 [1 2]] "Mojir")',
      '(index-of [5 10 15 20] 15)',
      '(index-of [5 10 15 20] 1)',
      '(index-of nil 1)',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: ['sequence', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns the first element that passes the test implemented by $fn. I no element was found, `nil` is returned.',
    examples: [
      `
(some
  string?
  ["Albert" "Mojir" 160 [1 2]])`,
      `
(some
  (fn [x] (> x 10))
  [5 10 15 20])`,
      `
(some
  (fn [x] (> x 10))
  [1 2 3 4])`,
      `
(some
  (fn [x] (> x 10))
  [])`,
      `
(some
  (fn [x] (> x 10))
  nil)`,
    ],
  },
  'reverse': {
    title: 'reverse',
    category: 'Sequence',
    linkName: 'reverse',
    returns: {
      type: ['sequence', 'nil'],
    },
    args: {
      seq: {
        type: ['sequence', 'nil'],
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
      '(reverse nil)',
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
        type: ['sequence', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns the first element of $seq. If $seq is empty or `nil`, `nil` is returned.',
    examples: [
      '(first ["Albert" "Mojir" 160 [1 2]])',
      '(first [])',
      '(first nil)',
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
        type: ['sequence', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns the second element of $seq. If $seq has less than two elements or is `nil`, `nil` is returned.',
    examples: [
      '(second ["Albert" "Mojir" 160 [1 2]])',
      '(second [1])',
      '(second [])',
      '(second nil)',
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
        type: ['sequence', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns the last element of $seq. If $seq is empty, `nil` is returned.',
    examples: [
      '(last ["Albert" "Mojir" 160 [1 2]])',
      '(last [1 2])',
      '(last [1])',
      '(last [])',
      '(last nil)',
    ],
  },
  'rest': {
    title: 'rest',
    category: 'Sequence',
    linkName: 'rest',
    returns: {
      type: ['sequence', 'nil'],
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
      type: ['sequence', 'nil'],
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'If $seq is an array, returns a new array with all but the first element from $seq. If $seq has less than two elements, `nil` is returned. For string $seq returns all but the first characters in $seq. If length of string $seq is less than two, `nil` is returned.',
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
      type: ['sequence', 'nil'],
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
    description: 'If $seq is an array, returns a new array with all but the first $n elements from $seq. If $seq has less or equal than $n elements, `nil` returned. For string $seq returns all but the first $n characters in $seq. If length of string $seq is less or equal than $n, `nil` is returned.',
    examples: [
      '(nthnext ["Albert" "Mojir" 160 [1 2]] 2)',
      '(nthnext "Albert" 3)',
      '(nthnext "Albert" 6)',
      '(nthnext [] 0)',
      '(nthnext "" 0)',
    ],
  },
  'cons': {
    title: 'cons',
    category: 'Sequence',
    linkName: 'cons',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      x: {
        type: 'any',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['x', 'seq'] },
    ],
    description: 'Constructs a new array with $x as first element and $seq as the rest.',
    examples: [
      '(cons "Hi" ["Albert" "Mojir" 160 [1 2]])',
      '(cons "Hi" [])',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns the members of $seq in order, stopping before the first one for which `predicate` returns a falsy value.',
    examples: [
      `
(take-while
  (fn [x] (< x 3))
  [1 2 3 2 1])`,
      `
(take-while
  (fn [x] (> x 3))
  [1 2 3 2 1])`,
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
    description: 'Constructs a new array/string with the $n first elements dropped from $seq.',
    examples: [
      '(drop 3 [1 2 3 4 5])',
      '(drop 0 [1 2 3 4 5])',
      '(drop 2 "Albert")',
      '(drop 50 "Albert")',
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
    description: 'Constructs a new array with the $n last elements dropped from $seq.',
    examples: [
      '(drop-last 3 [1 2 3 4 5])',
      '(drop-last 0 [1 2 3 4 5])',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns the members of $seq in order, skipping the fist elements for witch the `predicate` returns a truethy value.',
    examples: [
      `
(drop-while
  (fn [x] (< x 3))
  [1 2 3 2 1])`,
      `
(drop-while
  (fn [x] (> x 3))
  [1 2 3 2 1])`,
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns a new sequence with the elements from $seq sorted according to $fn. If no $fn is supplied, builtin `compare` will be used.',
    examples: [
      '(sort [3 1 2])',
      `
(sort
  (fn [a b] (cond (< a b) -1 (> a b) 1 true -1))
  [3 1 2])`,
      `
(sort
  (fn [a b] (cond (> a b) -1 (< a b) 1 true -1))
  [3 1 2])`,
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
      keyfn: {
        type: 'function',
      },
      comp: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq', 'keyfn'] },
      { argumentNames: ['seq', 'keyfn', 'comp'] },
    ],
    description: 'Returns a sorted sequence of the items in $seq, where the sort order is determined by comparing `(keyfn item)`. If no $comp is supplied, uses builtin `compare`.',
    examples: [
      '(sort-by count ["Albert" "Mojir" "Nina"])',
      '(sort-by lower-case #(compare %2 %1) "Albert")',
    ],
  },
  'random-sample!': {
    title: 'random-sample!',
    category: 'Sequence',
    linkName: 'random-sample_exclamation',
    clojureDocs: 'random-sample',
    returns: {
      type: 'sequence',
    },
    args: {
      prob: {
        type: 'number',
        description: 'Probability of including an element from $seq. Between 0 and 1.',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['prob', 'seq'] },
    ],
    description: 'Each element from $seq has the probability $prob to be included in the result.',
    examples: [
      '(random-sample! 0.5 [1 2 3 4 5 6 7 8 9 10])',
      '(random-sample! 0.5 "Albert")',
    ],
  },
  'rand-nth!': {
    title: 'rand-nth!',
    category: 'Sequence',
    linkName: 'rand-nth_exclamation',
    clojureDocs: 'rand-nth',
    returns: {
      type: 'any',
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns a random element from $seq. Returns `nil` if $seq is empty.',
    examples: [
      '(rand-nth! [1 2 3 4 5 6 7 8 9 10])',
      '(rand-nth! "Albert")',
      '(rand-nth! [])',
    ],
  },
  'shuffle!': {
    title: 'shuffle!',
    category: 'Sequence',
    linkName: 'shuffle_exclamation',
    clojureDocs: 'shuffle',
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
    description: 'Returns a shuffled copy of $seq.',
    examples: [
      '(shuffle! [1 2 3 4 5 6 7 8 9 10])',
      '(shuffle! "Albert Mojir")',
      '(shuffle! [1 2])',
      '(shuffle! [1])',
      '(shuffle! [])',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns a new sequence of items in $seq for witch ``(pred item)`` returns a falsy value.',
    examples: [
      '(remove even? [1 2 3 1 3 5])',
      '(remove #(has? "aoueiyAOUEIY" %1) "Albert Mojir")',
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
      n: {
        type: 'number',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['n', 'seq'] },
    ],
    description: 'Returns a new sequence of all items in $seq except item at position $n.',
    examples: [
      '(remove-at 0 [1 2 3 1 3 5])',
      '(remove-at -1 [1 2 3 1 3 5])',
      '(remove-at 6 "Albert Mojir")',
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
      n: {
        type: 'number',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['n', 'seq'] },
    ],
    description: 'Returns a pair of sequence ``[(take pos input) (drop pos input)]``.',
    examples: [
      '(split-at 2 [1 2 3 4 5])',
      '(split-at 2 "Albert")',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns a pair of sequences ``[(take-while fn input) (drop-while fn input)]``.',
    examples: [
      '(split-with #(> %1 3) [1 2 3 4 5])',
      '(split-with #(<= %1 :Z) "Albert")',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Returns an object of the elements of $seq keyed by the result of $fn on each element. The value at each key will be an array of the corresponding elements.',
    examples: [
      '(group-by "name" [{"name" "Albert"} {"name" "Albert"} {"name" "Mojir"}])',
      '(group-by (fn [char] (if (has? "aoueiAOUEI" char) "vowel" "other")) "Albert Mojir")',
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
      n: {
        type: 'number',
      },
      step: {
        type: 'number',
      },
      pad: {
        type: 'array',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['n', 'seq'] },
      { argumentNames: ['n', 'step', 'seq'] },
      { argumentNames: ['n', 'step', 'pad', 'seq'] },
    ],
    description: 'Returns an array of sequences of $n items each, at offsets $step apart. If $step is not supplied, defaults to $n. If a $pad array is supplied, use its elements as necessary to complete last partition upto $n items. In case there are not enough padding elements, return a partition with less than $n items.',
    examples: [
      '(partition 4 (range 20))',
      '(partition 4 (range 22))',
      '(partition 4 6 (range 20))',
      '(partition 4 3 (range 20))',
      '(partition 3 6 [:a] (range 20))',
      '(partition 4 6 [:a] (range 20))',
      '(partition 4 6 [:a :b :c :d] (range 20))',
      '(partition 3 1 [:a :b :c :d :e :f])',
      '(partition 10 [1 2 3 4])',
      '(partition 10 10 [1 2 3 4])',
      '(partition 10 10 [] [1 2 3 4])',
      '(partition 10 10 nil [1 2 3 4])',
      '(partition 5 "superfragilistic")',
      '(partition 5 5 nil "superfragilistic")',
      '(def foo [5 6 7 8]) (partition 2 1 foo foo)',
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
      n: {
        type: 'number',
      },
      step: {
        type: 'number',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['n', 'seq'] },
      { argumentNames: ['n', 'step', 'seq'] },
    ],
    description: 'Returns an array of sequences like partition, but may include partitions with fewer than n items at the end.',
    examples: [
      '(partition-all 4 [0 1 2 3 4 5 6 7 8 9])',
      '(partition 4 [0 1 2 3 4 5 6 7 8 9])',
      '(partition-all 2 4 [0 1 2 3 4 5 6 7 8 9])',
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
      fn: {
        type: 'function',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['fn', 'seq'] },
    ],
    description: 'Applies $fn to each value in $seq, splitting it each time $fn returns a new value. Returns an array of sequences.',
    examples: [
      '(partition-by #(= 3 %1) [1 2 3 4 5])',
      '(partition-by odd? [1 1 1 2 2 3 3])',
      '(partition-by identity "Leeeeeerrroyyy")',
    ],
  },
}
