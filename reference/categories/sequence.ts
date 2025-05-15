import type { FunctionReference } from '..'
import { type SequenceApiName, getOperatorArgs } from '../api'

export const sequenceReference: Record<SequenceApiName, FunctionReference<'Sequence'>> = {
  'nth': {
    title: 'nth',
    category: 'Sequence',
    linkName: 'nth',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('sequence', 'integer'),
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
      '[1, 2, 3] nth 1',
      '"A string" nth 3',
      'nth([1, 2, 3], 1)',
      'nth([1, 2, 3], 3)',
      'nth([1, 2, 3], -1)',
      'nth([1, 2, 3], 3, 99)',
      'nth("A string", 1)',
      'nth("A string", 3)',
      'nth("A string", -3)',
      'nth("A string", 30, "X")',
      'nth(null, 1)',
      'nth(null, 1, "Default value")',
    ],
  },
  'push': {
    title: 'push',
    category: 'Sequence',
    linkName: 'push',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'any'),
      seq: {
        type: 'sequence',
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
      '[1, 2, 3] push 4',
      '"Albert" push "!"',
      'push([1, 2, 3], 4)',
      'push([1, 2, 3], 4, 5, 6)',
      `
let l = [1, 2, 3];
push(l, 4);
l`,
    ],
  },
  'pop': {
    title: 'pop',
    category: 'Sequence',
    linkName: 'pop',
    returns: {
      type: ['sequence', 'null'],
      rest: true,
    },
    args: {
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
    ],
    description: 'Returns a copy of $seq with last element removed. If $seq is empty `null` is returned.',
    examples: [
      'pop([1, 2, 3])',
      'pop([])',
    ],
  },
  'unshift': {
    title: 'unshift',
    category: 'Sequence',
    linkName: 'unshift',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'any'),
      seq: {
        type: 'sequence',
      },
      values: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['seq', 'values'] },
    ],
    description: 'Returns copy of $seq with $values added to the beginning.',
    examples: [
      '[1, 2, 3] unshift 4',
      'unshift([1, 2, 3], 4)',
      'unshift([1, 2, 3], 4, 5, 6)',
      `
let l = [1, 2, 3];
unshift(l, 4);
l`,
    ],
  },
  'shift': {
    title: 'shift',
    category: 'Sequence',
    linkName: 'shift',
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
    description: 'Returns a copy of $seq with first element removed. If $seq is empty `null` is returned.',
    examples: [
      'shift([1, 2, 3])',
      'shift([])',
    ],
  },
  'slice': {
    title: 'slice',
    category: 'Sequence',
    linkName: 'slice',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'integer'),
      seq: {
        type: 'sequence',
        rest: true,
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
      '[1, 2, 3, 4, 5] slice 2',
      'slice([1, 2, 3, 4, 5], 2, 4)',
      'slice([1, 2, 3, 4, 5], 2)',
    ],
  },
  'splice': {
    title: 'splice',
    category: 'Sequence',
    linkName: 'splice',
    returns: {
      type: 'sequence',
    },
    args: {
      seq: {
        type: 'sequence',
        rest: true,
      },
      start: {
        type: 'integer',
      },
      deleteCount: {
        type: 'integer',
      },
      items: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['seq', 'start', 'deleteCount'] },
      { argumentNames: ['seq', 'start', 'deleteCount', 'items'] },
    ],
    description: 'Returns a a spliced array. Removes $deleteCount elements from $seq starting at $start and replaces them with $items. If $start is negative, it is counting from the end of the array.',
    examples: [
      'splice([1, 2, 3, 4, 5], 2, 2, "x")',
      'splice([1, 2, 3, 4, 5], -2, 1, "x")',
      'splice("Albert", 2, 2, "fo")',
    ],
  },
  'position': {
    title: 'position',
    category: 'Sequence',
    linkName: 'position',
    returns: {
      type: ['number', 'null'],
    },
    args: {
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: ['sequence', 'null'],
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns the index of the first elements that passes the test implemented by $fun. If no element was found, `null` is returned.',
    examples: [
      `
position(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
      `
position(
  [5, 10, 15, 20],
  -> $ > 10
)`,
      `
position(
  [5, 10, 15, 20],
  -> $ > 100
)`,
      `
position(
  null,
  -> $ > 100
)`,
    ],
  },
  'index-of': {
    title: 'index-of',
    category: 'Sequence',
    linkName: 'index-of',
    returns: {
      type: ['number', 'null'],
    },
    args: {
      ...getOperatorArgs('sequence', 'any'),
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
      '[[1], [2], [1], [2]] index-of [1]',
      'index-of(["Albert", "Mojir", 160, [1, 2]], "Mojir")',
      'index-of([5, 10, 15, 20], 15)',
      'index-of([5, 10, 15, 20], 1)',
      'index-of(null, 1)',
    ],
  },
  'last-index-of': {
    title: 'last-index-of',
    category: 'Sequence',
    linkName: 'last-index-of',
    returns: {
      type: ['number', 'null'],
    },
    args: {
      ...getOperatorArgs('sequence', 'any'),
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
      '[[1], [2], [1], [2]] last-index-of [1]',
      'last-index-of(["Albert", "Mojir", 160, [1, 2]], "Mojir")',
      'last-index-of([5, 10, 15, 20, 15], 15)',
      'last-index-of([5, 10, 15, 20], 1)',
      'last-index-of(null, 1)',
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
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: ['sequence', 'null'],
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns the first element that passes the test implemented by $fun. I no element was found, `null` is returned.',
    examples: [
      `
some(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
      `
some(
  [5, 10, 15, 20],
  -> $ > 10
)`,
      `
some(
  [1, 2, 3, 4],
  -> $ > 10
)`,
      `
some(
  [],
  -> $ > 10
)`,
      `
some(
  null,
  -> $ > 10
)`,
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
      'reverse(["Albert", "Mojir", 160, [1, 2]])',
      'reverse([])',
      'reverse("Albert")',
      'reverse(null)',
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
      'first(["Albert", "Mojir", 160, [1, 2]])',
      'first([])',
      'first(null)',
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
      'second(["Albert", "Mojir", 160, [1, 2]])',
      'second([1])',
      'second([])',
      'second(null)',
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
      'last(["Albert", "Mojir", 160, [1, 2]])',
      'last([1, 2])',
      'last([1])',
      'last([])',
      'last(null)',
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
      'rest(["Albert", "Mojir", 160, [1, 2]])',
      'rest(["Albert"])',
      'rest([])',
      'rest("Albert")',
      'rest("A",)',
      'rest("")',
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
      'next(["Albert", "Mojir", 160, [1, 2]])',
      'next(["Albert"])',
      'next([])',
      'next("Albert")',
      'next("A",)',
      'next("")',
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
      ...getOperatorArgs('sequence', 'integer'),
      n: {
        type: 'integer',
      },
      seq: {
        type: 'sequence',
      },
    },
    variants: [
      { argumentNames: ['seq', 'n'] },
    ],
    description: 'Constructs a new array/string with the $n first elements from $seq.',
    examples: [
      '[1, 2, 3, 4, 5] take 3',
      'take([1, 2, 3, 4, 5], 3)',
      'take([1, 2, 3, 4, 5], 0)',
      'take("Albert", 2)',
      'take("Albert", 50)',
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
      ...getOperatorArgs('sequence', 'integer'),
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
      '[1, 2, 3, 4, 5] take-last 3',
      'take-last([1, 2, 3, 4, 5], 3)',
      'take-last([1, 2, 3, 4, 5], 0)',
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
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns the members of $seq in order, stopping before the first one for which `predicate` returns a falsy value.',
    examples: [
      `
take-while(
  [1, 2, 3, 2, 1],
  -> $ < 3
)`,
      `
take-while(
  [1, 2, 3, 2, 1],
  -> $ > 3
)`,
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
      ...getOperatorArgs('sequence', 'integer'),
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
      'drop([1, 2, 3, 4, 5], 3)',
      'drop([1, 2, 3, 4, 5], 0)',
      'drop("Albert", 2)',
      'drop("Albert", 50)',
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
      ...getOperatorArgs('sequence', 'integer'),
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
      '[1, 2, 3, 4, 5] drop-last 3',
      'drop-last([1, 2, 3, 4, 5], 3)',
      'drop-last([1, 2, 3, 4, 5], 0)',
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
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns the members of $seq in order, skipping the fist elements for witch the `predicate` returns a truethy value.',
    examples: [
      `
drop-while(
  [1, 2, 3, 2, 1],
  -> $ < 3
)`,
      `
drop-while(
  [1, 2, 3, 2, 1],
  -> $ > 3
)`,
    ],
  },
  'sort': {
    title: 'sort',
    category: 'Sequence',
    linkName: 'sort',
    returns: {
      type: 'any',
      rest: true,
    },
    args: {
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq'] },
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns a new sequence with the elements from $seq sorted according to $fun. If no $fun is supplied, builtin `compare` will be used.',
    examples: [
      '[3, 1, 2] sort (a, b) -> b - a',
      'sort([3, 1, 2])',
      `
sort(
  [3, 1, 2],
  (a, b) -> cond { case a < b: -1 case a > b: 1 case true: -1 }
)`,
      `
sort(
  [3, 1, 2],
  (a, b) -> cond { case a > b: -1 case a < b: 1 case true: -1 }
)`,
    ],
  },
  'sort-by': {
    title: 'sort-by',
    category: 'Sequence',
    linkName: 'sort-by',
    returns: {
      type: 'any',
      rest: true,
    },
    args: {
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      keyfn: {
        type: 'function',
      },
      comparer: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'keyfn'] },
      { argumentNames: ['seq', 'keyfn', 'comparer'] },
    ],
    description: 'Returns a sorted sequence of the items in $seq, where the sort order is determined by comparing `(keyfn item)`. If no $comparer is supplied, uses builtin `compare`.',
    examples: [
      '["Albert", "Mojir", "Nina"] sort-by count',
      'sort-by(["Albert", "Mojir", "Nina"], count)',
      'sort-by("Albert", lower-case, -> $2 compare $1)',
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
      'distinct([[1], [2], [3], [1], [3], [5]])',
      'distinct([1, 2, 3, 1, 3, 5])',
      'distinct("Albert Mojir")',
      'distinct([])',
      'distinct("")',
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
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns a new sequence of items in $seq for witch `pred(item)` returns a falsy value.',
    examples: [
      '[1, 2, 3, 1, 3, 5] remove odd?',
      'remove([1, 2, 3, 1, 3, 5], even?)',
      'remove("Albert Mojir", -> "aoueiyAOUEIY" contains? $)',
    ],
  },
  'remove-at': {
    title: 'remove-at',
    category: 'Sequence',
    linkName: 'remove-at',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'integer'),
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
    description: 'Returns a new sequence of all items in $seq except item at position $n. If $n is negative, it is counting from the end of the sequence.',
    examples: [
      '[1, 2, 3, 1, 3, 5] remove-at 2',
      '"Albert" remove-at -2',
      'remove-at([1, 2, 3, 1, 3, 5], 0)',
      'remove-at([1, 2, 3, 1, 3, 5], -1)',
      'remove-at("Albert Mojir", 6)',
    ],
  },
  'split-at': {
    title: 'split-at',
    category: 'Sequence',
    linkName: 'split-at',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'integer'),
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
      '[1, 2, 3, 4, 5] split-at 2',
      '"Albert" split-at -2',
      'split-at([1, 2, 3, 4, 5], -2)',
      'split-at("Albert", 2)',
    ],
  },
  'split-with': {
    title: 'split-with',
    category: 'Sequence',
    linkName: 'split-with',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns a pair of sequences `[take-while(input, fun), drop-while(input, fun)]`.',
    examples: [
      '[1, 2, 3, 4, 5] split-with odd?',
      'split-with([1, 2, 3, 4, 5], -> $ > 3)',
      'split-with("Albert", -> $ <= "o")',
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
      'frequencies(["Albert", "Mojir", "Nina", "Mojir"])',
      'frequencies("Pneumonoultramicroscopicsilicovolcanoconiosis")',
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
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Returns an object of the elements of $seq keyed by the result of $fun on each element. The value at each key will be an array of the corresponding elements.',
    examples: [
      '[{ name: "Albert" }, { name: "Albert" }, { name: "Mojir" }] group-by "name"',
      'group-by([{name: "Albert"}, {name: "Albert"}, {name: "Mojir"}], "name")',
      'group-by("Albert Mojir", -> "aoueiAOUEI" contains? $ ? "vowel" : "other")',
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
      ...getOperatorArgs('sequence', 'number'),
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
      'range(20) partition 4',
      'partition(range(20), 4)',
      'partition(range(22), 4)',
      'partition(range(20), 4, 6)',
      'partition(range(20), 4, 3)',
      'partition(range(20), 3, 6, ["a"])',
      'partition(range(20), 4, 6, ["a"])',
      'partition(range(20), 4, 6, ["a", "b", "c", "d"])',
      'partition(["a", "b", "c", "d", "e", "f"], 3, 1)',
      'partition([1, 2, 3, 4], 10)',
      'partition([1, 2, 3, 4], 10, 10)',
      'partition([1, 2, 3, 4], 10, 10, [])',
      'partition([1, 2, 3, 4], 10, 10, null)',
      'partition("superfragilistic", 5)',
      'partition("superfragilistic", 5, 5, null)',
      'let foo = [5, 6, 7, 8]; partition(foo, 2, 1, foo)',
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
      ...getOperatorArgs('sequence', 'number'),
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
      '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9] partition-all 4',
      'partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
      'partition([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
      'partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 2, 4)',
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
      ...getOperatorArgs('sequence', 'function'),
      seq: {
        type: 'sequence',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['seq', 'fun'] },
    ],
    description: 'Applies $fun to each value in $seq, splitting it each time $fun returns a new value. Returns an array of sequences.',
    examples: [
      '[1, 2, 3, 4, 5] partition-by odd?',
      'partition-by([1, 2, 3, 4, 5], -> $ == 3)',
      'partition-by([1, 1, 1, 2, 2, 3, 3], odd?)',
      'partition-by("Leeeeeerrroyyy", identity)',
    ],
  },
  'starts-with?': {
    title: 'starts-with?',
    category: 'Sequence',
    linkName: 'starts-with-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('sequence', 'sequence'),
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
      '[[1], [2], [3], [4], [5]] starts-with? [1]',
      'starts-with?([1, 2, 3, 4, 5], 1)',
      'starts-with?([1, 2, 3, 4, 5], [1])',
      'starts-with?("Albert", "Al")',
      'starts-with?("Albert", "al")',
    ],
  },
  'ends-with?': {
    title: 'ends-with?',
    category: 'Sequence',
    linkName: 'ends-with-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('sequence', 'sequence'),
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
      '[[1], [2], [3], [4], [5]] starts-with? [5]',
      '[[1], [2], [3], [4], [5]] starts-with? 5',
      'ends-with?([1, 2, 3, 4, 5], 5)',
      'ends-with?([1, 2, 3, 4, 5], [5])',
      'ends-with?("Albert", "rt")',
      'ends-with?("Albert", "RT")',
    ],
  },
  'interleave': {
    title: 'interleave',
    category: 'Sequence',
    linkName: 'interleave',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'sequence'),
      seqs: {
        type: 'sequence',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['seqs'] },
    ],
    description: 'Returns a sequence of the first item from each of the $seqs, then the second item from each of the $seqs, until all items from the shortest seq are exhausted.',
    examples: [
      '[1, 2, 3] interleave [4, 5, 6]',
      '"Albert" interleave ".,.,.,"',
      'interleave([1, 2, 3], [4, 5, 6])',
      'interleave([1, 2, 3], [4, 5, 6], [7, 8, 9])',
      'interleave([1, 2, 3], [4, 5, 6], [7, 8])',
      'interleave([1, 2, 3], [4, 5, 6], [7])',
      'interleave([1, 2, 3], [4, 5, 6], [])',
      'interleave([1, 2, 3], [])',
      'interleave([])',
    ],
  },
  'interpose': {
    title: 'interpose',
    category: 'Sequence',
    linkName: 'interpose',
    returns: {
      type: 'sequence',
    },
    args: {
      ...getOperatorArgs('sequence', 'any'),
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
      '"Albert" interpose "-"',
      'interpose([1, 2, 3, 4, 5], "a")',
      'interpose(["Albert", "Mojir", "Nina"], ", ")',
      'interpose("Albert", ".")',
    ],
  },
}
