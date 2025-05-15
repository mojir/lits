import { type CollectionApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const collectionReference: Record<CollectionApiName, FunctionReference<'Collection'>> = {
  'filter': {
    title: 'filter',
    category: 'Collection',
    linkName: 'filter',
    returns: {
      type: 'collection',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
      coll: {
        type: 'collection',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun'] },
    ],
    description: 'Creates a new collection with all elements that pass the test implemented by $fun.',
    examples: [
      `
filter(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
      `
filter(
  [5, 10, 15, 20],
  -> $ > 10
)`,
      `
filter(
  { a: 1, b: 2 },
  odd?
)`,
    ],
  },
  'filteri': {
    title: 'filteri',
    category: 'Collection',
    linkName: 'filteri',
    returns: {
      type: 'collection',
    },
    args: {
      a: {
        type: 'collection',
      },
      b: {
        type: 'function',
        description: 'The function to call for each element in the collection. The function should take two arguments: the element itself and the index.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Creates a new collection with all elements that pass the test implemented by $b. The function is called for each element in the collection, and it should take two arguments: the element itself and the index.',
    examples: [
      'filteri([1, 2, 3], (x, i) -> i % 2 == 0)',
      'filteri([1, 2, 3], (x, i) -> x % 2 == 0)',
      'filteri([1, 2, 3], (x, i) -> x + i > 3)',
    ],
  },
  'map': {
    title: 'map',
    category: 'Collection',
    linkName: 'map',
    returns: {
      type: 'collection',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
      colls: {
        type: 'collection',
        rest: true,
        description: 'At least one.',
      },
      fun: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['colls', 'fun'] },
    ],
    description: 'Creates a new collection populated with the results of calling $fun on every element in $colls.',
    examples: [
      '[1, 2, 3] map -',
      '[1, 2, 3] map -> -($)',
      'map(["Albert", "Mojir", 42], str)',
      'map([1, 2, 3], inc)',
      'map([1, 2, 3], [1, 10, 100], *)',
      'map({ a: 1, b: 2 }, inc)',
      'map({ a: 1, b: 2 }, { a: 10, b: 20 }, +)',
    ],
  },
  'mapi': {
    title: 'mapi',
    category: 'Collection',
    linkName: 'mapi',
    returns: {
      type: 'collection',
    },
    args: {
      a: {
        type: 'collection',
      },
      b: {
        type: 'function',
        description: 'The function to call for each element in the collection. The function should take two arguments: the element itself and the index.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Creates a new collection populated with the results of calling $b on every element in $a. The function is called for each element in the collection, and it should take two arguments: the element itself and the index.',
    examples: [
      'mapi([1, 2, 3], (x, i) -> x + i)',
      'mapi([1, 2, 3], (x, i) -> x * i)',
      'mapi([1, 2, 3], (x, i) -> x - i)',
      'mapi([1, 2, 3], (x, i) -> x / i)',
      'mapi([1, 2, 3], (x, i) -> x % inc(i))',
    ],
  },
  'reduce': {
    title: 'reduce',
    category: 'Collection',
    linkName: 'reduce',
    returns: {
      type: 'any',
    },
    args: {
      fun: {
        type: 'function',
      },
      coll: {
        type: 'collection',
      },
      initial: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun', 'initial'] },
    ],
    description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
    examples: [
      'reduce([1, 2, 3], +, 0)',
      'reduce([], +, 0)',
      'reduce({ a: 1, b: 2 }, +, 0)',
      `
reduce(
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  (result, value) -> result + (even?(value) ? value : 0),
  0)`,
    ],
  },
  'reduce-right': {
    title: 'reduce-right',
    category: 'Collection',
    linkName: 'reduce-right',
    returns: {
      type: 'any',
    },
    args: {
      fun: {
        type: 'function',
      },
      coll: {
        type: 'collection',
      },
      initial: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun', 'initial'] },
    ],
    description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
    examples: [
      'reduce-right(["A", "B", "C"], str, "")',
      'reduce-right({ a: 1, b: 2 }, +, 0)',
    ],
  },
  'reducei-right': {
    title: 'reducei-right',
    category: 'Collection',
    linkName: 'reducei-right',
    returns: {
      type: 'any',
    },
    args: {
      coll: {
        type: 'collection',
      },
      fun: {
        type: 'function',
        description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
      },
      initial: {
        type: 'any',
        description: 'The initial value to use as the accumulator.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun', 'initial'] },
    ],
    description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
    examples: [
      'reducei-right([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
      'reducei-right("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
      'reducei-right({ a: 1, b: 2 }, -> $1 ++ $3, "")',
    ],
  },
  'reducei': {
    title: 'reducei',
    category: 'Collection',
    linkName: 'reducei',
    returns: {
      type: 'any',
    },
    args: {
      coll: {
        type: 'collection',
      },
      fun: {
        type: 'function',
        description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
      },
      initial: {
        type: 'any',
        description: 'The initial value to use as the accumulator.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun', 'initial'] },
    ],
    description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
    examples: [
      'reducei([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
      'reducei("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
      'reducei({ a: 1, b: 2 }, -> $1 ++ $3, "")',
    ],
  },
  'reductions': {
    title: 'reductions',
    category: 'Collection',
    linkName: 'reductions',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      fun: {
        type: 'function',
      },
      coll: {
        type: 'collection',
      },
      initial: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun', 'initial'] },
    ],
    description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun.',
    examples: [
      'reductions([1, 2, 3], +, 0)',
      'reductions([1, 2, 3], +, 10)',
      'reductions([], +, 0)',
      'reductions({ a: 1, b: 2 }, +, 0)',
      `
reductions(
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  (result, value) -> result + (even?(value) ? value : 0),
  0
)`,
    ],
  },
  'reductionsi': {
    title: 'reductionsi',
    category: 'Collection',
    linkName: 'reductionsi',
    returns: {
      type: 'any',
      array: true,
    },
    args: {
      coll: {
        type: 'collection',
      },
      fun: {
        type: 'function',
        description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
      },
      initial: {
        type: 'any',
        description: 'The initial value to use as the accumulator.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fun', 'initial'] },
    ],
    description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
    examples: [
      'reductionsi([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
      'reductionsi("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
      'reductionsi({ a: 1, b: 2 }, -> $1 ++ $3, "")',
    ],
  },
  'count': {
    title: 'count',
    category: 'Collection',
    linkName: 'count',
    returns: {
      type: 'number',
    },
    args: {
      coll: {
        type: ['collection', 'null'],
      },
    },
    variants: [
      { argumentNames: ['coll'] },
    ],
    description: 'Returns number of elements in $coll.',
    examples: [
      'count([1, 2, 3])',
      'count([])',
      'count({ a: 1 })',
      'count("")',
      'count("Albert")',
      'count(null)',
    ],
  },
  'get': {
    title: 'get',
    category: 'Collection',
    linkName: 'get',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('collection', ['string', 'integer']),
      'not-found': {
        type: 'any',
        description: 'Default value to return if $b is not found.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'not-found'] },
    ],
    description: 'Returns value in $a mapped at $b.',
    examples: [
      '[1, 2, 3] get 1',
      '{ a: 1 } get "a"',
      '"Albert" get "3"',
      `
get(
  [1, 2, 3],
  1, // Optional comma after last argument
)`,
      `
get(
  [],
  1
)`,
      `
get(
  [],
  1,
  "default"
)`,
      `
get(
  { a: 1 },
  "a"
)`,
      `
get(
  { a: 1 },
  "b"
)`,
      `
get(
  { a: 1 },
  "b",
  "default"
)`,
      `
get(
  null,
  "a"
)`,
      `
get(
  null,
  "b",
  "default"
)`,
    ],
  },
  'get-in': {
    title: 'get-in',
    category: 'Collection',
    linkName: 'get-in',
    returns: {
      type: 'any',
    },
    args: {
      ...getOperatorArgs('collection', 'array'),
      'not-found': {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
      { argumentNames: ['a', 'b', 'not-found'] },
    ],
    description: 'Returns the value in a nested collection, where $b is an array of keys. Returns $not-found if the key is not present. If $not-found is not set, `null` is returned.',
    examples: [
      `
get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "a", 0]
)`,
      `
get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0]
)`,
      `
get-in(
  [[1, 2, 3], [4, { a: "Kalle" }, 6]],
  [1, 1, "b", 0],
  "Lisa"
)`,
    ],
  },
  'contains?': {
    title: 'contains?',
    category: 'Collection',
    linkName: 'contains-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs(['collection', 'null'], ['string', 'integer']),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns `true` if $a contains $b, otherwise returns `false`. For strings, it checks if substring is included.',
    examples: [
      '[1, 2, 3] contains? 1',
      'null contains? 1',
      '{ a: 1, b: 2 } contains? "a"',
      `
contains?(
  [],
  1
)`,
      `
contains?(
  [1],
  1
)`,
      `
contains?(
  [1, 2, 3],
  1
)`,
      `
contains?(
  {},
  "a"
)`,
      `
contains?(
  { a: 1, b: 2 },
  "a"
)`,
    ],
  },
  'assoc': {
    title: 'assoc',
    category: 'Collection',
    linkName: 'assoc',
    returns: {
      type: 'collection',
    },
    args: {
      coll: {
        type: 'collection',
      },
      key: {
        type: ['string', 'number'],
      },
      value: {
        type: 'any',
      },
      kvs: {
        type: 'any',
        description: 'Key-value pairs to associate.',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['coll', 'key', 'value'] },
      { argumentNames: ['coll', 'key', 'value', 'kvs'] },
    ],
    description: `
Add or replace the value of element $key to $value in $coll. Repeated for all key-value pairs in $kvs.  
If $coll is an \'array\', $key must be \`number\` satisfying \`0 <=\` $key \`<= length\`.`,
    examples: [
      `
assoc(
  [1, 2, 3],
  1,
  "Two"
)`,
      `
assoc(
  [1, 2, 3],
  3,
  "Four"
)`,
      `
assoc(
  { a: 1, b: 2 },
  "a",
  "One")`,
      `
assoc(
  { a: 1, b: 2 },
  "c",
  "Three")`,
      `
assoc(
  "Albert",
  6,
  "a")`,
    ],
  },
  'assoc-in': {
    title: 'assoc-in',
    category: 'Collection',
    linkName: 'assoc-in',
    returns: {
      type: 'collection',
    },
    args: {
      coll: {
        type: 'collection',
      },
      ks: {
        type: ['number', 'string'],
        array: true,
      },
      value: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['coll', 'ks', 'value'] },
    ],
    description: `
Associates a value in the nested collection $coll, where $ks is an array of keys and $value is the new value.

If any levels do not exist, objects will be created - and the corresponding keys must be of type string.`,
    examples: [
      `
assoc-in(
  {},
  ["a", "b", "c"],
  "Albert"
)`,
      `
assoc-in(
  [1, 2, [1, 2, 3]],
  [2, 1],
  "Albert"
)`,
      `
assoc-in(
  [1, 2, { name: "albert" }],
  [2, "name", 0],
  "A"
)`,
    ],
  },
  '++': {
    title: '++',
    category: 'Collection',
    linkName: '-plus-plus',
    returns: {
      type: 'collection',
    },
    args: {
      ...getOperatorArgs('collection', 'collection'),
      colls: {
        type: 'collection',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['a'] },
      { argumentNames: ['a', 'colls'] },
    ],
    description: 'Concatenates collections into one collection.',
    examples: [
      '"Albert" ++ " " ++ "Mojir"',
      '"Albert" ++ "Mojir"',

      '"Hi " concat "Albert"',
      '[1, 2] concat [3, 4]',

      '++("Albert", "-", "Mojir")',
      '++("Albert")',

      'concat("A", "l", "b", "e", "r", "t")',
      'concat([1, 2], [3, 4])',
      'concat([], [3, 4])',
      'concat([1, 2], [])',
      'concat([1, 2], [3, 4], [5, 6])',
      'concat([])',
      'concat({ a: 1, b: 2 }, { b: 1, c: 2 })',
      'concat({}, { a: 1 })',
    ],
    aliases: ['concat'],
  },
  'not-empty': {
    title: 'not-empty',
    category: 'Collection',
    linkName: 'not-empty',
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: ['collection', 'null'],
      },
    },
    variants: [
      { argumentNames: ['coll'] },
    ],
    description: 'Returns `null` if $coll is empty or `null`, otherwise $coll.',
    examples: [
      'not-empty([])',
      'not-empty([1, 2, 3])',
      'not-empty({})',
      'not-empty({ a: 2 })',
      'not-empty("")',
      'not-empty("Albert")',
      'not-empty(null)',
    ],
  },
  'every?': {
    title: 'every?',
    category: 'Collection',
    linkName: 'every-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns `true` if all entries in $a pass the test implemented by $b, otherwise returns `false`.',
    examples: [
      '[1, 2, 3] every? number?',
      '[1, 2, 3] every? even?',
      `
every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?,
)`,
      `
every?(
  [50, 100, 150, 200],
  -> $ > 10,
)`,
      `
every?(
  [],
  number?
)`,
      `
every?(
  "",
  number?
)`,
      `
every?(
  {},
  number?
)`,
      `
every?(
  { a: 2, b: 4},
  -> even?(second($))
)`,
      `
every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
    ],
  },
  'not-every?': {
    title: 'not-every?',
    category: 'Collection',
    linkName: 'not-every-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns `true` if at least one element in $a does not pass the test implemented by $b, otherwise returns `false`.',
    examples: [
      `
not-every?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
      `
not-every?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
      `
not-every?(
  [],
  number?
)`,
      `
not-every?(
  "",
  number?
)`,
      `
not-every?(
  {},
  number?
)`,
      `
not-every?(
  { a: 2, b: 4 },
  -> even?(second($))
)`,
      `
not-every?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
    ],
  },
  'any?': {
    title: 'any?',
    category: 'Collection',
    linkName: 'any-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns `true` if any element in $a pass the test implemented by $b, otherwise returns `false`.',
    examples: [
      `
any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
      `
any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
      `
any?(
  [],
  number?
)`,
      `
any?(
  "",
  number?
)`,
      `
any?(
  {},
  number?
)`,
      `
any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      `
any?(
  { a: 1, b: 3 },
  -> even?(second($))
)`,
    ],
  },
  'not-any?': {
    title: 'not-any?',
    category: 'Collection',
    linkName: 'not-any-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('collection', 'function'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns `false` if any element in $a pass the test implemented by $b, otherwise returns `true`.',
    examples: [
      `
not-any?(
  ["Albert", "Mojir", 160, [1, 2]],
  string?
)`,
      `
not-any?(
  [50, 100, 150, 200],
  x -> x > 10
)`,
      `
not-any?(
  [],
  number?
)`,
      `
not-any?(
  "",
  number?
)`,
      `
not-any?(
  {},
  number?
)`,
      `
not-any?(
  { a: 2, b: 3 },
  -> even?(second($))
)`,
      `
not-any?(
  { a: 1, b: 3 },
  -> even?(second($))
)`,
    ],
  },
  'update': {
    title: 'update',
    category: 'Collection',
    linkName: 'update',
    returns: {
      type: 'collection',
    },
    args: {
      'coll': {
        type: 'collection',
      },
      'key': {
        type: ['string', 'number'],
      },
      'fun': {
        type: 'function',
      },
      'fun-args': {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['coll', 'value', 'fun'] },
      { argumentNames: ['coll', 'value', 'fun', 'fun-args'] },
    ],
    description: `
Updates a value in the $coll collection, where $key is a key. $fun is a function
that will take the old value and any supplied $fun-args and
return the new value.
If the key does not exist, \`null\` is passed as the old value.`,
    examples: [
      `
let x = { a: 1, b: 2 };
update(x, "a", inc)`,
      `
let x = { a: 1, b: 2 };
update(
  x,
  "c",
  val -> null?(val) ? 0 : inc(val)
)`,
    ],
  },
  'update-in': {
    title: 'update-in',
    category: 'Collection',
    linkName: 'update-in',
    returns: {
      type: 'collection',
    },
    args: {
      'coll': {
        type: 'collection',
      },
      'ks': {
        type: 'array',
      },
      'fun': {
        type: 'function',
      },
      'fun-args': {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['coll', 'ks', 'fun'] },
      { argumentNames: ['coll', 'ks', 'fun', 'fun-args'] },
    ],
    description: `Updates a value in the $coll collection, where $ks is an array of
keys and $fun is a function that will take the old value and
any supplied $fun-args and return the new value. If any levels do not exist,
objects will be created - and the corresponding keys must be of type string.`,
    examples: [
      `
update-in(
  { a: [1, 2, 3] },
  ["a", 1],
  -> null?($) ? 0 : inc($)
)`,
      `
update-in(
  { a: { foo: "bar"} },
  ["a", "foo"],
  -> null?($) ? "?" : "!"
)`,
      `
update-in(
  { a: { foo: "bar"} },
  ["a", "baz"],
  -> null?($) ? "?" : "!"
)`,
      `
update-in(
  { a: [1, 2, 3] },
  ["a", 1],
  *,
  10,
  10,
  10,
)`,
    ],
  },
}
