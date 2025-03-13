import { type CollectionApiName, getOperatorArgs } from '../api'
import type { FunctionReference } from '..'

export const collectionReference: Record<CollectionApiName, FunctionReference<'Collection'>> = {
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
      'count({ a := 1 })',
      'count("")',
      'count("Albert")',
      'count(null)',
    ],
    algebraic: true,
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
      '{ a := 1 } get "a"',
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
  { a := 1 },
  "a"
)`,
      `
get(
  { a := 1 },
  "b"
)`,
      `
get(
  { a := 1 },
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
    algebraic: true,
  },
  'get-in': {
    title: 'get-in',
    category: 'Collection',
    linkName: 'get-in',
    returns: {
      type: 'any',
    },
    args: {
      'coll': {
        type: 'collection',
      },
      'keys': {
        type: 'array',
      },
      'not-found': {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['coll', 'keys'] },
      { argumentNames: ['coll', 'keys', 'not-found'] },
    ],
    description: 'Returns the value in a nested collection, where $keys is an array of keys. Returns $not-found if the key is not present. If $not-found is not set, `null` is returned.',
    examples: [
      `
(get-in
  [[1, 2, 3] [4 {:a "Kalle"} 6]]
  [1 1 :a 0])`,
      `
(get-in
  [[1, 2, 3] [4 {:a "Kalle"} 6]]
  [1 1 :b 0])`,
      `
(get-in
  [[1, 2, 3] [4 {:a "Kalle"} 6]]
  [1 1 :b 0]
  "Lisa")`,
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
      '{ a := 1, b := 2 } contains? "a"',
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
  { a := 1, b := 2 },
  "a"
)`,
    ],
    algebraic: true,
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
  { a := 1, b := 2 },
  "a",
  "One")`,
      `
assoc(
  { a := 1, b := 2 },
  "c",
  "Three")`,
      `
assoc(
  "Albert",
  6,
  "a")`,
    ],
    algebraic: true,
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
      keys: {
        type: ['number', 'string'],
        array: true,
      },
      value: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['coll', 'keys', 'value'] },
    ],
    description: `
Associates a value in the nested collection $coll, where $keys is an array of keys and $value is the new value.

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
  [1, 2, { name := "albert" }],
  [2, "name", 0],
  "A"
)`,
    ],
    algebraic: true,
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
      'concat({ a := 1, b := 2 }, { b := 1, c := 2 })',
      'concat({}, { a := 1 })',
    ],
    algebraic: true,
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
      'not-empty({ a := 2 })',
      'not-empty("")',
      'not-empty("Albert")',
      'not-empty(null)',
    ],
    algebraic: true,
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
  { a := 2, b := 4},
  -> even?(second($))
)`,
      `
every?(
  { a := 2, b := 3 },
  -> even?(second($))
)`,
    ],
    algebraic: true,
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
  { a := 2, b := 4 },
  -> even?(second($))
)`,
      `
not-every?(
  { a := 2, b := 3 },
  -> even?(second($))
)`,
    ],
    algebraic: true,
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
  { a := 2, b := 3 },
  -> even?(second($))
)`,
      `
any?(
  { a := 1, b := 3 },
  -> even?(second($))
)`,
    ],
    algebraic: true,
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
  { a := 2, b := 3 },
  -> even?(second($))
)`,
      `
not-any?(
  { a := 1, b := 3 },
  -> even?(second($))
)`,
    ],
    algebraic: true,
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
      'fn': {
        type: 'function',
      },
      'fn-args': {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['coll', 'value', 'fn'] },
      { argumentNames: ['coll', 'value', 'fn', 'fn-args'] },
    ],
    description: `
Updates a value in the $coll collection, where $key is a key. $fn is a function
that will take the old value and any supplied $fn-args and
return the new value.
If the key does not exist, \`null\` is passed as the old value.`,
    examples: [
      `
let x := { a := 1, b := 2 };
update(x, "a", inc)`,
      `
let x := { a := 1, b := 2 };
update(
  x,
  "c",
  val -> if null?(val) then 0 else inc(val) end
)`,
    ],
    algebraic: true,
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
      'keys': {
        type: 'array',
      },
      'fn': {
        type: 'function',
      },
      'fn-args': {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['coll', 'keys', 'fn'] },
      { argumentNames: ['coll', 'keys', 'fn', 'fn-args'] },
    ],
    description: `Updates a value in the $coll collection, where $keys is an array of
keys and $fn is a function that will take the old value and
any supplied $fn-args and return the new value. If any levels do not exist,
objects will be created - and the corresponding keys must be of type string.`,
    examples: [
      `
update-in(
  { a := [1, 2, 3] },
  ["a", 1],
  -> if null?($) then 0 end
)`,
      `
update-in(
  { a := { foo := "bar"} },
  ["a", "foo"],
  -> if null?($) then "?" else "!" end
)`,
      `
update-in(
  { a := { foo := "bar"} },
  ["a", "baz"],
  -> if null?($) then "?" else "!" end
)`,
      `
update-in(
  { a := [1, 2, 3] },
  ["a", 1],
  *,
  10,
  10,
  10,
)`,
    ],
    algebraic: true,
  },
}
