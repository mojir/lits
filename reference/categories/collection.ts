import type { CollectionApiName } from '../api.ts'
import type { FunctionReference } from '../index.ts'

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
        type: ['collection', 'string', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['coll'] },
    ],
    description: 'Returns number of elements in $coll.',
    examples: [
      '(count [1 2 3])',
      '(count [])',
      '(count (object :a 1))',
      '(count "")',
      '(count "Albert")',
      '(count nil)',
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
      'coll': {
        type: 'collection',
      },
      'key': {
        type: ['string', 'integer'],
      },
      'not-found': {
        type: 'any',
        description: 'Default value to return if $key is not found.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'key'] },
      { argumentNames: ['coll', 'key', 'not-found'] },
    ],
    description: 'Returns value in $coll mapped at \`key\`.',
    examples: [
      `
(get
  [1 2 3]
  1)`,
      `
(get
  []
  1)`,
      `
(get
  []
  1
  "default")`,
      `
(get
  (object :a 1)
  :a)`,
      `
(get
  (object :a 1)
  :b)`,
      `
(get
  (object :a 1)
  :b
  "default")`,
      `
(get
  nil
  :a)`,
      `
(get
  nil
  :b
  "default")`,
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
    description: 'Returns the value in a nested collection, where $keys is an array of keys. Returns $not-found if the key is not present. If $not-found is not set, `nil` is returned.',
    examples: [
      `
(get-in
  [[1 2 3] [4 {:a "Kalle"} 6]]
  [1 1 :a 0])`,
      `
(get-in
  [[1 2 3] [4 {:a "Kalle"} 6]]
  [1 1 :b 0])`,
      `
(get-in
  [[1 2 3] [4 {:a "Kalle"} 6]]
  [1 1 :b 0]
  "Lisa")`,
    ],
  },
  'contains?': {
    title: 'contains?',
    category: 'Collection',
    linkName: 'contains_question',
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: ['collection', 'nil'],
      },
      key: {
        type: ['string', 'number'],
      },
    },
    variants: [
      { argumentNames: ['coll', 'key'] },
    ],
    description: 'Returns `true` if $coll contains $key, otherwise returns `false`. For strings, it checks if substring is included.',
    examples: [
      `
(contains?
  []
  1)`,
      `
(contains?
  [1]
  1)`,
      `
(contains?
  [1 2 3]
  1)`,
      `
(contains?
  {}
  :a)`,
      `
(contains?
  {:a 1 :b 2}
  :a)`,
    ],
  },
  'has?': {
    title: 'has?',
    category: 'Collection',
    linkName: 'has_question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: ['collection', 'nil'],
      },
      value: {
        type: ['any'],
        description: 'Only support primitive values: `number`, `string`, `boolean`, or `nil`.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'value'] },
    ],
    description: 'Returns `true` if $coll has $value, otherwise returns `false`.',
    examples: [
      `
(has?
  [1 2 3]
  1)`,
      `
(has?
  [1 2 3]
  0)`,
      `
(has?
  {:a 1 :b 2}
  1)`,
      `
(has?
  {:a 1 :b 2}
  0)`,
      `
(has?
  "Albert"
  :A)`,
      `
(has?
  "Albert"
  :a)`,
      `
(has?
  nil
  :a)`,
    ],
  },
  'has-some?': {
    title: 'has-some?',
    category: 'Collection',
    linkName: 'has-some_question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: 'collection',
      },
      values: {
        type: 'any',
        array: true,
        description: 'Only support primitive values: `number`, `string`, `boolean`, or `nil`.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'values'] },
    ],
    description: 'Returns `true` if $coll has any of the elements in $values, otherwise returns `false`.',
    examples: [
      `
(has-some?
  []
  [])`,
      `
(has-some?
  [1 2 3]
  [])`,
      `
(has-some?
  [1 2 3]
  [0])`,
      `
  (has-some?
  [1 2 3]
  [0 1])`,
      `
(has-some?
  (object :a 1 :b 2)
  [0])`,
      `
(has-some?
  (object :a 1 :b 2)
  [0 1])`,
      `
(has-some?
  "Albert"
  "xyz")`,
      `
(has-some?
  "Albert"
  "xyzl")`,
      `
(has-some?
  [:a :b :c :d]
  "xyz")`,
      `
(has-some?
  [:a :b :c :d]
  "xyzc")`,
      `
(has-some?
  nil
  [1])`,
      `
(has-some?
  nil
  "")`,
    ],
  },
  'has-every?': {
    title: 'has-every?',
    category: 'Collection',
    linkName: 'has-every_question',
    clojureDocs: null,
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: 'collection',
      },
      values: {
        type: 'any',
        array: true,
        description: 'Only support primitive values: `number`, `string`, `boolean`, or `nil`.',
      },
    },
    variants: [
      { argumentNames: ['coll', 'values'] },
    ],
    description: 'Returns `true` if $coll has all of the elements in $values, otherwise returns `false`.',
    examples: [
      `
(has-every?
  []
  [])`,
      `
(has-every?
  [1 2 3]
  [])`,
      `
(has-every?
  [1 2 3]
  [0 1])`,
      `
(has-every?
  [1 2 3]
  [1 2])`,
      `
(has-every?
  (object :a 1 :b 2)
  [0 1])`,
      `
(has-every?
  (object :a 1 :b 2)
  [1 2])`,
      `
(has-every?
  "Albert"
  "xyz")`,
      `
(has-every?
  "Albert"
  "treblA")`,
      `
(has-every?
  [:a :b :c :d]
  "xyz")`,
      `
(has-every?
  [:a :b :c :d]
  "dcba")`,
      `
(has-every?
  nil
  "abc")`,
      `
(has-every?
  nil
  [0, 1, nil])`,
      `
(has-every?
  nil
  nil)`,
      `
(has-every?
  [1, 2, 3]
  nil)`,
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
(assoc
  [1 2 3]
  1
  "Two")`,
      `
(assoc
  [1 2 3]
  3
  "Four")`,
      `
(assoc
  {:a 1 :b 2}
  :a
  "One")`,
      `
(assoc
  {:a 1 :b 2}
  :c
  "Three")`,
      `
(assoc
  :Albert
  6
  :a)`,
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
(assoc-in
  {}
  [:a :b :c]
  "Albert")`,
      `
(assoc-in
  [1 2 [1 2 3]]
  [2 1]
  "Albert")`,
      `
(assoc-in
  [1 2 {"name" "albert"}]
  [2 "name" 0]
  :A)`,
    ],
  },
  'concat': {
    title: 'concat',
    category: 'Collection',
    linkName: 'concat',
    returns: {
      type: 'collection',
    },
    args: {
      coll: {
        type: 'collection',
      },
      colls: {
        type: 'collection',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['coll'] },
      { argumentNames: ['coll', 'colls'] },
    ],
    description: 'Concatenates collections into one collection.',
    examples: [
      '(concat :A :l :b :e :r :t)',
      '(concat [1 2] [3 4])',
      '(concat [] [3 4])',
      '(concat [1 2] [])',
      '(concat [1 2] [3 4] [5 6])',
      '(concat [])',
      '(concat {:a 1 :b 2} {:b 1 :c 2})',
      '(concat {} {:a 1})',
    ],
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
        type: ['collection', 'nil'],
      },
    },
    variants: [
      { argumentNames: ['coll'] },
    ],
    description: 'Returns `nil` if $coll is empty or `nil`, otherwise $coll.',
    examples: [
      '(not-empty [])',
      '(not-empty [1 2 3])',
      '(not-empty {})',
      '(not-empty {:a 2})',
      '(not-empty "")',
      '(not-empty "Albert")',
      '(not-empty nil)',
    ],
  },
  'every?': {
    title: 'every?',
    category: 'Collection',
    linkName: 'every_question',
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: 'collection',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fn'] },
    ],
    description: 'Returns `true` if all entries in $coll pass the test implemented by $fn, otherwise returns `false`.',
    examples: [
      `
(every?
  string?
  ["Albert" "Mojir" 160 [1 2]])`,
      `
(every?
  (fn [x] (> x 10))
  [50 100 150 200])`,
      `
(every?
  number?
  [])`,
      `
(every?
  number?
  "")`,
      `
(every?
  number?
  {})`,
      `
(every?
  #(even? (second %))
  {:a 2 :b 4})`,
      `
(every?
  #(even? (second %))
  {:a 2 :b 3})`,
    ],
  },
  'not-every?': {
    title: 'not-every?',
    category: 'Collection',
    linkName: 'not-every_question',
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: 'collection',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fn'] },
    ],
    description: 'Returns `true` if at least one element in $coll does not pass the test implemented by $fn, otherwise returns `false`.',
    examples: [
      `
(not-every?
  string?
  ["Albert" "Mojir" 160 [1 2]])`,
      `
(not-every?
  (fn [x] (> x 10))
  [50 100 150 200])`,
      `
(not-every?
  number?
  [])`,
      `
(not-every?
  number?
  "")`,
      `
(not-every?
  number?
  {})`,
      `
(not-every?
  #(even? (second %))
  {:a 2 :b 4})`,
      `
(not-every?
  #(even? (second %))
  {:a 2 :b 3})`,
    ],
  },
  'any?': {
    title: 'any?',
    category: 'Collection',
    linkName: 'any_question',
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: 'collection',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fn'] },
    ],
    description: 'Returns `true` if any element in $coll pass the test implemented by $fn, otherwise returns `false`.',
    examples: [
      `
(any?
  string?
  ["Albert" "Mojir" 160 [1 2]])`,
      `
(any?
  (fn [x] (> x 10))
  [50 100 150 200])`,
      `
(any?
  number?
  [])`,
      `
(any?
  number?
  "")`,
      `
(any?
  number?
  {})`,
      `
(any?
  #(even? (second %))
  {:a 2 :b 3})`,
      `
(any?
  #(even? (second %))
  {:a 1 :b 3})`,
    ],
  },
  'not-any?': {
    title: 'not-any?',
    category: 'Collection',
    linkName: 'not-any_question',
    returns: {
      type: 'boolean',
    },
    args: {
      coll: {
        type: 'collection',
      },
      fn: {
        type: 'function',
      },
    },
    variants: [
      { argumentNames: ['coll', 'fn'] },
    ],
    description: 'Returns `false` if any element in $coll pass the test implemented by $fn, otherwise returns `true`.',
    examples: [
      `
(not-any?
  string?
  ["Albert" "Mojir" 160 [1 2]])`,
      `
(not-any?
  (fn [x] (> x 10))
  [50 100 150 200])`,
      `
(not-any?
  number?
  [])`,
      `
(not-any?
  number?
  "")`,
      `
(not-any?
  number?
  {})`,
      `
(not-any?
  #(even? (second %))
  {:a 2 :b 3})`,
      `
(not-any?
  #(even? (second %))
  {:a 1 :b 3})`,
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
If the key does not exist, \`nil\` is passed as the old value.`,
    examples: [
      `
(def x {:a 1 :b 2})
(update x :a inc)`,
      `
(def x {:a 1 :b 2})
(update
  x
  :c
  (fn [val]
    (if (nil? val) 0 (inc val))))`,
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
(update-in
  {:a [1 2 3]}
  [:a 1]
  (fn [val]
    (when (nil? val) 0)))`,
      `
(update-in
  {:a {:foo :bar}}
  [:a :foo]
  (fn [val]
    (if (nil? val) "?" "!")))`,
      `
(update-in
  {:a {:foo :bar}}
  [:a :baz]
  (fn [val]
    (if (nil? val) "?" "!")))`,
      `
(update-in
  {:a [1 2 3]}
  [:a 1]
  *
  10
  10
  10)`,
    ],
  },
}
