module.exports = {
  dissoc: {
    name: `dissoc`,
    category: `Object`,
    linkName: `dissoc`,
    returns: {
      type: `value`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
      {
        name: `attr`,
        type: `string`,
      },
    ],
    description: `Return shallow copy of \`object\` with attribute \`attr\` deleted.`,
    examples: [`(dissoc (object :x 10 :y 20) :x)`, `(dissoc { :x 10 } :y)`, `(def o { :a 5 }) (dissoc o :a) o`],
  },
  object: {
    name: `object`,
    category: `Object`,
    linkName: `object`,
    clojureDocs: null,
    returns: {
      type: `object`,
    },
    arguments: [
      {
        name: `[key value]`,
        type: `[string any]`,
        description: `zero or more`,
      },
    ],
    description: `Constructs a new object. Object members are created from \`key\` - \`value\` pairs. Requires an even number of arguments.`,
    examples: [`(object)`, `(object :x 10 :y true :z "A string")`, `{}`, `{:a 1 :b 2}`],
  },
  keys: {
    name: `keys`,
    category: `Object`,
    linkName: `keys`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    description: `Returns array of all keys in \`object\`.`,
    examples: [`(keys (object))`, `(keys (object :x 10 :y true :z "A string"))`],
  },
  vals: {
    name: `vals`,
    category: `Object`,
    linkName: `vals`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    description: `Returns array of all values in \`object\`.`,
    examples: [`(vals (object))`, `(vals (object :x 10 :y true :z "A string"))`],
  },
  entries: {
    name: `entries`,
    category: `Object`,
    linkName: `entries`,
    clojureDocs: null,
    returns: {
      type: `array of [key value] - paris`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    description: `Returns nested array of all key - value pairs in \`object\`.`,
    examples: [`(entries (object))`, `(entries (object :x 10 :y true :z "A string"))`],
  },
  find: {
    name: `find`,
    category: `Object`,
    linkName: `find`,
    returns: {
      type: `Entry`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
      {
        name: `key`,
        type: `string`,
      },
    ],
    description: `Returns entry for \`key\`, or \`nil\` if \`key\` not present in \`object\`.`,
    examples: [`(find (object :a 1 :b 2) :b)`, `(find (object :a 1 :b 2) :c)`],
  },
  merge: {
    name: `merge`,
    category: `Object`,
    linkName: `merge`,
    returns: {
      type: `object`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
        description: `one or many`,
      },
    ],
    description: `Returns a new object created by merging together all arguments.`,
    examples: [`(merge (object :x 10) (object :y 20))`, `(merge (object :x 10) (object :x 15 :y 20))`],
  },
  'merge-with': {
    name: `merge-with`,
    category: `Object`,
    linkName: `merge-with`,
    returns: {
      type: `object`,
    },
    arguments: [
      {
        name: `fn`,
        type: `Function`,
      },
      {
        name: `object`,
        type: `object`,
        description: `one or many`,
      },
    ],
    description: `Returns a new object created by merging together all arguments. If two keys appears in more than one object \`fn\` is used to calculate the new value.`,
    examples: [
      `(merge-with + (object :x 10) (object :y 20))`,
      `(merge-with + (object :x 10) (object :x 15 :y 20))`,
      `(merge-with - (object :x 10) (object :x 20) (object :x 30) (object :x 40))`,
    ],
  },
  zipmap: {
    name: `zipmap`,
    category: `Object`,
    linkName: `zipmap`,
    returns: {
      type: `object`,
    },
    arguments: [
      {
        name: `keys`,
        type: `Array<string>`,
      },
      {
        name: `values`,
        type: `Array`,
      },
    ],
    description: `Returns a new object created by mapping \`keys\` to \`values\`.`,
    examples: [`(zipmap [:a :b :c] [10 nil [1 2 3]])`, `(zipmap [:a :b :c] [1])`, `(zipmap [] [10 nil [1 2 3]])`],
  },
  'select-keys': {
    name: `select-keys`,
    category: `Object`,
    linkName: `select-keys`,
    returns: {
      type: `object`,
    },
    arguments: [
      {
        name: `object`,
        type: `Object`,
      },
      {
        name: `keys`,
        type: `Array<string>`,
      },
    ],
    description: `Returns an object containing only those entries in \`object\` whose key is in \`keys\`.`,
    examples: [`(select-keys {:a 1 :b 2 :c 3} [:a :b])`, `(select-keys {:a 1} [:a :b])`],
  },
}
