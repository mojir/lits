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
    description: `Deletes the attribute \`attr\` from \`object\`.`,
    examples: [
      `(dissoc (object "x" 10) "x")`,
      `(dissoc (object "x" 10) "y")`,
      `(def o (object "a" 5)) (dissoc o "a") o`,
      `(def o (object "a" 5)) (dissoc o "b") o`,
    ],
    specialExpression: false,
  },
  object: {
    name: `object`,
    category: `Object`,
    linkName: `object`,
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
    examples: [`(object)`, `(object "x" 10 "y" true "z" "A string")`],
    specialExpression: false,
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
    examples: [`(keys (object))`, `(keys (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
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
    examples: [`(vals (object))`, `(vals (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
  },
  entries: {
    name: `entries`,
    category: `Object`,
    linkName: `entries`,
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
    examples: [`(entries (object))`, `(entries (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
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
    description: `Returns entry for \`key\`, or undefined if \`key\` not present in \`object\`.`,
    examples: [`(find (object "a" 1 "b" 2) "b")`, `(find (object "a" 1 "b" 2) "c")`],
    specialExpression: false,
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
    examples: [`(merge (object "x" 10) (object "y" 20))`, `(merge (object "x" 10) (object "x" 15 "y" 20))`],
    specialExpression: false,
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
    examples: [
      `(zipmap ["a" "b" "c"] [10 null [1 2 3]])`,
      `(zipmap ["a" "b" "c"] [1])`,
      `(zipmap [] [10 null [1 2 3]])`,
    ],
    specialExpression: false,
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
    examples: [`(select-keys {"a" 1 "b" 2 "c" 3} ["a" "b"])`, `(select-keys {"a" 1} ["a" "b"])`],
    specialExpression: false,
  },
}
