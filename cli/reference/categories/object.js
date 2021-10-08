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
    shortDescription: `Deletes the attribute \`attr\` from \`object\`.`,
    longDescription: `Deletes the attribute \`attr\` from \`object\`.`,
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
    shortDescription: `Constructs a new object. Object members are created from \`key\` - \`value\` pairs.`,
    longDescription: `Constructs a new object. Object members are created from \`key\` - \`value\` pairs. Requires an even number of arguments.`,
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
    shortDescription: `Returns array of all keys in \`object\`.`,
    longDescription: `Returns array of all keys in \`object\`.`,
    examples: [`(keys (object))`, `(keys (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
  },
  values: {
    name: `values`,
    category: `Object`,
    linkName: `values`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    shortDescription: `Returns array of all values in \`object\`.`,
    longDescription: `Returns array of all values in \`object\`.`,
    examples: [`(values (object))`, `(values (object "x" 10 "y" true "z" "A string"))`],
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
    shortDescription: `Returns nested array of all key - value pairs in \`object\`.`,
    longDescription: `Returns nested array of all key - value pairs in \`object\`.`,
    examples: [`(entries (object))`, `(entries (object "x" 10 "y" true "z" "A string"))`],
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
    shortDescription: `Returns a new object created by merging together all arguments.`,
    longDescription: `Returns a new object created by merging together all arguments.`,
    examples: [`(merge (object "x" 10) (object "y" 20))`, `(merge (object "x" 10) (object "x" 15 "y" 20))`],
    specialExpression: false,
  },
}
