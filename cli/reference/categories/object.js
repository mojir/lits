module.exports = {
  ohas: {
    name: `ohas`,
    category: `Object`,
    linkName: `ohas`,
    returns: {
      type: `boolean`,
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
    shortDescription: `Returns \`true\` if \`object\` has an attribute named \`attr\`, otherwise returns \`false\`.`,
    longDescription: `Returns \`true\` if \`object\` has an attribute named \`attr\`, otherwise returns \`false\`.`,
    examples: [
      `(ohas (object "a" 10 "b" 20) "a")`,
      `(ohas (object "a" 10 "b" undefined) "b")`,
      `(ohas (object "a" 10 "b" undefined) "c")`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  oget: {
    name: `oget`,
    category: `Object`,
    linkName: `oget`,
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
    shortDescription: `Returns the value of \`object\`'s attribute \`attr\`. Returns \`undefined\` if \`object\` has no attribute named \`attr\`.`,
    longDescription: `Returns the value of \`object\`'s attribute \`attr\`. Returns \`undefined\` if \`object\` has no attribute named \`attr\`.`,
    examples: [
      `(oget (object "a" 10 "b" 20) "a")`,
      `(oget (object "a" 10 "b" undefined) "b")`,
      `(oget (object "a" 10 "b" undefined) "c")`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  odel: {
    name: `odel`,
    category: `Object`,
    linkName: `odel`,
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
      `(odel (object "x" 10) "x")`,
      `(odel (object "x" 10) "y")`,
      `(setq o (object "a" 5)) (odel o "a") o`,
      `(setq o (object "a" 5)) (odel o "b") o`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  oset: {
    name: `oset`,
    category: `Object`,
    linkName: `oset`,
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
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Sets an attribute on \`object\`. Returns \`value\`.`,
    longDescription: `Sets an attribute on \`object\`. Returns \`value\`.`,
    examples: [
      `(oset (object "x" 10) "a" 10)`,
      `(setq o (object)) (oset o "a" 10) o`,
      `(setq o (object "a" 5)) (oset o "a" 10) o`,
    ],
    specialExpression: false,
    sideEffects: [],
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
    sideEffects: [],
  },
  keys: {
    name: `keys`,
    category: `Object`,
    linkName: `keys`,
    returns: {
      type: `list`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    shortDescription: `Returns list of all keys in \`object\`.`,
    longDescription: `Returns list of all keys in \`object\`.`,
    examples: [`(keys (object))`, `(keys (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  values: {
    name: `values`,
    category: `Object`,
    linkName: `values`,
    returns: {
      type: `list`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    shortDescription: `Returns list of all values in \`object\`.`,
    longDescription: `Returns list of all values in \`object\`.`,
    examples: [`(values (object))`, `(values (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
  },
  entries: {
    name: `entries`,
    category: `Object`,
    linkName: `entries`,
    returns: {
      type: `list of [key value] - paris`,
    },
    arguments: [
      {
        name: `object`,
        type: `object`,
      },
    ],
    shortDescription: `Returns nested list of all key - value pairs in \`object\`.`,
    longDescription: `Returns nested list of all key - value pairs in \`object\`.`,
    examples: [`(entries (object))`, `(entries (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: false,
    sideEffects: [],
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
    sideEffects: [],
  },
}
