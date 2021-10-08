module.exports = {
  count: {
    name: `count`,
    category: `Collection`,
    linkName: `count`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection | string`,
      },
    ],
    shortDescription: `Returns number of elements in \`coll\`.`,
    longDescription: `Returns number of elements in \`coll\`.`,
    examples: [`(count [1 2 3])`, `(count [])`, `(count (object "a" 1))`, `(count "")`, `(count "Albert")`],
    specialExpression: false,
  },
  get: {
    name: `get`,
    category: `Collection`,
    linkName: `get`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or integer`,
      },
      {
        name: `default`,
        type: `any`,
        description: `optional`,
      },
    ],
    shortDescription: `Returns value in \`coll\` mapped at \`key\`.`,
    longDescription: `Returns value in \`coll\` mapped at \`key\`.`,
    examples: [
      `(get [1 2 3] 1)`,
      `(get [] 1)`,
      `(get [] 1 "default")`,
      `(get (object "a" 1) "a")`,
      `(get (object "a" 1) "b")`,
      `(get (object "a" 1) "b" "default")`,
    ],
    specialExpression: false,
  },
  'contains?': {
    name: `contains?`,
    category: `Collection`,
    linkName: `contains_question`,
    returns: {
      type: `true | false`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or number`,
      },
    ],
    shortDescription: `Returns \`true\` if \`collection\` contains \`key\`, otherwise returns \`false\`.`,
    longDescription: `Returns \`true\` if \`collection\` contains \`key\`, otherwise returns \`false\`.`,
    examples: [
      `(contains? [] 1)`,
      `(contains? [1] 1)`,
      `(contains? [1 2 3] 1)`,
      `(contains? {} "a")`,
      `(contains? {"a" 1 "b" 2} "a")`,
    ],
    specialExpression: false,
  },
  assoc: {
    name: `assoc`,
    category: `Collection`,
    linkName: `assoc`,
    returns: {
      type: `collection`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or number`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Sets \`value\` on specified element of \`coll\`.`,
    longDescription: `Sets \`value\` on specified element of \`coll\`.`,
    examples: [
      `(assoc [1 2 3] 1 "Two")`,
      `(assoc [1 2 3] 3 "Four")`,
      `(assoc {"a" 1 "b" 2} "a" "One")`,
      `(assoc {"a" 1 "b" 2} "c" "Three")`,
    ],
    specialExpression: false,
  },
  concat: {
    name: `concat`,
    category: `Collection`,
    linkName: `concat`,
    returns: {
      type: `collection`,
    },
    arguments: [
      {
        name: `value`,
        type: `collection`,
        description: `zero or more`,
      },
    ],
    shortDescription: `Concatenates array or object arguments into one collection.`,
    longDescription: `Concatenates array or object arguments into one collection.`,
    examples: [
      `(concat [1 2] [3 4])`,
      `(concat [] [3 4])`,
      `(concat [1 2] [])`,
      `(concat [1 2] [3 4] [5 6])`,
      `(concat [])`,
      `(concat {"a" 1 "b" 2} ["b" 1 "c" 2})`,
      `(concat {} ["a" 1})`,
    ],
    specialExpression: false,
  },
}
