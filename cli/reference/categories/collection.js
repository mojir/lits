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
        type: `collection`,
      },
    ],
    shortDescription: `Returns number of elements in \`coll\`.`,
    longDescription: `Returns number of elements in \`coll\`.`,
    examples: [`(count [1 2 3])`, `(count [])`, `(count (object "a" 1))`],
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
      `(contains? (object) "a")`,
      `(contains? (object "a" 1 "b" 2) "a")`,
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
      `(assoc {"a" 1 "b" 2] "a" "One")`,
      `(assoc {"a" 1 "b" 2] "c" "Three")`,
    ],
    specialExpression: false,
  },
}
