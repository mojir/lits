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
}
