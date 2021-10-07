module.exports = {
  regexp: {
    name: `regexp`,
    category: `Regular expression`,
    linkName: `regexp`,
    returns: {
      type: `RegExp`,
    },
    arguments: [
      {
        name: `pattern`,
        type: `string`,
      },
      {
        name: `flags`,
        type: `string`,
      },
    ],
    shortDescription: `Creates a RegExp from \`pattern\` and \`flags\`.`,
    longDescription: `Creates a RegExp from \`pattern\` and \`flags\`.`,
    examples: [`(regexp "^\\s*(.*)$")`, `(regexp "albert" "i")`],
    specialExpression: false,
  },
  match: {
    name: `match`,
    category: `Regular expression`,
    linkName: `match`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `pattern`,
        type: `RegExp`,
      },
      {
        name: `string`,
        type: `string`,
      },
    ],
    shortDescription: `Matches \`string\` against \`pattern\`. If \`string\` matches, a *match*-array is returned, otherwise \`undefined\`.`,
    longDescription: `Matches \`string\` against \`pattern\`. If \`string\` matches, a *match*-array is returned, otherwise \`undefined\`.`,
    examples: [
      `(match (regexp "^\\s*(.*)$") "  A string")`,
      `(match (regexp "albert" "i") "My name is Albert")`,
      `(match (regexp "albert" "i") "My name is Ben")`,
    ],
    specialExpression: false,
  },
  test: {
    name: `test`,
    category: `Regular expression`,
    linkName: `test`,
    returns: {
      type: `true | false`,
    },
    arguments: [
      {
        name: `pattern`,
        type: `RegExp`,
      },
      {
        name: `string`,
        type: `string`,
      },
    ],
    shortDescription: `Matches \`string\` against \`pattern\`. If \`string\` matches, \`true\` is returned, otherwise \`false\`.`,
    longDescription: `Matches \`string\` against \`pattern\`. If \`string\` matches, \`true\` is returned, otherwise \`false\`.`,
    examples: [
      `(test (regexp "^\\s*(.*)$") "  A string")`,
      `(test (regexp "albert" "i") "My name is Albert")`,
      `(test (regexp "albert" "i") "My name is Ben")`,
    ],
    specialExpression: false,
  },
  replace: {
    name: `replace`,
    category: `Regular expression`,
    linkName: `replace`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
      {
        name: `pattern`,
        type: `RegExp`,
      },
      {
        name: `replacement`,
        type: `string`,
      },
    ],
    shortDescription: `Returns a new string with some or all matches of \`pattern\` replaced by \`replacement\`.`,
    longDescription: `Returns a new string with some or all matches of \`pattern\` replaced by \`replacement\`.`,
    examples: [`(replace "Duck" (regexp "u") "i")`, `(replace "abcABC" (regexp "a" "gi") "-")`],
    specialExpression: false,
  },
}
