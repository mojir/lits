module.exports = {
  regexp: {
    name: `regexp`,
    category: `Regular expression`,
    linkName: `regexp`,
    clojureDocs: null,
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
    description: `Creates a RegExp from \`pattern\` and \`flags\`.`,
    examples: [`(regexp "^\\s*(.*)$")`, `#"^\\s*(.*)$"`, `(regexp "albert" :i)`, `#"albert"ig`],
  },
  match: {
    name: `match`,
    category: `Regular expression`,
    linkName: `match`,
    clojureDocs: null,
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
    description: `Matches \`string\` against \`pattern\`. If \`string\` matches, a *match*-array is returned, otherwise \`nil\`.`,
    examples: [
      `(match (regexp "^\\s*(.*)$") "  A string")`,
      `(match #"albert"i "My name is Albert")`,
      `(match #"albert"i "My name is Ben")`,
    ],
  },
  replace: {
    name: `replace`,
    category: `Regular expression`,
    linkName: `replace`,
    clojureDocs: null,
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
    description: `Returns a new string with some or all matches of \`pattern\` replaced by \`replacement\`.`,
    examples: [
      `(replace "Duck" (regexp :u) :i)`,
      `(replace "abcABC" (regexp :a "gi") "-")`,
      `(replace "abcABC" #"a"gi "-")`,
    ],
  },
}
