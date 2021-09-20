module.exports = {
  regexp: {
    name: 'regexp',
    category: 'Regular expression',
    linkName: 'regexp',
    returns: {
      type: 'RegExp',
    },
    arguments: [
      {
        name: 'pattern',
        type: 'string',
      },
      {
        name: 'flags',
        type: 'string',
      },
    ],
    shortDescription: 'Creates a RegExp from `pattern` and `flags`.',
    longDescription: 'Creates a RegExp from `pattern` and `flags`.',
    examples: [`(regexp "^\\s*(.*)$")`, `(regexp "albert" "i")`],
    specialExpression: false,
    sideEffects: [],
  },
  match: {
    name: 'match',
    category: 'Regular expression',
    linkName: 'match',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'pattern',
        type: 'RegExp',
      },
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription:
      'Matches `string` against `pattern`. If `string` matches, a *match*-list is returned, otherwise `undefined`.',
    longDescription:
      'Matches `string` against `pattern`. If `string` matches, a *match*-list is returned, otherwise `undefined`.',
    examples: [
      `(match (regexp "^\\s*(.*)$") "  A string")`,
      `(match (regexp "albert" "i") "My name is Albert")`,
      `(match (regexp "albert" "i") "My name is Ben")`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  replace: {
    name: 'replace',
    category: 'Regular expression',
    linkName: 'replace',
    returns: {
      type: 'list',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'pattern',
        type: 'RegExp',
      },
      {
        name: 'replacement',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string with some or all matches of `pattern` replaced by `replacement`.',
    longDescription: 'Returns a new string with some or all matches of `pattern` replaced by `replacement`.',
    examples: [`(replace "Duck" (regexp "u") "i")`, `(replace "abcABC" (regexp "a" "gi") "-")`],
    specialExpression: false,
    sideEffects: [],
  },
}
