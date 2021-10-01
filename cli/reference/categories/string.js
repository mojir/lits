module.exports = {
  substring: {
    name: 'substring',
    category: 'String',
    linkName: 'substring',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'indexStart',
        type: 'integer',
      },
      {
        name: 'indexEnd',
        type: 'integer',
        description: 'optional',
      },
    ],
    shortDescription: 'Extracts characters from `indexStart` up to but not including `indexEnd`.',
    longDescription: 'Extracts characters from `indexStart` up to but not including `indexEnd`.',
    examples: [
      '(substring "A string" 2)',
      '(substring "A string" 2 5)',
      '(substring "A string" 2 100)',
      '(substring "A string" 100)',
      '(substring "A string" 5 2)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string-length': {
    name: 'string-length',
    category: 'String',
    linkName: 'string-length',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns length of `string`.',
    longDescription: 'Returns length of `string`.',
    examples: ['(string-length "A string")', '(string-length "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'string-repeat': {
    name: 'string-repeat',
    category: 'String',
    linkName: 'string-repeat',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'count',
        type: 'integer',
      },
    ],
    shortDescription: 'Repeates `string` `count` times.',
    longDescription: 'Repeates `string` `count` times.',
    examples: ['(string-repeat "*" 10)', '(string-repeat "***" 0)'],
    specialExpression: false,
    sideEffects: [],
  },
  concat: {
    name: 'concat',
    category: 'String',
    linkName: 'concat',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'strings',
        type: 'string[]',
        description: 'zero or more',
      },
    ],
    shortDescription: 'Concatenats `strings` into one `string`.',
    longDescription: 'Concatenats `strings` into one `string`.',
    examples: ['(concat "A string" ", and another string" " ...and more")', '(concat "Just one string")', '(concat)'],
    specialExpression: false,
    sideEffects: [],
  },
  'string>': {
    name: 'string>',
    category: 'String',
    linkName: 'string_gt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than `string2`, otherwise result is `false`.',
    examples: [
      '(string> "A string" "Another string")',
      '(string> "Albert Mojir" "Albert")',
      '(string> "Albert" "Albert")',
      '(string> "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string>=': {
    name: 'string>=',
    category: 'String',
    linkName: 'string_gte',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than or equal to `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is greater than or equal to `string2`, otherwise result is `false`.',
    examples: [
      '(string>= "A string" "Another string")',
      '(string>= "Albert Mojir" "Albert")',
      '(string>= "Albert" "Albert")',
      '(string>= "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string<': {
    name: 'string<',
    category: 'String',
    linkName: 'string_lt',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than `string2`, otherwise result is `false`.',
    examples: [
      '(string< "A string" "Another string")',
      '(string< "Albert Mojir" "Albert")',
      '(string< "Albert" "Albert")',
      '(string< "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string<=': {
    name: 'string<=',
    category: 'String',
    linkName: 'string_lte',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string1',
        type: 'string',
      },
      {
        name: 'string2',
        type: 'string',
      },
    ],
    shortDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than or equal to `string2`, otherwise result is `false`.',
    longDescription:
      'Compares the two string arguments lexicographically, and the result is `true` if `string1` is less than or equal to `string2`, otherwise result is `false`.',
    examples: [
      '(string<= "A string" "Another string")',
      '(string<= "Albert Mojir" "Albert")',
      '(string<= "Albert" "Albert")',
      '(string<= "Albert" "albert")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string-to-number': {
    name: 'string-to-number',
    category: 'String',
    linkName: 'string-to-number',
    returns: {
      type: 'number',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Parses `string` to a number.',
    longDescription: 'Parses `string` to a number.',
    examples: [
      '(string-to-number "10")',
      '(string-to-number "010")',
      '(string-to-number "-1.01")',
      '(string-to-number "a10")',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'number-to-string': {
    name: 'number-to-string',
    category: 'String',
    linkName: 'number-to-string',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'number',
        type: 'number',
      },
      {
        name: 'base',
        type: 'number: 2, 8, 10 or 16',
      },
    ],
    shortDescription:
      'Converts `number` to a string. If `base` is not equal to `10` number must be a non negative integer.',
    longDescription:
      'Converts `number` to a string. If `base` is not equal to `10` nu mber must be a non negative integer.',
    examples: [
      '(number-to-string 10)',
      '(number-to-string -1.01)',
      '(number-to-string -.01)',
      '(number-to-string 15 2)',
      '(number-to-string 15 8)',
      '(number-to-string 15 16)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'string-reverse': {
    name: 'string-reverse',
    category: 'String',
    linkName: 'string-reverse',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Reverses a string.',
    longDescription: 'Reverses a string.',
    examples: ['(string-reverse "Albert")', '(string-reverse "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'lower-case': {
    name: 'lower-case',
    category: 'String',
    linkName: 'lower-case',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns `string` converted to lower case.',
    longDescription: 'Returns `string` converted to lower case.',
    examples: ['(lower-case "Albert")', '(lower-case "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'upper-case': {
    name: 'upper-case',
    category: 'String',
    linkName: 'upper-case',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns `string` converted to upper case.',
    longDescription: 'Returns `string` converted to upper case.',
    examples: ['(upper-case "Albert")', '(upper-case "")'],
    specialExpression: false,
    sideEffects: [],
  },
  trim: {
    name: 'trim',
    category: 'String',
    linkName: 'trim',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string with leading and trailing whitespaces removed.',
    longDescription: 'Returns a new string with leading and trailing whitespaces removed.',
    examples: ['(trim "  Albert  ")', '(trim "   ")', '(trim "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'trim-left': {
    name: 'trim-left',
    category: 'String',
    linkName: 'trim-left',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string with leading whitespaces removed.',
    longDescription: 'Returns a new string with leading whitespaces removed.',
    examples: ['(trim-left "  Albert  ")', '(trim-left "   ")', '(trim-left "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'trim-right': {
    name: 'trim-right',
    category: 'String',
    linkName: 'trim-right',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
    ],
    shortDescription: 'Returns a new string with trailing whitespaces removed.',
    longDescription: 'Returns a new string with trailing whitespaces removed.',
    examples: ['(trim-right "  Albert  ")', '(trim-right "   ")', '(trim-right "")'],
    specialExpression: false,
    sideEffects: [],
  },
  'pad-left': {
    name: 'pad-left',
    category: 'String',
    linkName: 'pad-left',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'length',
        type: 'integer',
      },
      {
        name: 'padString',
        type: 'string',
        description: 'optional',
      },
    ],
    shortDescription:
      'Pads from the start of `string` with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
    longDescription:
      'Pads from the start of `string` with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
    examples: [
      '(pad-left "Albert" 20)',
      '(pad-left "Albert" 20 "-*-")',
      '(pad-left "Albert" 5)',
      '(pad-left "Albert" -1)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  'pad-right': {
    name: 'pad-right',
    category: 'String',
    linkName: 'pad-right',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'length',
        type: 'integer',
      },
      {
        name: 'padString',
        type: 'string',
        description: 'optional',
      },
    ],
    shortDescription:
      'Pads at the end of `string` with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
    longDescription:
      'Pads from the start of `string` with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
    examples: [
      '(pad-right "Albert" 20)',
      '(pad-right "Albert" 20 "-*-")',
      '(pad-right "Albert" 5)',
      '(pad-right "Albert" -1)',
    ],
    specialExpression: false,
    sideEffects: [],
  },
  split: {
    name: 'split',
    category: 'String',
    linkName: 'split',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'string',
        type: 'string',
      },
      {
        name: 'delimiter',
        type: 'string',
      },
      {
        name: 'limit',
        type: 'non negative integer',
        description: 'optional',
      },
    ],
    shortDescription: 'Divides `string` into a list of substrings. The division is done by searching for `delimiter`.',
    longDescription:
      'Divides `string` into a list of substrings. The division is done by searching for `delimiter`. If `limit` as provided, at most `limit` number of substrings are returned.',
    examples: [
      `(split "Albert Mojir" " ")`,
      `(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))`,
      `(split "0123456789" "")`,
      `(map #'string-to-number (split "0123456789" "" 5))`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
  template: {
    name: 'template',
    category: 'String',
    linkName: 'template',
    returns: {
      type: 'string',
    },
    arguments: [
      {
        name: 'templateString',
        type: 'string',
      },
      {
        name: 'placeholder',
        type: 'string',
        description: 'zero to nine',
      },
    ],
    shortDescription: 'Applies placeholders to a string. Support for basic pluralization - see examples',
    longDescription:
      'Applies placeholders to a string. Support for basic pluralization - see examples. If pluralization is used, first placeholder must be a number.',
    examples: [
      `(template "Hi, $1 and $2" "Carl" "Larry")`,
      `(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" "A" "B" "C" "D" "E" "F" "G" "H" "I")`,
      `(template "$1 book||||$1 books" 0)`,
      `(template "$1 book||||$1 books" 1)`,
      `(template "$1 book||||$1 books" 2)`,
    ],
    specialExpression: false,
    sideEffects: [],
  },
}
