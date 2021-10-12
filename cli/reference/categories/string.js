module.exports = {
  subs: {
    name: `subs`,
    category: `String`,
    linkName: `subs`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
      {
        name: `indexStart`,
        type: `integer`,
      },
      {
        name: `indexEnd`,
        type: `integer`,
        description: `optional`,
      },
    ],
    description: `Extracts characters from \`indexStart\` up to but not including \`indexEnd\`.`,
    examples: [`(subs "A string" 2)`, `(subs "A string" 2 5)`, `(subs "A string" 2 100)`, `(subs "A string" 100)`],
    specialExpression: false,
  },
  'string-repeat': {
    name: `string-repeat`,
    category: `String`,
    linkName: `string-repeat`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
      {
        name: `count`,
        type: `integer`,
      },
    ],
    description: `Repeates \`string\` \`count\` times.`,
    examples: [`(string-repeat "*" 10)`, `(string-repeat "***" 0)`],
    specialExpression: false,
  },
  str: {
    name: `str`,
    category: `String`,
    linkName: `str`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
        description: `zero or more`,
      },
    ],
    description: `Concatenats \`values\` into one \`string\`. If \`value\` equals \`undefined\` empty string is returned.`,
    examples: [
      `(str "A string" ", and another string" " ...and more")`,
      `(str "Just one string")`,
      `(str)`,
      `(str 0 false true null (regexp "^kalle") [1 2 3] {"a" "a"} undefined)`,
    ],
    specialExpression: false,
  },
  'string-to-number': {
    name: `string-to-number`,
    category: `String`,
    linkName: `string-to-number`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
    ],
    description: `Parses \`string\` to a number.`,
    examples: [`(string-to-number "10")`, `(string-to-number "010")`, `(string-to-number "-1.01")`],
    specialExpression: false,
  },
  'number-to-string': {
    name: `number-to-string`,
    category: `String`,
    linkName: `number-to-string`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
      {
        name: `base`,
        type: `number: 2, 8, 10 or 16`,
      },
    ],
    description: `Converts \`number\` to a string. If \`base\` is not equal to \`10\` nu mber must be a non negative integer.`,
    examples: [
      `(number-to-string 10)`,
      `(number-to-string -1.01)`,
      `(number-to-string -.01)`,
      `(number-to-string 15 2)`,
      `(number-to-string 15 8)`,
      `(number-to-string 15 16)`,
    ],
    specialExpression: false,
  },
  'lower-case': {
    name: `lower-case`,
    category: `String`,
    linkName: `lower-case`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
    ],
    description: `Returns \`string\` converted to lower case.`,
    examples: [`(lower-case "Albert")`, `(lower-case "")`],
    specialExpression: false,
  },
  'upper-case': {
    name: `upper-case`,
    category: `String`,
    linkName: `upper-case`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
    ],
    description: `Returns \`string\` converted to upper case.`,
    examples: [`(upper-case "Albert")`, `(upper-case "")`],
    specialExpression: false,
  },
  trim: {
    name: `trim`,
    category: `String`,
    linkName: `trim`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
    ],
    description: `Returns a new string with leading and trailing whitespaces removed.`,
    examples: [`(trim "  Albert  ")`, `(trim "   ")`, `(trim "")`],
    specialExpression: false,
  },
  'trim-left': {
    name: `trim-left`,
    category: `String`,
    linkName: `trim-left`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
    ],
    description: `Returns a new string with leading whitespaces removed.`,
    examples: [`(trim-left "  Albert  ")`, `(trim-left "   ")`, `(trim-left "")`],
    specialExpression: false,
  },
  'trim-right': {
    name: `trim-right`,
    category: `String`,
    linkName: `trim-right`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
    ],
    description: `Returns a new string with trailing whitespaces removed.`,
    examples: [`(trim-right "  Albert  ")`, `(trim-right "   ")`, `(trim-right "")`],
    specialExpression: false,
  },
  'pad-left': {
    name: `pad-left`,
    category: `String`,
    linkName: `pad-left`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
      {
        name: `length`,
        type: `integer`,
      },
      {
        name: `padString`,
        type: `string`,
        description: `optional`,
      },
    ],
    description: `Pads from the start of \`string\` with \`padString\` (multiple times, if needed) until the resulting string reaches the given \`length\`.`,
    examples: [
      `(pad-left "Albert" 20)`,
      `(pad-left "Albert" 20 "-*-")`,
      `(pad-left "Albert" 5)`,
      `(pad-left "Albert" -1)`,
    ],
    specialExpression: false,
  },
  'pad-right': {
    name: `pad-right`,
    category: `String`,
    linkName: `pad-right`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
      {
        name: `length`,
        type: `integer`,
      },
      {
        name: `padString`,
        type: `string`,
        description: `optional`,
      },
    ],
    description: `Pads from the start of \`string\` with \`padString\` (multiple times, if needed) until the resulting string reaches the given \`length\`.`,
    examples: [
      `(pad-right "Albert" 20)`,
      `(pad-right "Albert" 20 "-*-")`,
      `(pad-right "Albert" 5)`,
      `(pad-right "Albert" -1)`,
    ],
    specialExpression: false,
  },
  split: {
    name: `split`,
    category: `String`,
    linkName: `split`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `string`,
        type: `string`,
      },
      {
        name: `delimiter`,
        type: `string`,
      },
      {
        name: `limit`,
        type: `non negative integer`,
        description: `optional`,
      },
    ],
    description: `Divides \`string\` into an array of substrings. The division is done by searching for \`delimiter\`. If \`limit\` as provided, at most \`limit\` number of substrings are returned.`,
    examples: [
      `(split "Albert Mojir" " ")`,
      `(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))`,
      `(split "0123456789" "")`,
      `(map string-to-number (split "0123456789" "" 5))`,
    ],
    specialExpression: false,
  },
  template: {
    name: `template`,
    category: `String`,
    linkName: `template`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `templateString`,
        type: `string`,
      },
      {
        name: `placeholder`,
        type: `string`,
        description: `zero to nine`,
      },
    ],
    description: `Applies placeholders to a string. Support for basic pluralization - see examples. If pluralization is used, first placeholder must be a number.`,
    examples: [
      `(template "Hi, $1 and $2" "Carl" "Larry")`,
      `(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" "A" "B" "C" "D" "E" "F" "G" "H" "I")`,
      `(template "$1 book||||$1 books" 0)`,
      `(template "$1 book||||$1 books" 1)`,
      `(template "$1 book||||$1 books" 2)`,
    ],
    specialExpression: false,
  },
}
