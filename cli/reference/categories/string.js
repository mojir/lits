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
  },
  'string-repeat': {
    name: `string-repeat`,
    category: `String`,
    linkName: `string-repeat`,
    clojureDocs: null,
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
    description: `Concatenats \`values\` into one \`string\`. If \`value\` equals \`nil\` empty string is returned.`,
    examples: [
      `(str "A string" ", and another string" " ...and more")`,
      `(str "Just one string")`,
      `(str)`,
      `(str 0 false true nil #"^kalle" [1 2 3] {:a :a})`,
    ],
  },
  number: {
    name: `number`,
    category: `String`,
    linkName: `number`,
    clojureDocs: null,
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
    examples: [`(number "10")`, `(number "010")`, `(number "-1.01")`],
  },
  'number-to-string': {
    name: `number-to-string`,
    category: `String`,
    linkName: `number-to-string`,
    clojureDocs: null,
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
  },
  'upper-case': {
    name: `upper-case`,
    category: `String`,
    linkName: `upper-case`,
    clojureDocs: null,
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
  },
  trim: {
    name: `trim`,
    category: `String`,
    linkName: `trim`,
    clojureDocs: null,
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
  },
  'trim-left': {
    name: `trim-left`,
    category: `String`,
    linkName: `trim-left`,
    clojureDocs: null,
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
  },
  'trim-right': {
    name: `trim-right`,
    category: `String`,
    linkName: `trim-right`,
    clojureDocs: null,
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
  },
  'pad-left': {
    name: `pad-left`,
    category: `String`,
    linkName: `pad-left`,
    clojureDocs: null,
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
  },
  'pad-right': {
    name: `pad-right`,
    category: `String`,
    linkName: `pad-right`,
    clojureDocs: null,
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
  },
  split: {
    name: `split`,
    category: `String`,
    linkName: `split`,
    clojureDocs: null,
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
      `(map number (split "0123456789" "" 5))`,
    ],
  },
  template: {
    name: `template`,
    category: `String`,
    linkName: `template`,
    clojureDocs: null,
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
      `(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" :A :B :C :D :E :F :G :H :I)`,
      `(template "$1 book||||$1 books" 0)`,
      `(template "$1 book||||$1 books" 1)`,
      `(template "$1 book||||$1 books" 2)`,
      `(template "No book||||$1 book||||$1 books" 0)`,
      `(template "No book||||$1 book||||$1 books" 1)`,
      `(template "No book||||$1 book||||$1 books" 10)`,
      `(template "No book||||One book||||Two books||||Three books||||$1 books" 0)`,
      `(template "No book||||One book||||Two books||||Three books||||$1 books" 1)`,
      `(template "No book||||One book||||Two books||||Three books||||$1 books" 2)`,
      `(template "No book||||One book||||Two books||||Three books||||$1 books" 3)`,
      `(template "No book||||One book||||Two books||||Three books||||$1 books" 4)`,
    ],
  },
  'to-char-code': {
    name: `to-char-code`,
    category: `String`,
    linkName: `to-char-code`,
    clojureDocs: null,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `input`,
        type: `string`,
      },
    ],
    description: `Return code point for first character in \`input\`.`,
    examples: [`(to-char-code :A)`, `(to-char-code "Albert")`],
  },
  'from-char-code': {
    name: `from-char-code`,
    category: `String`,
    linkName: `from-char-code`,
    clojureDocs: null,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `code`,
        type: `number`,
      },
    ],
    description: `Return character for code point \`code\`.`,
    examples: [`(from-char-code 65)`, `(from-char-code 0)`],
  },
  'encode-base64': {
    name: `encode-base64`,
    category: `String`,
    linkName: `encode-base64`,
    clojureDocs: null,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `value`,
        type: `string`,
      },
    ],
    description: `Returns a Base64 encoded string from \`value\`.`,
    examples: [`(encode-base64 "Albert 🐻")`],
  },
  'decode-base64': {
    name: `decode-base64`,
    category: `String`,
    linkName: `decode-base64`,
    clojureDocs: null,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `base64string`,
        type: `string`,
      },
    ],
    description: `Returns a Base64 decoded string from \`base64string\`.`,
    examples: [`(decode-base64 "QWxiZXJ0IPCfkLs=")`],
  },
  'encode-uri-component': {
    name: `encode-uri-component`,
    category: `String`,
    linkName: `encode-uri-component`,
    clojureDocs: null,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `uri`,
        type: `string`,
      },
    ],
    description: `Returns an escaped \`uri\` string.`,
    examples: [`(encode-uri-component "Hi everyone!? 👍")`],
  },
  'decode-uri-component': {
    name: `decode-uri-component`,
    category: `String`,
    linkName: `decode-uri-component`,
    clojureDocs: null,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `uri`,
        type: `string`,
      },
    ],
    description: `Returns an un-escaped \`uri\` string.`,
    examples: [`(decode-uri-component "Hi%20everyone!%3F%20%F0%9F%91%8D")`],
  },
}
