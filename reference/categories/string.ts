import type { FunctionReference } from '..'
import { type StringApiName, getOperatorArgs } from '../api'

export const stringReference: Record<StringApiName, FunctionReference<'String'>> = {
  'string-repeat': {
    title: 'string-repeat',
    category: 'String',
    linkName: 'string-repeat',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('string', 'integer'),
      s: {
        type: 'string',
      },
      n: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['s', 'n'] },
    ],
    description: 'Repeates $s $n times.',
    examples: [
      '"*" string-repeat 10',
      'string-repeat("*", 10)',
      'string-repeat("***", 0)',
    ],
  },
  'str': {
    title: 'str',
    category: 'String',
    linkName: 'str',
    returns: {
      type: 'string',
    },
    args: {
      values: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['values'] },
    ],
    description: 'Concatenats $values into one string. If `value` equals `null` empty string is returned.',
    examples: [
      'str("A string", ", and another string", " ...and more")',
      'str("Just one string")',
      'str()',
      'str(0, false, true, null, #"^kalle", [1, 2, 3], {a := "a"})',
    ],
    noOperatorDocumentation: true,
  },
  'number': {
    title: 'number',
    category: 'String',
    linkName: 'number',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Parses $s to a number.',
    examples: [
      'number("10")',
      'number("010")',
      'number("-1.01")',
    ],
  },
  'lower-case': {
    title: 'lower-case',
    category: 'String',
    linkName: 'lower-case',
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns $s converted to lower case.',
    examples: [
      'lower-case("Albert")',
      'lower-case("")',
    ],
  },
  'upper-case': {
    title: 'upper-case',
    category: 'String',
    linkName: 'upper-case',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns $s converted to upper case.',
    examples: [
      'upper-case("Albert")',
      'upper-case("")',
    ],
  },
  'trim': {
    title: 'trim',
    category: 'String',
    linkName: 'trim',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns a new string with leading and trailing whitespaces removed.',
    examples: [
      'trim("  Albert  ")',
      'trim("   ")',
      'trim("")',
    ],
  },
  'trim-left': {
    title: 'trim-left',
    category: 'String',
    linkName: 'trim-left',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns a new string with leading whitespaces removed.',
    examples: [
      'trim-left("  Albert  ")',
      'trim-left("   ")',
      'trim-left("")',
    ],
  },
  'trim-right': {
    title: 'trim-right',
    category: 'String',
    linkName: 'trim-right',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns a new string with trailing whitespaces removed.',
    examples: [
      'trim-right("  Albert  ")',
      'trim-right("   ")',
      'trim-right("")',
    ],
  },
  'pad-left': {
    title: 'pad-left',
    category: 'String',
    linkName: 'pad-left',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      ...getOperatorArgs('string', 'integer'),
      s: {
        type: 'string',
      },
      length: {
        type: 'integer',
      },
      padString: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s', 'length'] },
      { argumentNames: ['s', 'length', 'padString'] },
    ],
    description: 'Pads from the start of $s with `padString` (multiple times, if needed) until the resulting string reaches the given $length.',
    examples: [
      '"Albert" pad-left 20',
      'pad-left("Albert", 20)',
      'pad-left("Albert", 20, "-*-")',
      'pad-left("Albert", 5)',
      'pad-left("Albert", -1)',
    ],
  },
  'pad-right': {
    title: 'pad-right',
    category: 'String',
    linkName: 'pad-right',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      ...getOperatorArgs('string', 'integer'),
      s: {
        type: 'string',
      },
      length: {
        type: 'integer',
      },
      padString: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s', 'length'] },
      { argumentNames: ['s', 'length', 'padString'] },
    ],
    description: 'Pads from the start of $s with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
    examples: [
      '"Albert" pad-right 20',
      'pad-right("Albert", 20)',
      'pad-right("Albert", 20, "-*-")',
      'pad-right("Albert", 5)',
      'pad-right("Albert", -1)',
    ],
  },
  'split': {
    title: 'split',
    category: 'String',
    linkName: 'split',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      ...getOperatorArgs('string', 'string'),
      s: {
        type: 'string',
      },
      delimiter: {
        type: 'string',
      },
      limit: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['s', 'delimiter'] },
      { argumentNames: ['s', 'delimiter', 'limit'] },
    ],
    description: 'Divides $s into an array of substrings. The division is done by searching for `delimiter`. If `limit` as provided, at most `limit` number of substrings are returned.',
    examples: [
      '"Albert Mojir" split " "',
      'split("Albert Mojir", " ")',
      'split("abcdefghijklmnopqrstuvw", #"[aoueiy]")',
      'split("0123456789", "")',
      'split("0123456789", "", 5) map number',
    ],
  },
  'split-lines': {
    title: 'split-lines',
    category: 'String',
    linkName: 'split-lines',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Divides $s into an array of substrings, each representing a line.',
    examples: [
      'split-lines("Albert\nMojir\n")',
      'split-lines("Albert\n\nMojir")',
      'split-lines("Albert\nMojir\n\n")',
      'split-lines("")',
    ],
  },
  'template': {
    title: 'template',
    category: 'String',
    linkName: 'template',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
      params: {
        type: 'any',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['s', 'params'] },
    ],
    description: 'Applies placeholders to a string. Support for basic pluralization - see examples. If pluralization is used, first placeholder must be a number.',
    examples: [
      'template("Hi, $1 and $2", "Carl", "Larry")',
      'template("Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9", "A", "B", "C", "D", "E", "F", "G", "H", "I")',
      'template("$1 book||||$1 books", 0)',
      'template("$1 book||||$1 books", 1)',
      'template("$1 book||||$1 books", 2)',
      'template("No book||||$1 book||||$1 books", 0)',
      'template("No book||||$1 book||||$1 books", 1)',
      'template("No book||||$1 book||||$1 books", 10)',
      'template("No book||||One book||||Two books||||Three books||||$1 books", 0)',
      'template("No book||||One book||||Two books||||Three books||||$1 books", 1)',
      'template("No book||||One book||||Two books||||Three books||||$1 books", 2)',
      'template("No book||||One book||||Two books||||Three books||||$1 books", 3)',
      'template("No book||||One book||||Two books||||Three books||||$1 books", 4)',
    ],
    noOperatorDocumentation: true,
  },
  'to-char-code': {
    title: 'to-char-code',
    category: 'String',
    linkName: 'to-char-code',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      c: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['c'] },
    ],
    description: 'Return code point for first character in $c.',
    examples: [
      'to-char-code("A")',
      'to-char-code("Albert")',
    ],
  },
  'from-char-code': {
    title: 'from-char-code',
    category: 'String',
    linkName: 'from-char-code',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      code: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['code'] },
    ],
    description: 'Return character for code point $code.',
    examples: [
      'from-char-code(65)',
      'from-char-code(0)',
    ],
  },
  'encode-base64': {
    title: 'encode-base64',
    category: 'String',
    linkName: 'encode-base64',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns a Base64 encoded string from $s.',
    examples: [
      'encode-base64("Albert")',
    ],
  },
  'decode-base64': {
    title: 'decode-base64',
    category: 'String',
    linkName: 'decode-base64',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      base64string: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['base64string'] },
    ],
    description: 'Returns a Base64 decoded string from $base64string.',
    examples: [
      'decode-base64("QWxiZXJ0IPCfkLs=")',
    ],
  },
  'encode-uri-component': {
    title: 'encode-uri-component',
    category: 'String',
    linkName: 'encode-uri-component',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns an escaped `URI` string.',
    examples: [
      'encode-uri-component("Hi everyone!?")',
    ],
  },
  'decode-uri-component': {
    title: 'decode-uri-component',
    category: 'String',
    linkName: 'decode-uri-component',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns an un-escaped `URI` string.',
    examples: [
      'decode-uri-component("Hi%20everyone!%3F%20%F0%9F%91%8D")',
    ],
  },

  'join': {
    title: 'join',
    category: 'String',
    linkName: 'join',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      ...getOperatorArgs('array', 'string'),
      arr: {
        type: 'array',
      },
      delimiter: {
        type: 'string',
      },
    },
    variants: [{
      argumentNames: ['arr', 'delimiter'],
    }],
    description: 'Returns a new string by concatenating all of the elements in $arr, separated by $delimiter.',
    examples: [
      'map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str) join ", "',
      '([0, 1, 2, 3, 4, 5, 6, 7, 8, 9] map str) join ", "',
      'join(["Albert", 10], ", ")',
      'join(["Albert", "Mojir"], " ")',
      'join(map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str), ", ")',
    ],
  },
  'capitalize': {
    title: 'capitalize',
    category: 'String',
    linkName: 'capitalize',
    clojureDocs: 'clojure.string/capitalize',
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns $s with the first character converted to uppercase and the rest to lowercase.',
    examples: [
      'capitalize("albert")',
      'capitalize("ALBERT")',
      'capitalize("aLBERT")',
      'capitalize("")',
    ],
  },
  'blank?': {
    title: 'blank?',
    category: 'String',
    linkName: 'blank-question',
    clojureDocs: 'clojure.string/blank_q',
    returns: {
      type: 'boolean',
    },
    args: {
      s: {
        type: ['string', 'null'],
      },
    },
    variants: [
      { argumentNames: ['s'] },
    ],
    description: 'Returns true if $s is null or only contains whitespace characters.',
    examples: [
      'blank?("")',
      'blank?(null)',
      'blank?("\n")',
      'blank?(" ")',
      'blank?(".")',
    ],
  },
}
