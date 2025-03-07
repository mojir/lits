import type { FunctionReference } from '..'
import type { StringApiName } from '../api'

export const stringReference: Record<StringApiName, FunctionReference<'String'>> = {
  'subs': {
    title: 'subs',
    category: 'String',
    linkName: 'subs',
    returns: {
      type: 'string',
    },
    args: {
      s: {
        type: 'string',
      },
      start: {
        type: 'integer',
      },
      stop: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['s', 'start', 'stop'] },
      { argumentNames: ['s', 'start'] },
    ],
    description: 'Extracts characters from $start up to but not including $stop.',
    examples: [
      '(subs "A string" 2)',
      '(subs "A string" 2 5)',
      '(subs "A string" 2 100)',
      '(subs "A string" 100)',
    ],
  },
  'string_repeat': {
    title: 'string_repeat',
    category: 'String',
    linkName: 'string_repeat',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
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
      '(string_repeat "*" 10)',
      '(string_repeat "***" 0)',
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
    description: 'Concatenats $values into one string. If `value` equals `nil` empty string is returned.',
    examples: [
      '(str "A string" ", and another string" " ...and more")',
      '(str "Just one string")',
      '(str)',
      '(str 0 false true nil #"^kalle" [1 2 3] {:a :a})',
    ],
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
      '(number "10")',
      '(number "010")',
      '(number "-1.01")',
    ],
  },
  'lower_case': {
    title: 'lower_case',
    category: 'String',
    linkName: 'lower_case',
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
      '(lower_case "Albert")',
      '(lower_case "")',
    ],
  },
  'upper_case': {
    title: 'upper_case',
    category: 'String',
    linkName: 'upper_case',
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
      '(upper_case "Albert")',
      '(upper_case "")',
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
      '(trim "  Albert  ")',
      '(trim "   ")',
      '(trim "")',
    ],
  },
  'trim_left': {
    title: 'trim_left',
    category: 'String',
    linkName: 'trim_left',
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
      '(trim_left "  Albert  ")',
      '(trim_left "   ")',
      '(trim_left "")',
    ],
  },
  'trim_right': {
    title: 'trim_right',
    category: 'String',
    linkName: 'trim_right',
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
      '(trim_right "  Albert  ")',
      '(trim_right "   ")',
      '(trim_right "")',
    ],
  },
  'pad_left': {
    title: 'pad_left',
    category: 'String',
    linkName: 'pad_left',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
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
      '(pad_left "Albert" 20)',
      '(pad_left "Albert" 20 "-*-")',
      '(pad_left "Albert" 5)',
      '(pad_left "Albert" -1)',
    ],
  },
  'pad_right': {
    title: 'pad_right',
    category: 'String',
    linkName: 'pad_right',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
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
      '(pad_right "Albert" 20)',
      '(pad_right "Albert" 20 "-*-")',
      '(pad_right "Albert" 5)',
      '(pad_right "Albert" -1)',
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
      '(split "Albert Mojir" " ")',
      '(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))',
      '(split "0123456789" "")',
      '(map number (split "0123456789" "" 5))',
    ],
  },
  'split_lines': {
    title: 'split_lines',
    category: 'String',
    linkName: 'split_lines',
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
      '(split_lines "Albert\nMojir\n")',
      '(split_lines "Albert\n\nMojir")',
      '(split_lines "Albert\nMojir\n\n")',
      '(split_lines "")',
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
      '(template "Hi, $1 and $2" "Carl" "Larry")',
      '(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" :A :B :C :D :E :F :G :H :I)',
      '(template "$1 book||||$1 books" 0)',
      '(template "$1 book||||$1 books" 1)',
      '(template "$1 book||||$1 books" 2)',
      '(template "No book||||$1 book||||$1 books" 0)',
      '(template "No book||||$1 book||||$1 books" 1)',
      '(template "No book||||$1 book||||$1 books" 10)',
      '(template "No book||||One book||||Two books||||Three books||||$1 books" 0)',
      '(template "No book||||One book||||Two books||||Three books||||$1 books" 1)',
      '(template "No book||||One book||||Two books||||Three books||||$1 books" 2)',
      '(template "No book||||One book||||Two books||||Three books||||$1 books" 3)',
      '(template "No book||||One book||||Two books||||Three books||||$1 books" 4)',
    ],
  },
  'to_char_code': {
    title: 'to_char_code',
    category: 'String',
    linkName: 'to_char_code',
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
      '(to_char_code :A)',
      '(to_char_code "Albert")',
    ],
  },
  'from_char_code': {
    title: 'from_char_code',
    category: 'String',
    linkName: 'from_char_code',
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
      '(from_char_code 65)',
      '(from_char_code 0)',
    ],
  },
  'encode_base64': {
    title: 'encode_base64',
    category: 'String',
    linkName: 'encode_base64',
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
      '(encode_base64 "Albert")',
    ],
  },
  'decode_base64': {
    title: 'decode_base64',
    category: 'String',
    linkName: 'decode_base64',
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
      '(decode_base64 "QWxiZXJ0IPCfkLs=")',
    ],
  },
  'encode_uri_component': {
    title: 'encode_uri_component',
    category: 'String',
    linkName: 'encode_uri_component',
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
      '(encode_uri_component "Hi everyone!?")',
    ],
  },
  'decode_uri_component': {
    title: 'decode_uri_component',
    category: 'String',
    linkName: 'decode_uri_component',
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
      '(decode_uri_component "Hi%20everyone!%3F%20%F0%9F%91%8D")',
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
      '(join ["Albert" "Mojir"] " ")',
      '(join (map [0 1 2 3 4 5 6 7 8 9] str) ", ")',
    ],
  },
  '++': {
    title: '++',
    category: 'String',
    linkName: '-plus-plus',
    clojureDocs: null,
    returns: {
      type: 'string',
    },
    args: {
      strings: {
        type: ['string', 'number', 'null'],
        rest: true,
      },
    },
    variants: [{
      argumentNames: ['strings'],
    }],
    description: 'Concatenats $strings into one string.',
    examples: [
      '(++ "Albert" " " "Mojir")',
      '(++ "Albert" "Mojir")',
      '(++ "Albert" null "Mojir")',
      '(++ "Albert")',
      '(++)',
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
      '(capitalize "albert")',
      '(capitalize "ALBERT")',
      '(capitalize "aLBERT")',
      '(capitalize "")',
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
      '(blank? "")',
      '(blank? null)',
      '(blank? "\n")',
      '(blank? " ")',
      '(blank? ".")',
    ],
  },
}
