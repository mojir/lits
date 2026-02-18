import { LitsError } from '../../errors'
import type { Arr } from '../../interface'
import type { SourceCodeInfo } from '../../tokenizer/token'
import { asNonUndefined } from '../../typeGuards'
import { assertArray } from '../../typeGuards/array'
import { assertStringOrRegularExpression, isObj } from '../../typeGuards/lits'
import { assertNumber } from '../../typeGuards/number'
import { asStringOrNumber, assertString, assertStringOrNumber } from '../../typeGuards/string'
import { toNonNegativeInteger } from '../../utils'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../interface'

const blankRegexp = /^\s*$/
export const stringNormalExpression: BuiltinNormalExpressions = {
  'string-repeat': {
    evaluate: ([str, count], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return str.repeat(count)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'String',
      returns: { type: 'number' },
      args: {
        a: { type: 'string' },
        b: { type: 'integer' },
        s: { type: 'string' },
        n: { type: 'integer' },
      },
      variants: [{ argumentNames: ['s', 'n'] }],
      description: 'Repeates $s $n times.',
      examples: [
        '"*" string-repeat 10',
        'string-repeat("*", 10)',
        'string-repeat("***", 0)',
      ],
    },
  },

  'str': {
    evaluate: (params: Arr) => {
      return params.reduce((result: string, param) => {
        const paramStr
          = param === undefined || param === null
            ? ''
            : isObj(param)
              ? JSON.stringify(param)
              : Array.isArray(param)
                ? JSON.stringify(param)
                : `${param}`
        return result + paramStr
      }, '')
    },
    arity: {},
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { values: { type: 'any', rest: true } },
      variants: [{ argumentNames: ['values'] }],
      description: 'Concatenats $values into one string. If `value` equals `null` empty string is returned.',
      examples: [
        'str("A string", ", and another string", " ...and more")',
        'str("Just one string")',
        'str()',
        'str(0, false, true, null, #"^kalle", [1, 2, 3], {a: "a"})',
      ],
      hideOperatorForm: true,
    },
  },

  'number': {
    evaluate: ([str], sourceCodeInfo): number => {
      assertString(str, sourceCodeInfo)
      const number = Number(str)
      if (Number.isNaN(number))
        throw new LitsError(`Could not convert '${str}' to a number.`, sourceCodeInfo)

      return number
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'number' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Parses $s to a number.',
      examples: [
        'number("10")',
        'number("010")',
        'number("-1.01")',
      ],
    },
  },

  'from-char-code': {
    evaluate: ([num], sourceCodeInfo): string => {
      assertNumber(num, sourceCodeInfo, { finite: true })
      const int = toNonNegativeInteger(num)
      try {
        return String.fromCodePoint(int)
      }
      catch (error) {
        throw new LitsError(error as Error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { code: { type: 'number' } },
      variants: [{ argumentNames: ['code'] }],
      description: 'Return character for code point $code.',
      examples: [
        'from-char-code(65)',
        'from-char-code(0)',
      ],
    },
  },

  'to-char-code': {
    evaluate: ([str], sourceCodeInfo): number => {
      assertString(str, sourceCodeInfo, { nonEmpty: true })
      return asNonUndefined(str.codePointAt(0), sourceCodeInfo)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'number' },
      args: { c: { type: 'string' } },
      variants: [{ argumentNames: ['c'] }],
      description: 'Return code point for first character in $c.',
      examples: [
        'to-char-code("A")',
        'to-char-code("Albert")',
      ],
    },
  },

  'lower-case': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.toLowerCase()
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns $s converted to lower case.',
      examples: [
        'lower-case("Albert")',
        'lower-case("")',
      ],
    },
  },

  'upper-case': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.toUpperCase()
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns $s converted to upper case.',
      examples: [
        'upper-case("Albert")',
        'upper-case("")',
      ],
    },
  },

  'trim': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.trim()
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a new string with leading and trailing whitespaces removed.',
      examples: [
        'trim("  Albert  ")',
        'trim("   ")',
        'trim("")',
      ],
    },
  },

  'trim-left': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.replace(/^\s+/, '')
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a new string with leading whitespaces removed.',
      examples: [
        'trim-left("  Albert  ")',
        'trim-left("   ")',
        'trim-left("")',
      ],
    },
  },

  'trim-right': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.replace(/\s+$/, '')
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a new string with trailing whitespaces removed.',
      examples: [
        'trim-right("  Albert  ")',
        'trim-right("   ")',
        'trim-right("")',
      ],
    },
  },

  'join': {
    evaluate: ([stringList, delimiter], sourceCodeInfo): string => {
      assertArray(stringList, sourceCodeInfo)
      stringList.forEach(str => assertStringOrNumber(str, sourceCodeInfo))
      assertString(delimiter, sourceCodeInfo)
      return stringList.join(delimiter)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: {
        a: { type: 'array' },
        b: { type: 'string' },
        arr: { type: 'array' },
        delimiter: { type: 'string' },
      },
      variants: [{ argumentNames: ['arr', 'delimiter'] }],
      description: 'Returns a new string by concatenating all of the elements in $arr, separated by $delimiter.',
      examples: [
        'map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str) join ", "',
        '([0, 1, 2, 3, 4, 5, 6, 7, 8, 9] map str) join ", "',
        'join(["Albert", 10], ", ")',
        'join(["Albert", "Mojir"], " ")',
        'join(map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str), ", ")',
      ],
    },
  },

  'split': {
    evaluate: ([str, stringOrRegExpValue, limit], sourceCodeInfo): string[] => {
      assertString(str, sourceCodeInfo)
      assertStringOrRegularExpression(stringOrRegExpValue, sourceCodeInfo)
      if (limit !== undefined)
        assertNumber(limit, sourceCodeInfo, { integer: true, nonNegative: true })

      const delimiter
        = typeof stringOrRegExpValue === 'string'
          ? stringOrRegExpValue
          : new RegExp(stringOrRegExpValue.s, stringOrRegExpValue.f)
      return str.split(delimiter, limit)
    },
    arity: { min: 2, max: 3 },
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: {
        a: { type: 'string' },
        b: { type: 'string' },
        s: { type: 'string' },
        delimiter: { type: 'string' },
        limit: { type: 'integer' },
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
  },
  'split-lines': {
    evaluate: ([str], sourceCodeInfo): string[] => {
      assertString(str, sourceCodeInfo)
      return str.split((/\r\n|\n|\r/)).filter(line => line !== '')
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Divides $s into an array of substrings, each representing a line.',
      examples: [
        'split-lines("Albert\nMojir\n")',
        'split-lines("Albert\n\nMojir")',
        'split-lines("Albert\nMojir\n\n")',
        'split-lines("")',
      ],
    },
  },

  'pad-left': {
    evaluate: ([str, length, padString], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(length, sourceCodeInfo, { integer: true })

      if (padString !== undefined)
        assertString(padString, sourceCodeInfo)

      return str.padStart(length, padString)
    },
    arity: { min: 2, max: 3 },
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: {
        a: { type: 'string' },
        b: { type: 'integer' },
        s: { type: 'string' },
        length: { type: 'integer' },
        padString: { type: 'string' },
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
  },

  'pad-right': {
    evaluate: ([str, length, padString], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(length, sourceCodeInfo, { integer: true })

      if (padString !== undefined)
        assertString(padString, sourceCodeInfo)

      return str.padEnd(length, padString)
    },
    arity: { min: 2, max: 3 },
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: {
        a: { type: 'string' },
        b: { type: 'integer' },
        s: { type: 'string' },
        length: { type: 'integer' },
        padString: { type: 'string' },
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
  },

  'template': {
    evaluate: ([templateString, ...placeholders], sourceCodeInfo): string => {
      assertString(templateString, sourceCodeInfo)
      assertArray(placeholders, sourceCodeInfo)
      const templateStrings = templateString.split('||||')
      if (templateStrings.length <= 1) {
        return applyPlaceholders(templateStrings[0] as string, placeholders, sourceCodeInfo)
      }
      else {
        // Pluralisation
        const count = placeholders[0]
        assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })
        const stringPlaceholders = [`${count}`, ...placeholders.slice(1)] as string[]
        if (templateStrings.length === 2) {
          // Exactly two valiants.
          // First variant (singular) for count = 1, Second variant (plural) for count = 0 or count > 1

          const placehoder = templateStrings[count === 1 ? 0 : 1] as string
          return applyPlaceholders(placehoder, stringPlaceholders, sourceCodeInfo)
        }
        else {
          // More than two variant:
          // Use count as index
          // If count >= number of variants, use last variant

          const placehoder = templateStrings[Math.min(count, templateStrings.length - 1)] as string
          return applyPlaceholders(placehoder, stringPlaceholders, sourceCodeInfo)
        }
      }
    },
    arity: { min: 1, max: 10 },
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: {
        s: { type: 'string' },
        params: { type: 'any', rest: true },
      },
      variants: [{ argumentNames: ['s', 'params'] }],
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
      hideOperatorForm: true,
    },
  },

  'encode-base64': {
    evaluate: ([value], sourceCodeInfo): string => {
      assertString(value, sourceCodeInfo)
      return btoa(
        encodeURIComponent(value).replace(/%([0-9A-F]{2})/g, (_match, p1) => {
          // eslint-disable-next-line ts/no-unsafe-argument
          return String.fromCharCode(Number.parseInt(p1, 16))
        }),
      )
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a Base64 encoded string from $s.',
      examples: [
        'encode-base64("Albert")',
      ],
    },
  },

  'decode-base64': {
    evaluate: ([value], sourceCodeInfo): string => {
      assertString(value, sourceCodeInfo)
      try {
        return decodeURIComponent(
          Array.prototype.map
            .call(atob(value), (c) => {
              // eslint-disable-next-line ts/no-unsafe-call, ts/no-unsafe-member-access
              return `%${(`00${c.charCodeAt(0).toString(16)}`).slice(-2)}`
            })
            .join(''),
        )
      }
      catch (error) {
        throw new LitsError(error as Error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { base64string: { type: 'string' } },
      variants: [{ argumentNames: ['base64string'] }],
      description: 'Returns a Base64 decoded string from $base64string.',
      examples: [
        'decode-base64("QWxiZXJ0IPCfkLs=")',
      ],
    },
  },

  'encode-uri-component': {
    evaluate: ([value], sourceCodeInfo): string => {
      assertString(value, sourceCodeInfo)
      return encodeURIComponent(value)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns an escaped `URI` string.',
      examples: [
        'encode-uri-component("Hi everyone!?")',
      ],
    },
  },

  'decode-uri-component': {
    evaluate: ([value], sourceCodeInfo): string => {
      assertString(value, sourceCodeInfo)
      try {
        return decodeURIComponent(value)
      }
      catch (error) {
        throw new LitsError(error as Error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns an un-escaped `URI` string.',
      examples: [
        'decode-uri-component("Hi%20everyone!%3F%20%F0%9F%91%8D")',
      ],
    },
  },
  'capitalize': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase()
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns $s with the first character converted to uppercase and the rest to lowercase.',
      examples: [
        'capitalize("albert")',
        'capitalize("ALBERT")',
        'capitalize("aLBERT")',
        'capitalize("")',
      ],
    },
  },
  'blank?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      if (value === null) {
        return true
      }
      assertString(value, sourceCodeInfo)
      return blankRegexp.test(value)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'String',
      returns: { type: 'boolean' },
      args: { s: { type: ['string', 'null'] } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns true if $s is null or only contains whitespace characters.',
      examples: [
        'blank?("")',
        'blank?(null)',
        'blank?("\n")',
        'blank?(" ")',
        'blank?(".")',
      ],
    },
  },
}

const doubleDollarRegexp = /\$\$/g
function applyPlaceholders(templateString: string, placeholders: unknown[], sourceCodeInfo?: SourceCodeInfo): string {
  for (let i = 0; i < 9; i += 1) {
    // Matches $1, $2, ..., $9
    // Does not match $$1
    // But does match $$$1, (since the two first '$' will later be raplaced with a single '$'
    const re = new RegExp(`(\\$\\$|[^$]|^)\\$${i + 1}`, 'g')
    if (re.test(templateString)) {
      const placeHolder = asStringOrNumber(placeholders[i], sourceCodeInfo)
      templateString = templateString.replace(re, `$1${placeHolder}`)
    }
  }
  templateString = templateString.replace(doubleDollarRegexp, '$')
  return templateString
}
