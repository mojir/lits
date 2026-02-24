import { LitsError } from '../../../errors'
import { asNonUndefined } from '../../../typeGuards'
import { assertArray } from '../../../typeGuards/array'
import { assertNumber } from '../../../typeGuards/number'
import { asStringOrNumber, assertString } from '../../../typeGuards/string'
import { toNonNegativeInteger } from '../../../utils'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../interface'
import type { LitsModule } from '../interface'
import type { SourceCodeInfo } from '../../../tokenizer/token'

const stringUtilsFunctions: BuiltinNormalExpressions = {
  'string-repeat': {
    evaluate: ([str, count], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return str.repeat(count)
    },
    arity: toFixedArity(2),
    docs: {
      category: 'string',
      returns: { type: 'number' },
      args: {
        a: { type: 'string' },
        b: { type: 'integer' },
        s: { type: 'string' },
        n: { type: 'integer' },
      },
      variants: [{ argumentNames: ['s', 'n'] }],
      description: 'Repeates $s $n times.',
      seeAlso: ['str', 'repeat'],
      examples: [
        `let { string-repeat } = import(string);
"*" string-repeat 10`,
        `let { string-repeat } = import(string);
string-repeat("*", 10)`,
        `let { string-repeat } = import(string);
string-repeat("***", 0)`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { code: { type: 'number' } },
      variants: [{ argumentNames: ['code'] }],
      description: 'Return character for code point $code.',
      seeAlso: ['string.to-char-code'],
      examples: [
        `let { from-char-code } = import(string);
from-char-code(65)`,
        `let { from-char-code } = import(string);
from-char-code(0)`,
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
      category: 'string',
      returns: { type: 'number' },
      args: { c: { type: 'string' } },
      variants: [{ argumentNames: ['c'] }],
      description: 'Return code point for first character in $c.',
      seeAlso: ['string.from-char-code'],
      examples: [
        `let { to-char-code } = import(string);
to-char-code("A")`,
        `let { to-char-code } = import(string);
to-char-code("Albert")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a new string with leading whitespaces removed.',
      seeAlso: ['trim', 'string.trim-right'],
      examples: [
        `let { trim-left } = import(string);
trim-left("  Albert  ")`,
        `let { trim-left } = import(string);
trim-left("   ")`,
        `let { trim-left } = import(string);
trim-left("")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a new string with trailing whitespaces removed.',
      seeAlso: ['trim', 'string.trim-left'],
      examples: [
        `let { trim-right } = import(string);
trim-right("  Albert  ")`,
        `let { trim-right } = import(string);
trim-right("   ")`,
        `let { trim-right } = import(string);
trim-right("")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Divides $s into an array of substrings, each representing a line.',
      seeAlso: ['split'],
      examples: [
        `let { split-lines } = import(string);
split-lines("Albert\nMojir\n")`,
        `let { split-lines } = import(string);
split-lines("Albert\n\nMojir")`,
        `let { split-lines } = import(string);
split-lines("Albert\nMojir\n\n")`,
        `let { split-lines } = import(string);
split-lines("")`,
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
      category: 'string',
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
      seeAlso: ['string.pad-right'],
      examples: [
        `let { pad-left } = import(string);
"Albert" pad-left 20`,
        `let { pad-left } = import(string);
pad-left("Albert", 20)`,
        `let { pad-left } = import(string);
pad-left("Albert", 20, "-*-")`,
        `let { pad-left } = import(string);
pad-left("Albert", 5)`,
        `let { pad-left } = import(string);
pad-left("Albert", -1)`,
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
      category: 'string',
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
      seeAlso: ['string.pad-left'],
      examples: [
        `let { pad-right } = import(string);
"Albert" pad-right 20`,
        `let { pad-right } = import(string);
pad-right("Albert", 20)`,
        `let { pad-right } = import(string);
pad-right("Albert", 20, "-*-")`,
        `let { pad-right } = import(string);
pad-right("Albert", 5)`,
        `let { pad-right } = import(string);
pad-right("Albert", -1)`,
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
      category: 'string',
      returns: { type: 'string' },
      args: {
        s: { type: 'string' },
        params: { type: 'any', rest: true },
      },
      variants: [{ argumentNames: ['s', 'params'] }],
      description: 'Applies placeholders to a string. Support for basic pluralization - see examples. If pluralization is used, first placeholder must be a number.',
      seeAlso: ['str'],
      examples: [
        `let { template } = import(string);
template("Hi, $1 and $2", "Carl", "Larry")`,
        `let { template } = import(string);
template("Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9", "A", "B", "C", "D", "E", "F", "G", "H", "I")`,
        `let { template } = import(string);
template("$1 book||||$1 books", 0)`,
        `let { template } = import(string);
template("$1 book||||$1 books", 1)`,
        `let { template } = import(string);
template("$1 book||||$1 books", 2)`,
        `let { template } = import(string);
template("No book||||$1 book||||$1 books", 0)`,
        `let { template } = import(string);
template("No book||||$1 book||||$1 books", 1)`,
        `let { template } = import(string);
template("No book||||$1 book||||$1 books", 10)`,
        `let { template } = import(string);
template("No book||||One book||||Two books||||Three books||||$1 books", 0)`,
        `let { template } = import(string);
template("No book||||One book||||Two books||||Three books||||$1 books", 1)`,
        `let { template } = import(string);
template("No book||||One book||||Two books||||Three books||||$1 books", 2)`,
        `let { template } = import(string);
template("No book||||One book||||Two books||||Three books||||$1 books", 3)`,
        `let { template } = import(string);
template("No book||||One book||||Two books||||Three books||||$1 books", 4)`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns a Base64 encoded string from $s.',
      seeAlso: ['string.decode-base64'],
      examples: [
        `let { encode-base64 } = import(string);
encode-base64("Albert")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { base64string: { type: 'string' } },
      variants: [{ argumentNames: ['base64string'] }],
      description: 'Returns a Base64 decoded string from $base64string.',
      seeAlso: ['string.encode-base64'],
      examples: [
        `let { decode-base64 } = import(string);
decode-base64("QWxiZXJ0IPCfkLs=")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns an escaped `URI` string.',
      seeAlso: ['string.decode-uri-component'],
      examples: [
        `let { encode-uri-component } = import(string);
encode-uri-component("Hi everyone!?")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns an un-escaped `URI` string.',
      seeAlso: ['string.encode-uri-component'],
      examples: [
        `let { decode-uri-component } = import(string);
decode-uri-component("Hi%20everyone!%3F%20%F0%9F%91%8D")`,
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
      category: 'string',
      returns: { type: 'string' },
      args: { s: { type: 'string' } },
      variants: [{ argumentNames: ['s'] }],
      description: 'Returns $s with the first character converted to uppercase and the rest to lowercase.',
      seeAlso: ['lower-case', 'upper-case'],
      examples: [
        `let { capitalize } = import(string);
capitalize("albert")`,
        `let { capitalize } = import(string);
capitalize("ALBERT")`,
        `let { capitalize } = import(string);
capitalize("aLBERT")`,
        `let { capitalize } = import(string);
capitalize("")`,
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

export const stringUtilsModule: LitsModule = {
  name: 'string',
  functions: stringUtilsFunctions,
}
