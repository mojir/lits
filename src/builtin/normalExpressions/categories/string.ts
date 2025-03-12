import { LitsError } from '../../../errors'
import type { Arr } from '../../../interface'
import type { SourceCodeInfo } from '../../../tokenizer/interface'
import { asNonUndefined } from '../../../typeGuards'
import { assertArray } from '../../../typeGuards/array'
import { assertStringOrRegularExpression, isObj } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { asStringOrNumber, assertString, assertStringOrNumber } from '../../../typeGuards/string'
import { toNonNegativeInteger } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'

const blankRegexp = /^\s*$/
export const stringNormalExpression: BuiltinNormalExpressions = {
  'string-repeat': {
    evaluate: ([str, count], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return str.repeat(count)
    },
    paramCount: 2,
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
    paramCount: {},
  },

  'number': {
    evaluate: ([str], sourceCodeInfo): number => {
      assertString(str, sourceCodeInfo)
      const number = Number(str)
      if (Number.isNaN(number))
        throw new LitsError(`Could not convert '${str}' to a number.`, sourceCodeInfo)

      return number
    },
    paramCount: 1,
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
    paramCount: 1,
  },

  'to-char-code': {
    evaluate: ([str], sourceCodeInfo): number => {
      assertString(str, sourceCodeInfo, { nonEmpty: true })
      return asNonUndefined(str.codePointAt(0), sourceCodeInfo)
    },
    paramCount: 1,
  },

  'lower-case': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.toLowerCase()
    },
    paramCount: 1,
  },

  'upper-case': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.toUpperCase()
    },
    paramCount: 1,
  },

  'trim': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.trim()
    },
    paramCount: 1,
  },

  'trim-left': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.replace(/^\s+/, '')
    },
    paramCount: 1,
  },

  'trim-right': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.replace(/\s+$/, '')
    },
    paramCount: 1,
  },

  'join': {
    evaluate: ([stringList, delimiter], sourceCodeInfo): string => {
      assertArray(stringList, sourceCodeInfo)
      stringList.forEach(str => assertStringOrNumber(str, sourceCodeInfo))
      assertString(delimiter, sourceCodeInfo)
      return stringList.join(delimiter)
    },
    paramCount: 2,
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
    paramCount: { min: 2, max: 3 },
  },
  'split-lines': {
    evaluate: ([str], sourceCodeInfo): string[] => {
      assertString(str, sourceCodeInfo)
      return str.split((/\r\n|\n|\r/)).filter(line => line !== '')
    },
    paramCount: 1,
  },

  'pad-left': {
    evaluate: ([str, length, padString], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(length, sourceCodeInfo, { integer: true })

      if (padString !== undefined)
        assertString(padString, sourceCodeInfo)

      return str.padStart(length, padString)
    },
    paramCount: { min: 2, max: 3 },
  },

  'pad-right': {
    evaluate: ([str, length, padString], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      assertNumber(length, sourceCodeInfo, { integer: true })

      if (padString !== undefined)
        assertString(padString, sourceCodeInfo)

      return str.padEnd(length, padString)
    },
    paramCount: { min: 2, max: 3 },
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
    paramCount: { min: 1, max: 10 },
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
    paramCount: 1,
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
    paramCount: 1,
  },

  'encode-uri-component': {
    evaluate: ([value], sourceCodeInfo): string => {
      assertString(value, sourceCodeInfo)
      return encodeURIComponent(value)
    },
    paramCount: 1,
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
    paramCount: 1,
  },
  'blank?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      if (value === null) {
        return true
      }
      assertString(value, sourceCodeInfo)
      return blankRegexp.test(value)
    },
    paramCount: 1,
  },
  'capitalize': {
    evaluate: ([str], sourceCodeInfo): string => {
      assertString(str, sourceCodeInfo)
      return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase()
    },
    paramCount: 1,
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
