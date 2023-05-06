import { LitsError } from '../../../errors'
import { Any, Arr } from '../../../interface'
import { DebugInfo } from '../../../tokenizer/interface'
import { toNonNegativeInteger } from '../../../utils'
import {
  number,
  object,
  array,
  string,
  assertNumberOfParams,
  stringOrRegExp,
  asValue,
  stringOrNumber,
} from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const stringNormalExpression: BuiltinNormalExpressions = {
  subs: {
    evaluate: ([first, second, third], debugInfo): Any => {
      string.assert(first, debugInfo)
      number.assert(second, debugInfo, { integer: true, nonNegative: true })

      if (third === undefined) {
        return (first as string).substring(second)
      }

      number.assert(third, debugInfo, { gte: second })
      return (first as string).substring(second, third)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `subs`, debugInfo),
  },

  'string-repeat': {
    evaluate: ([str, count], debugInfo): string => {
      string.assert(str, debugInfo)
      number.assert(count, debugInfo, { integer: true, nonNegative: true })

      return str.repeat(count)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `string-repeat`, debugInfo),
  },

  str: {
    evaluate: (params: Arr) => {
      return params.reduce((result: string, param) => {
        const paramStr =
          param === undefined || param === null
            ? ``
            : object.is(param)
            ? JSON.stringify(param)
            : Array.isArray(param)
            ? JSON.stringify(param)
            : `${param}`
        return result + paramStr
      }, ``)
    },
    validateArity: () => undefined,
  },

  number: {
    evaluate: ([str], debugInfo): number => {
      string.assert(str, debugInfo)
      const number = Number(str)
      if (Number.isNaN(number)) {
        throw new LitsError(`Could not convert '${str}' to a number.`, debugInfo)
      }
      return number
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `number`, debugInfo),
  },

  'number-to-string': {
    evaluate: (params, debugInfo): string => {
      const [num, base] = params
      number.assert(num, debugInfo, { finite: true })
      if (params.length === 1) {
        return `${num}`
      } else {
        number.assert(base, debugInfo, { finite: true })
        if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
          throw new LitsError(`Expected "number-to-string" base argument to be 2, 8, 10 or 16, got: ${base}`, debugInfo)
        }
        if (base === 10) {
          return `${num}`
        }
        number.assert(num, debugInfo, { integer: true, nonNegative: true })
        return Number(num).toString(base)
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `number-to-string`, debugInfo),
  },

  'from-char-code': {
    evaluate: ([num], debugInfo): string => {
      number.assert(num, debugInfo, { finite: true })
      const int = toNonNegativeInteger(num)
      try {
        return String.fromCodePoint(int)
      } catch (error) {
        throw new LitsError(error as Error, debugInfo)
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `from-char-code`, debugInfo),
  },

  'to-char-code': {
    evaluate: ([str], debugInfo): number => {
      string.assert(str, debugInfo, { nonEmpty: true })
      return asValue(str.codePointAt(0), debugInfo)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `to-char-code`, debugInfo),
  },

  'lower-case': {
    evaluate: ([str], debugInfo): string => {
      string.assert(str, debugInfo)
      return str.toLowerCase()
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `lower-case`, debugInfo),
  },

  'upper-case': {
    evaluate: ([str], debugInfo): string => {
      string.assert(str, debugInfo)
      return str.toUpperCase()
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `upper-case`, debugInfo),
  },

  trim: {
    evaluate: ([str], debugInfo): string => {
      string.assert(str, debugInfo)
      return str.trim()
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `trim`, debugInfo),
  },

  'trim-left': {
    evaluate: ([str], debugInfo): string => {
      string.assert(str, debugInfo)
      return str.replace(/^\s+/, ``)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `trim-left`, debugInfo),
  },

  'trim-right': {
    evaluate: ([str], debugInfo): string => {
      string.assert(str, debugInfo)
      return str.replace(/\s+$/, ``)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `trim-right`, debugInfo),
  },

  join: {
    evaluate: ([stringList, delimiter], debugInfo): string => {
      array.assert(stringList, debugInfo)
      stringList.forEach(str => string.assert(str, debugInfo))
      string.assert(delimiter, debugInfo)
      return stringList.join(delimiter)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `join`, debugInfo),
  },

  split: {
    evaluate: ([str, stringOrRegExpValue, limit], debugInfo): string[] => {
      string.assert(str, debugInfo)
      stringOrRegExp.assert(stringOrRegExpValue, debugInfo)
      if (limit !== undefined) {
        number.assert(limit, debugInfo, { integer: true, nonNegative: true })
      }
      const delimiter =
        typeof stringOrRegExpValue === `string`
          ? stringOrRegExpValue
          : new RegExp(stringOrRegExpValue.source, stringOrRegExpValue.flags)
      return str.split(delimiter, limit)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `split`, debugInfo),
  },

  'pad-left': {
    evaluate: ([str, length, padString], debugInfo): string => {
      string.assert(str, debugInfo)
      number.assert(length, debugInfo, { integer: true })

      if (padString !== undefined) {
        string.assert(padString, debugInfo)
      }

      return str.padStart(length, padString)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `pad-left`, debugInfo),
  },

  'pad-right': {
    evaluate: ([str, length, padString], debugInfo): string => {
      string.assert(str, debugInfo)
      number.assert(length, debugInfo, { integer: true })

      if (padString !== undefined) {
        string.assert(padString, debugInfo)
      }

      return str.padEnd(length, padString)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `pad-right`, debugInfo),
  },

  template: {
    evaluate: ([templateString, ...placeholders], debugInfo): string => {
      string.assert(templateString, debugInfo)
      array.assert(placeholders, debugInfo)
      const templateStrings = templateString.split(`||||`)
      if (templateStrings.length <= 1) {
        return applyPlaceholders(templateStrings[0] as string, placeholders, debugInfo)
      } else {
        // Pluralisation
        const count = placeholders[0]
        number.assert(count, debugInfo, { integer: true, nonNegative: true })
        const stringPlaceholders = [`${count}`, ...placeholders.slice(1)] as string[]
        if (templateStrings.length === 2) {
          // Exactly two valiants.
          // First variant (singular) for count = 1, Second variant (plural) for count = 0 or count > 1

          const placehoder = templateStrings[count === 1 ? 0 : 1] as string
          return applyPlaceholders(placehoder, stringPlaceholders, debugInfo)
        } else {
          // More than two variant:
          // Use count as index
          // If count >= number of variants, use last variant

          const placehoder = templateStrings[Math.min(count, templateStrings.length - 1)] as string
          return applyPlaceholders(placehoder, stringPlaceholders, debugInfo)
        }
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 10 }, arity, `template`, debugInfo),
  },

  'encode-base64': {
    evaluate: ([value], debugInfo): string => {
      string.assert(value, debugInfo)
      return btoa(
        encodeURIComponent(value).replace(/%([0-9A-F]{2})/g, (_match, p1) => {
          return String.fromCharCode(parseInt(p1, 16))
        }),
      )
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `encode-base64`, debugInfo),
  },

  'decode-base64': {
    evaluate: ([value], debugInfo): string => {
      string.assert(value, debugInfo)
      try {
        return decodeURIComponent(
          Array.prototype.map
            .call(atob(value), c => {
              return `%` + (`00` + c.charCodeAt(0).toString(16)).slice(-2)
            })
            .join(``),
        )
      } catch (error) {
        throw new LitsError(error as Error, debugInfo)
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `decode-base64`, debugInfo),
  },

  'encode-uri-component': {
    evaluate: ([value], debugInfo): string => {
      string.assert(value, debugInfo)
      return encodeURIComponent(value)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `encode-uri-component`, debugInfo),
  },

  'decode-uri-component': {
    evaluate: ([value], debugInfo): string => {
      string.assert(value, debugInfo)
      try {
        return decodeURIComponent(value)
      } catch (error) {
        throw new LitsError(error as Error, debugInfo)
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `decode-uri-component`, debugInfo),
  },
}

const doubleDollarRegexp = /\$\$/g
function applyPlaceholders(templateString: string, placeholders: unknown[], debugInfo?: DebugInfo): string {
  for (let i = 0; i < 9; i += 1) {
    // Matches $1, $2, ..., $9
    // Does not match $$1
    // But does match $$$1, (since the two first '$' will later be raplaced with a single '$'
    const re = new RegExp(`(\\$\\$|[^$]|^)\\$${i + 1}`, `g`)
    if (re.test(templateString)) {
      const placeHolder = stringOrNumber.as(placeholders[i], debugInfo)
      templateString = templateString.replace(re, `$1${placeHolder}`)
    }
  }
  templateString = templateString.replace(doubleDollarRegexp, `$`)
  return templateString
}
