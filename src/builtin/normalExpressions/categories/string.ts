import { LitsError } from '../../../errors'
import { Any, Arr } from '../../../interface'
import { NormalExpressionNode } from '../../../parser/interface'
import { TokenMeta } from '../../../tokenizer/interface'
import {
  assertLength,
  assertNonNegativeInteger,
  assertFiniteNumber,
  assertNumberGte,
  assertString,
  assertInteger,
  assertArr,
  assertStringOrRegExp,
  assertStringArray,
  isObj,
  assertNonEmptyString,
  asNotUndefined,
  toNonNegativeInteger,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const stringNormalExpression: BuiltinNormalExpressions = {
  subs: {
    evaluate: ([first, second, third], meta): Any => {
      assertString(first, meta)
      assertNonNegativeInteger(second, meta)

      if (third === undefined) {
        return (first as string).substring(second)
      }

      assertNumberGte(third, second, meta)
      return (first as string).substring(second, third)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 2, max: 3 }, node),
  },

  'string-repeat': {
    evaluate: ([string, count], meta): string => {
      assertString(string, meta)
      assertNonNegativeInteger(count, meta)

      return string.repeat(count)
    },
    validate: node => assertLength(2, node),
  },

  str: {
    evaluate: (params: Arr) => {
      return params.reduce((result: string, param) => {
        const paramStr =
          param === undefined || param === null
            ? ``
            : isObj(param)
            ? JSON.stringify(param)
            : Array.isArray(param)
            ? JSON.stringify(param)
            : `${param}`
        return result + paramStr
      }, ``)
    },
  },

  number: {
    evaluate: ([str], meta): number => {
      assertString(str, meta)
      const number = Number(str)
      if (Number.isNaN(number)) {
        throw new LitsError(`Could not convert '${str}' to a number`, meta)
      }
      return number
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'number-to-string': {
    evaluate: (params, meta): string => {
      const [number, base] = params
      assertFiniteNumber(number, meta)
      if (params.length === 1) {
        return `${number}`
      } else {
        assertFiniteNumber(base, meta)
        if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
          throw new LitsError(`Expected "number-to-string" base argument to be 2, 8, 10 or 16, got: ${base}`, meta)
        }
        if (base === 10) {
          return `${number}`
        }
        assertNonNegativeInteger(number, meta)
        return Number(number).toString(base)
      }
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1, max: 2 }, node),
  },

  'from-char-code': {
    evaluate: ([number], meta): string => {
      assertFiniteNumber(number, meta)
      const int = toNonNegativeInteger(number)

      return String.fromCodePoint(int)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'to-char-code': {
    evaluate: ([str], meta): number => {
      assertNonEmptyString(str, meta)
      return asNotUndefined(str.codePointAt(0), meta)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'lower-case': {
    evaluate: ([str], meta): string => {
      assertString(str, meta)
      return str.toLowerCase()
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'upper-case': {
    evaluate: ([str], meta): string => {
      assertString(str, meta)
      return str.toUpperCase()
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  trim: {
    evaluate: ([str], meta): string => {
      assertString(str, meta)
      return str.trim()
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'trim-left': {
    evaluate: ([str], meta): string => {
      assertString(str, meta)
      return str.replace(/^\s+/, ``)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  'trim-right': {
    evaluate: ([str], meta): string => {
      assertString(str, meta)
      return str.replace(/\s+$/, ``)
    },
    validate: (node: NormalExpressionNode): void => assertLength(1, node),
  },

  join: {
    evaluate: ([stringList, delimiter], meta): string => {
      assertArr(stringList, meta)
      stringList.forEach(str => assertString(str, meta))
      assertString(delimiter, meta)
      return stringList.join(delimiter)
    },
    validate: (node: NormalExpressionNode): void => assertLength(2, node),
  },

  split: {
    evaluate: ([str, delimiter, limit], meta): string[] => {
      assertString(str, meta)
      assertStringOrRegExp(delimiter, meta)
      if (limit !== undefined) {
        assertNonNegativeInteger(limit, meta)
      }
      return str.split(delimiter, limit)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 2, max: 3 }, node),
  },

  'pad-left': {
    evaluate: ([str, length, padString], meta): string => {
      assertString(str, meta)
      assertInteger(length, meta)

      if (padString !== undefined) {
        assertString(padString, meta)
      }

      return str.padStart(length, padString)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 2, max: 3 }, node),
  },

  'pad-right': {
    evaluate: ([str, length, padString], meta): string => {
      assertString(str, meta)
      assertInteger(length, meta)

      if (padString !== undefined) {
        assertString(padString, meta)
      }

      return str.padEnd(length, padString)
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 2, max: 3 }, node),
  },

  template: {
    evaluate: ([templateString, ...placeholders], meta): string => {
      assertString(templateString, meta)
      const templateStrings = templateString.split(`||||`)
      if (templateStrings.length === 1) {
        assertStringArray(placeholders, meta)
        return applyPlaceholders(templateStrings[0] as string, placeholders, meta)
      } else if (templateStrings.length === 2) {
        const firstPlaceholder = placeholders[0]
        assertNonNegativeInteger(firstPlaceholder, meta)
        const stringPlaceholders = [`${firstPlaceholder}`, ...placeholders.slice(1)] as string[]
        if (firstPlaceholder === 1) {
          return applyPlaceholders(templateStrings[0] as string, stringPlaceholders, meta)
        } else {
          return applyPlaceholders(templateStrings[1] as string, stringPlaceholders, meta)
        }
      } else {
        throw new LitsError(`Invalid template string, only one "||||" separator allowed`, meta)
      }
    },
    validate: (node: NormalExpressionNode): void => assertLength({ min: 1, max: 10 }, node),
  },
}

const doubleDollarRegexp = /\$\$/g
function applyPlaceholders(templateString: string, placeholders: string[], meta: TokenMeta): string {
  for (let i = 0; i < 9; i += 1) {
    const re = new RegExp(`(?<=^|[^$]|\\$\\$)\\$${i + 1}`, `g`)
    if (re.test(templateString)) {
      const placeholder = placeholders[i]
      assertString(placeholder, meta)
      templateString = templateString.replace(re, placeholder)
    }
  }
  return templateString.replace(doubleDollarRegexp, `$`)
}
