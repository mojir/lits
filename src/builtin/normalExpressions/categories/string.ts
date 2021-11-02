import { LitsError } from '../../../errors'
import { Any, Arr } from '../../../interface'
import { NormalExpressionNode } from '../../../parser/interface'
import { SourceCodeInfo } from '../../../tokenizer/interface'
import { toNonNegativeInteger } from '../../../utils'
import {
  number,
  object,
  array,
  string,
  assertNumberOfParams,
  stringArray,
  stringOrRegExp,
  asValue,
} from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'

export const stringNormalExpression: BuiltinNormalExpressions = {
  subs: {
    evaluate: ([first, second, third], sourceCodeInfo): Any => {
      string.assert(first, sourceCodeInfo)
      number.assert(second, sourceCodeInfo, { integer: true, nonNegative: true })

      if (third === undefined) {
        return (first as string).substring(second)
      }

      number.assert(third, sourceCodeInfo, { gte: second })
      return (first as string).substring(second, third)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 2, max: 3 }, node),
  },

  'string-repeat': {
    evaluate: ([str, count], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      number.assert(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return str.repeat(count)
    },
    validate: node => assertNumberOfParams(2, node),
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
  },

  number: {
    evaluate: ([str], sourceCodeInfo): number => {
      string.assert(str, sourceCodeInfo)
      const number = Number(str)
      if (Number.isNaN(number)) {
        throw new LitsError(`Could not convert '${str}' to a number`, sourceCodeInfo)
      }
      return number
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'number-to-string': {
    evaluate: (params, sourceCodeInfo): string => {
      const [num, base] = params
      number.assert(num, sourceCodeInfo, { finite: true })
      if (params.length === 1) {
        return `${num}`
      } else {
        number.assert(base, sourceCodeInfo, { finite: true })
        if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
          throw new LitsError(
            `Expected "number-to-string" base argument to be 2, 8, 10 or 16, got: ${base}`,
            sourceCodeInfo,
          )
        }
        if (base === 10) {
          return `${num}`
        }
        number.assert(num, sourceCodeInfo, { integer: true, nonNegative: true })
        return Number(num).toString(base)
      }
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1, max: 2 }, node),
  },

  'from-char-code': {
    evaluate: ([num], sourceCodeInfo): string => {
      number.assert(num, sourceCodeInfo, { finite: true })
      const int = toNonNegativeInteger(num)

      return String.fromCodePoint(int)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'to-char-code': {
    evaluate: ([str], sourceCodeInfo): number => {
      string.assert(str, sourceCodeInfo, { nonEmpty: true })
      return asValue(str.codePointAt(0), sourceCodeInfo)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'lower-case': {
    evaluate: ([str], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      return str.toLowerCase()
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'upper-case': {
    evaluate: ([str], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      return str.toUpperCase()
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  trim: {
    evaluate: ([str], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      return str.trim()
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'trim-left': {
    evaluate: ([str], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      return str.replace(/^\s+/, ``)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  'trim-right': {
    evaluate: ([str], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      return str.replace(/\s+$/, ``)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(1, node),
  },

  join: {
    evaluate: ([stringList, delimiter], sourceCodeInfo): string => {
      array.assert(stringList, sourceCodeInfo)
      stringList.forEach(str => string.assert(str, sourceCodeInfo))
      string.assert(delimiter, sourceCodeInfo)
      return stringList.join(delimiter)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams(2, node),
  },

  split: {
    evaluate: ([str, delimiter, limit], sourceCodeInfo): string[] => {
      string.assert(str, sourceCodeInfo)
      stringOrRegExp.assert(delimiter, sourceCodeInfo)
      if (limit !== undefined) {
        number.assert(limit, sourceCodeInfo, { integer: true, nonNegative: true })
      }
      return str.split(delimiter, limit)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 2, max: 3 }, node),
  },

  'pad-left': {
    evaluate: ([str, length, padString], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      number.assert(length, sourceCodeInfo, { integer: true })

      if (padString !== undefined) {
        string.assert(padString, sourceCodeInfo)
      }

      return str.padStart(length, padString)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 2, max: 3 }, node),
  },

  'pad-right': {
    evaluate: ([str, length, padString], sourceCodeInfo): string => {
      string.assert(str, sourceCodeInfo)
      number.assert(length, sourceCodeInfo, { integer: true })

      if (padString !== undefined) {
        string.assert(padString, sourceCodeInfo)
      }

      return str.padEnd(length, padString)
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 2, max: 3 }, node),
  },

  template: {
    evaluate: ([templateString, ...placeholders], sourceCodeInfo): string => {
      string.assert(templateString, sourceCodeInfo)
      const templateStrings = templateString.split(`||||`)
      if (templateStrings.length === 1) {
        stringArray.assert(placeholders, sourceCodeInfo)
        return applyPlaceholders(templateStrings[0] as string, placeholders, sourceCodeInfo)
      } else if (templateStrings.length === 2) {
        const firstPlaceholder = placeholders[0]
        number.assert(firstPlaceholder, sourceCodeInfo, { integer: true, nonNegative: true })
        const stringPlaceholders = [`${firstPlaceholder}`, ...placeholders.slice(1)] as string[]
        if (firstPlaceholder === 1) {
          return applyPlaceholders(templateStrings[0] as string, stringPlaceholders, sourceCodeInfo)
        } else {
          return applyPlaceholders(templateStrings[1] as string, stringPlaceholders, sourceCodeInfo)
        }
      } else {
        throw new LitsError(`Invalid template string, only one "||||" separator allowed`, sourceCodeInfo)
      }
    },
    validate: (node: NormalExpressionNode): void => assertNumberOfParams({ min: 1, max: 10 }, node),
  },
}

const doubleDollarRegexp = /\$\$/g
function applyPlaceholders(templateString: string, placeholders: string[], sourceCodeInfo: SourceCodeInfo): string {
  for (let i = 0; i < 9; i += 1) {
    const re = new RegExp(`(?<=^|[^$]|\\$\\$)\\$${i + 1}`, `g`)
    if (re.test(templateString)) {
      const placeholder = placeholders[i]
      string.assert(placeholder, sourceCodeInfo)
      templateString = templateString.replace(re, placeholder)
    }
  }
  return templateString.replace(doubleDollarRegexp, `$`)
}
