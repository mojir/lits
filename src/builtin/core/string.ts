import { LitsError } from '../../errors'
import type { Arr } from '../../interface'
import { assertArray } from '../../typeGuards/array'
import { assertStringOrRegularExpression, isObj } from '../../typeGuards/lits'
import { assertNumber } from '../../typeGuards/number'
import { assertString, assertStringOrNumber } from '../../typeGuards/string'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../interface'

const blankRegexp = /^\s*$/
export const stringNormalExpression: BuiltinNormalExpressions = {
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
      seeAlso: ['++', 'join', 'String-Utils.template', 'String-Utils.string-repeat', 'number'],
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
      seeAlso: ['str', 'number?', 'integer?'],
      examples: [
        'number("10")',
        'number("010")',
        'number("-1.01")',
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
      seeAlso: ['upper-case', 'String-Utils.capitalize'],
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
      seeAlso: ['lower-case', 'String-Utils.capitalize'],
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
      seeAlso: ['String-Utils.trim-left', 'String-Utils.trim-right', 'blank?'],
      examples: [
        'trim("  Albert  ")',
        'trim("   ")',
        'trim("")',
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
      seeAlso: ['split', 'str', '++', 'interpose'],
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
      seeAlso: ['join', 'String-Utils.split-lines'],
      examples: [
        '"Albert Mojir" split " "',
        'split("Albert Mojir", " ")',
        'split("abcdefghijklmnopqrstuvw", #"[aoueiy]")',
        'split("0123456789", "")',
        'split("0123456789", "", 5) map number',
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
      seeAlso: ['trim', 'empty?', 'string?'],
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
