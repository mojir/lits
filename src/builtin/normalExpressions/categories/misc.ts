import type { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'
import { asAny, assertAny } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { assertString } from '../../../typeGuards/string'
import type { SourceCodeInfo } from '../../..'

function isEqual([first, ...rest]: unknown[], sourceCodeInfo: SourceCodeInfo | undefined) {
  const firstAny = asAny(first, sourceCodeInfo)
  for (const param of rest) {
    if (!deepEqual(firstAny, asAny(param, sourceCodeInfo), sourceCodeInfo))
      return false
  }
  return true
}

function isIdentical([first, ...rest]: unknown[]) {
  for (const param of rest) {
    if (param !== first)
      return false
  }

  return true
}

export const miscNormalExpression: BuiltinNormalExpressions = {
  '=': {
    evaluate: (params, sourceCodeInfo): boolean => {
      return isEqual(params, sourceCodeInfo)
    },
    paramCount: { min: 1 },
  },
  '≠': {
    evaluate: (params, sourceCodeInfo): boolean => {
      return !isEqual(params, sourceCodeInfo)
    },
    paramCount: { min: 1 },
    aliases: ['!='],
  },
  'identical?': {
    evaluate: (params): boolean => {
      return isIdentical(params)
    },
    paramCount: { min: 1 },
  },
  '>': {
    evaluate: ([first, ...rest]): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) <= 0)
          return false

        currentValue = param
      }
      return true
    },
    paramCount: { min: 1 },
  },

  '<': {
    evaluate: ([first, ...rest]): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) >= 0)
          return false

        currentValue = param
      }
      return true
    },
    paramCount: { min: 1 },
  },
  '≥': {
    evaluate: ([first, ...rest]): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) < 0)
          return false

        currentValue = param
      }
      return true
    },
    paramCount: { min: 1 },
    aliases: ['>='],
  },
  '≤': {
    evaluate: ([first, ...rest]): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) > 0)
          return false

        currentValue = param
      }
      return true
    },
    paramCount: { min: 1 },
    aliases: ['<='],
  },
  '!': {
    evaluate: ([first]): boolean => !first,
    paramCount: 1,
  },
  'epoch->iso-date': {
    evaluate: ([ms], sourceCodeInfo): string => {
      assertNumber(ms, sourceCodeInfo)
      return new Date(ms).toISOString()
    },
    paramCount: 1,
  },
  'iso-date->epoch': {
    evaluate: ([dateTime], sourceCodeInfo): number => {
      assertString(dateTime, sourceCodeInfo)
      const ms = new Date(dateTime).valueOf()
      assertNumber(ms, sourceCodeInfo, { finite: true })
      return ms
    },
    paramCount: 1,
  },
  'write!': {
    evaluate: (params, sourceCodeInfo): Any => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0)
        return asAny(params[params.length - 1], sourceCodeInfo)

      return null
    },
    paramCount: {},
  },
  'boolean': {
    evaluate: ([value]): boolean => {
      return !!value
    },
    paramCount: 1,
  },
  'compare': {
    evaluate: ([a, b]): number => {
      return compare(a, b)
    },
    paramCount: 2,
  },
  'json-parse': {
    evaluate: ([first], sourceCodeInfo): Any => {
      assertString(first, sourceCodeInfo)
      // eslint-disable-next-line ts/no-unsafe-return
      return JSON.parse(first)
    },
    paramCount: 1,
  },
  'json-stringify': {
    evaluate: ([first, second], sourceCodeInfo): string => {
      assertAny(first, sourceCodeInfo)
      if (second === undefined)
        return JSON.stringify(first)

      assertNumber(second, sourceCodeInfo)
      return JSON.stringify(first, null, second)
    },
    paramCount: { min: 1, max: 2 },
  },
}
