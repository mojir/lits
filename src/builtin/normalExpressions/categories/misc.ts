import type { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'
import { asAny, assertAny } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { assertString } from '../../../typeGuards/string'
import { assertNumberOfParams } from '../../../typeGuards'

export const miscNormalExpression: BuiltinNormalExpressions = {
  '!=': {
    evaluate: (params): boolean => {
      for (let i = 0; i < params.length - 1; i += 1) {
        for (let j = i + 1; j < params.length; j += 1) {
          if (params[i] === params[j])
            return false
        }
      }

      return true
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },
  '==': {
    evaluate: ([first, ...rest]): boolean => {
      for (const param of rest) {
        if (param !== first)
          return false
      }

      return true
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },
  'equal?': {
    evaluate: ([a, b], sourceCodeInfo): boolean => {
      return deepEqual(asAny(a, sourceCodeInfo), asAny(b, sourceCodeInfo), sourceCodeInfo)
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
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
    validate: node => assertNumberOfParams({ min: 1 }, node),
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
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  '>=': {
    evaluate: ([first, ...rest]): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) < 0)
          return false

        currentValue = param
      }
      return true
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },

  '<=': {
    evaluate: ([first, ...rest]): boolean => {
      let currentValue = first
      for (const param of rest) {
        if (compare(currentValue, param) > 0)
          return false

        currentValue = param
      }
      return true
    },
    validate: node => assertNumberOfParams({ min: 1 }, node),
  },
  '!': {
    evaluate: ([first]): boolean => !first,
    validate: node => assertNumberOfParams(1, node),
  },
  'epoch>iso_date': {
    evaluate: ([ms], sourceCodeInfo): string => {
      assertNumber(ms, sourceCodeInfo)
      return new Date(ms).toISOString()
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'iso_date>epoch': {
    evaluate: ([dateTime], sourceCodeInfo): number => {
      assertString(dateTime, sourceCodeInfo)
      const ms = new Date(dateTime).valueOf()
      assertNumber(ms, sourceCodeInfo, { finite: true })
      return ms
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'write!': {
    evaluate: (params, sourceCodeInfo): Any => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0)
        return asAny(params[params.length - 1], sourceCodeInfo)

      return null
    },
  },
  'boolean': {
    evaluate: ([value]): boolean => {
      return !!value
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'compare': {
    evaluate: ([a, b]): number => {
      return compare(a, b)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'json_parse': {
    evaluate: ([first], sourceCodeInfo): Any => {
      assertString(first, sourceCodeInfo)
      // eslint-disable-next-line ts/no-unsafe-return
      return JSON.parse(first)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  'json_stringify': {
    evaluate: ([first, second], sourceCodeInfo): string => {
      assertAny(first, sourceCodeInfo)
      if (second === undefined)
        return JSON.stringify(first)

      assertNumber(second, sourceCodeInfo)
      return JSON.stringify(first, null, second)
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
}
