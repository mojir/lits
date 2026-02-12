import { LitsError } from '../../../errors'
import type { Any } from '../../../interface'
// Import from index to ensure namespaces are registered
import { getNamespace } from '../../../namespaces'
import type { NamespaceFunction } from '../../../parser/types'
import type { SourceCodeInfo } from '../../../tokenizer/token'
import { asAny, assertAny } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { asStringOrNumber, assertString, assertStringOrNumber } from '../../../typeGuards/string'
import { compare, deepEqual } from '../../../utils'
import { toFixedArity } from '../../../utils/arity'
import { FUNCTION_SYMBOL } from '../../../utils/symbols'
import type { BuiltinNormalExpressions } from '../../interface'

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
  '==': {
    evaluate: (params, sourceCodeInfo): boolean => {
      return isEqual(params, sourceCodeInfo)
    },
    arity: { min: 1 },
  },
  '≠': {
    evaluate: (params, sourceCodeInfo): boolean => {
      return !isEqual(params, sourceCodeInfo)
    },
    arity: { min: 1 },
    aliases: ['!='],
  },
  'identical?': {
    evaluate: (params): boolean => {
      return isIdentical(params)
    },
    arity: { min: 1 },
  },
  '>': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) <= 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
  },

  '<': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) >= 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
  },
  '>=': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) < 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
    aliases: ['≥'],
  },
  '<=': {
    evaluate: ([first, ...rest], sourceCodeInfo): boolean => {
      let currentValue = asStringOrNumber(first)
      for (const param of rest) {
        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) > 0)
          return false

        currentValue = asStringOrNumber(param)
      }
      return true
    },
    arity: { min: 1 },
    aliases: ['≤'],
  },
  '!': {
    evaluate: ([first]): boolean => !first,
    arity: toFixedArity(1),
  },
  'epoch->iso-date': {
    evaluate: ([ms], sourceCodeInfo): string => {
      assertNumber(ms, sourceCodeInfo)
      return new Date(ms).toISOString()
    },
    arity: toFixedArity(1),
  },
  'iso-date->epoch': {
    evaluate: ([dateTime], sourceCodeInfo): number => {
      assertString(dateTime, sourceCodeInfo)
      const ms = new Date(dateTime).valueOf()
      assertNumber(ms, sourceCodeInfo, { finite: true })
      return ms
    },
    arity: toFixedArity(1),
  },
  'write!': {
    evaluate: (params, sourceCodeInfo): Any => {
      // eslint-disable-next-line no-console
      console.log(...params)

      if (params.length > 0)
        return asAny(params[params.length - 1], sourceCodeInfo)

      return null
    },
    arity: {},
  },
  'boolean': {
    evaluate: ([value]): boolean => {
      return !!value
    },
    arity: toFixedArity(1),
  },
  'compare': {
    evaluate: ([a, b], sourceCodeInfo): number => {
      assertStringOrNumber(a, sourceCodeInfo)
      assertStringOrNumber(b, sourceCodeInfo)
      return compare(a, b, sourceCodeInfo)
    },
    arity: toFixedArity(2),
  },
  'json-parse': {
    evaluate: ([first], sourceCodeInfo): Any => {
      assertString(first, sourceCodeInfo)
      // eslint-disable-next-line ts/no-unsafe-return
      return JSON.parse(first)
    },
    arity: toFixedArity(1),
  },
  'json-stringify': {
    evaluate: ([first, second], sourceCodeInfo): string => {
      assertAny(first, sourceCodeInfo)
      if (second === undefined)
        return JSON.stringify(first)

      assertNumber(second, sourceCodeInfo)
      return JSON.stringify(first, null, second)
    },
    arity: { min: 1, max: 2 },
  },
  'import': {
    evaluate: ([namespaceName], sourceCodeInfo): Record<string, NamespaceFunction> => {
      assertString(namespaceName, sourceCodeInfo)
      const namespace = getNamespace(namespaceName)
      if (!namespace) {
        throw new LitsError(`Unknown namespace: '${namespaceName}'`, sourceCodeInfo)
      }

      // Create an object where each key is a function name and value is a NamespaceFunction
      const result: Record<string, NamespaceFunction> = {}
      for (const [functionName, expression] of Object.entries(namespace.functions)) {
        result[functionName] = {
          [FUNCTION_SYMBOL]: true,
          sourceCodeInfo,
          functionType: 'Namespace',
          namespaceName,
          functionName,
          arity: expression.arity,
        }
        // Also add aliases
        if (expression.aliases) {
          for (const alias of expression.aliases) {
            result[alias] = {
              [FUNCTION_SYMBOL]: true,
              sourceCodeInfo,
              functionType: 'Namespace',
              namespaceName,
              functionName, // Point to the original function
              arity: expression.arity,
            }
          }
        }
      }
      return result
    },
    arity: toFixedArity(1),
  },
}
