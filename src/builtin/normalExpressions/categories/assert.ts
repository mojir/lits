import type { LitsError } from '../../../errors'
import { AssertionError } from '../../../errors'
import type { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import { assertString, assertStringOrNumber } from '../../../typeGuards/string'
import { asAny } from '../../../typeGuards/lits'

export const assertNormalExpression: BuiltinNormalExpressions = {
  'assert': {
    evaluate: (params, sourceCodeInfo): Any => {
      const value = params[0]
      const message = params.length === 2 ? params[1] : `${value}`
      assertString(message, sourceCodeInfo)
      if (!value)
        throw new AssertionError(message, sourceCodeInfo)

      return asAny(value, sourceCodeInfo)
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (!deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
        throw new AssertionError(
          `Expected ${JSON.stringify(first, null, 2)} to deep equal ${JSON.stringify(second, null, 2)}.${message}`,
          sourceCodeInfo,
        )
      }
      return null
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert!=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
        throw new AssertionError(
          `Expected ${JSON.stringify(first)} not to deep equal ${JSON.stringify(second)}.${message}`,
          sourceCodeInfo,
        )
      }
      return null
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert-gt': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second, sourceCodeInfo) <= 0)
        throw new AssertionError(`Expected ${first} to be grater than ${second}.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert-gte': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second, sourceCodeInfo) < 0)
        throw new AssertionError(`Expected ${first} to be grater than or equal to ${second}.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert-lt': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second, sourceCodeInfo) >= 0)
        throw new AssertionError(`Expected ${first} to be less than ${second}.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert-lte': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second, sourceCodeInfo) > 0)
        throw new AssertionError(`Expected ${first} to be less than or equal to ${second}.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert-true': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== true)
        throw new AssertionError(`Expected ${first} to be true.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert-false': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== false)
        throw new AssertionError(`Expected ${first} to be false.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert-truthy': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (!first)
        throw new AssertionError(`Expected ${first} to be truthy.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert-falsy': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first)
        throw new AssertionError(`Expected ${first} to be falsy.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert-null': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== null)
        throw new AssertionError(`Expected ${first} to be null.${message}`, sourceCodeInfo)

      return null
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert-throws': {
    evaluate: ([func, message], sourceCodeInfo, contextStack, { executeFunction }): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      assertLitsFunction(func, sourceCodeInfo)
      try {
        executeFunction(func, [], contextStack, sourceCodeInfo)
      }
      catch {
        return null
      }
      throw new AssertionError(`Expected function to throw.${message}`, sourceCodeInfo)
    },
    paramCount: { min: 1, max: 2 },
  },
  'assert-throws-error': {
    evaluate: ([func, throwMessage, message], sourceCodeInfo, contextStack, { executeFunction }): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      assertString(throwMessage, sourceCodeInfo)
      assertLitsFunction(func, sourceCodeInfo)
      try {
        executeFunction(func, [], contextStack, sourceCodeInfo)
      }
      catch (error) {
        const errorMessage = (error as LitsError).shortMessage
        if (errorMessage !== throwMessage) {
          throw new AssertionError(
            `Expected function to throw "${throwMessage}", but thrown "${errorMessage}".${message}`,
            sourceCodeInfo,
          )
        }
        return null
      }
      throw new AssertionError(`Expected function to throw "${throwMessage}".${message}`, sourceCodeInfo)
    },
    paramCount: { min: 2, max: 3 },
  },
  'assert-not-throws': {
    evaluate: ([func, message], sourceCodeInfo, contextStack, { executeFunction }): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      assertLitsFunction(func, sourceCodeInfo)
      try {
        executeFunction(func, [], contextStack, sourceCodeInfo)
      }
      catch {
        throw new AssertionError(`Expected function not to throw.${message}`, sourceCodeInfo)
      }
      return null
    },
    paramCount: { min: 1, max: 2 },
  },
}
