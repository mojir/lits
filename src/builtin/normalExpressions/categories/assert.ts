import type { LitsError } from '../../../errors'
import { AssertionError } from '../../../errors'
import type { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'
import { assertString } from '../../../typeGuards/string'
import { asAny } from '../../../typeGuards/lits'
import { assertNumberOfParams } from '../../../typeGuards'

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
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== second)
        throw new AssertionError(`Expected ${first} to be ${second}.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-not=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first === second)
        throw new AssertionError(`Expected ${first} not to be ${second}.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-equal': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (!deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
        throw new AssertionError(
          `Expected\n${JSON.stringify(first, null, 2)}\nto deep equal\n${JSON.stringify(second, null, 2)}.${message}`,
          sourceCodeInfo,
        )
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-not-equal': {
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
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert>': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second) <= 0)
        throw new AssertionError(`Expected ${first} to be grater than ${second}.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert>=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second) < 0)
        throw new AssertionError(`Expected ${first} to be grater than or equal to ${second}.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert<': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second) >= 0)
        throw new AssertionError(`Expected ${first} to be less than ${second}.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert<=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (compare(first, second) > 0)
        throw new AssertionError(`Expected ${first} to be less than or equal to ${second}.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-true': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== true)
        throw new AssertionError(`Expected ${first} to be true.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-false': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== false)
        throw new AssertionError(`Expected ${first} to be false.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-truthy': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (!first)
        throw new AssertionError(`Expected ${first} to be truthy.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-falsy': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first)
        throw new AssertionError(`Expected ${first} to be falsy.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-nil': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      message = typeof message === 'string' && message ? ` "${message}"` : ''
      if (first !== null)
        throw new AssertionError(`Expected ${first} to be nil.${message}`, sourceCodeInfo)

      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
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
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
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
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
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
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
}
