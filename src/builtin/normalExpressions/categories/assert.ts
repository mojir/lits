import { AssertionError, LitsError } from '../../../errors'
import { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { any, assertNumberOfParams, litsFunction, string } from '../../../utils/assertion'

export const assertNormalExpression: BuiltinNormalExpressions = {
  assert: {
    evaluate: (params, debugInfo): Any => {
      const value = params[0]
      const message = params.length === 2 ? params[1] : `${value}`
      string.assert(message, debugInfo)
      if (!value) {
        throw new AssertionError(message, debugInfo)
      }
      return any.as(value, debugInfo)
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert=': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (first !== second) {
        throw new AssertionError(`Expected ${first} to be ${second}.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-not=': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (first === second) {
        throw new AssertionError(`Expected ${first} not to be ${second}.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-equal': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (!deepEqual(any.as(first, debugInfo), any.as(second, debugInfo), debugInfo)) {
        throw new AssertionError(
          `Expected\n${JSON.stringify(first, null, 2)}\nto deep equal\n${JSON.stringify(second, null, 2)}.${message}`,
          debugInfo,
        )
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-not-equal': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (deepEqual(any.as(first, debugInfo), any.as(second, debugInfo), debugInfo)) {
        throw new AssertionError(
          `Expected ${JSON.stringify(first)} not to deep equal ${JSON.stringify(second)}.${message}`,
          debugInfo,
        )
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert>': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (compare(first, second) <= 0) {
        throw new AssertionError(`Expected ${first} to be grater than ${second}.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert>=': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (compare(first, second) < 0) {
        throw new AssertionError(`Expected ${first} to be grater than or equal to ${second}.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert<': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (compare(first, second) >= 0) {
        throw new AssertionError(`Expected ${first} to be less than ${second}.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert<=': {
    evaluate: ([first, second, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (compare(first, second) > 0) {
        throw new AssertionError(`Expected ${first} to be less than or equal to ${second}.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-true': {
    evaluate: ([first, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (first !== true) {
        throw new AssertionError(`Expected ${first} to be true.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-false': {
    evaluate: ([first, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (first !== false) {
        throw new AssertionError(`Expected ${first} to be false.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-truthy': {
    evaluate: ([first, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (!first) {
        throw new AssertionError(`Expected ${first} to be truthy.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-falsy': {
    evaluate: ([first, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (first) {
        throw new AssertionError(`Expected ${first} to be falsy.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-nil': {
    evaluate: ([first, message], debugInfo): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      if (first !== null) {
        throw new AssertionError(`Expected ${first} to be nil.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-throws': {
    evaluate: ([func, message], debugInfo, contextStack, { executeFunction }): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      litsFunction.assert(func, debugInfo)
      try {
        executeFunction(func, [], contextStack, debugInfo)
      } catch {
        return null
      }
      throw new AssertionError(`Expected function to throw.${message}`, debugInfo)
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
  'assert-throws-error': {
    evaluate: ([func, throwMessage, message], debugInfo, contextStack, { executeFunction }): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      string.assert(throwMessage, debugInfo)
      litsFunction.assert(func, debugInfo)
      try {
        executeFunction(func, [], contextStack, debugInfo)
      } catch (error) {
        const errorMessage = (error as LitsError).shortMessage
        if (errorMessage !== throwMessage) {
          throw new AssertionError(
            `Expected function to throw "${throwMessage}", but thrown "${errorMessage}".${message}`,
            debugInfo,
          )
        }
        return null
      }
      throw new AssertionError(`Expected function to throw "${throwMessage}".${message}`, debugInfo)
    },
    validate: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  },
  'assert-not-throws': {
    evaluate: ([func, message], debugInfo, contextStack, { executeFunction }): null => {
      message = typeof message === `string` && message ? ` "${message}"` : ``
      litsFunction.assert(func, debugInfo)
      try {
        executeFunction(func, [], contextStack, debugInfo)
      } catch {
        throw new AssertionError(`Expected function not to throw.${message}`, debugInfo)
      }
      return null
    },
    validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  },
}
