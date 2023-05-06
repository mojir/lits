import { AssertionError, LitsError } from '../../../errors'
import { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { any, assertNumberOfParams, litsFunction, string } from '../../../utils/assertion'
import { Type } from '../../../types/Type'

export const assertNormalExpression: BuiltinNormalExpressions = {
  assert: {
    evaluate: (params, debugInfo): Any | Type => {
      if (params.every(Type.isNotType)) {
        const value = params[0]
        const message = params.length === 2 ? params[1] : `${value}`
        string.assert(message, debugInfo)
        if (!value) {
          throw new AssertionError(message, debugInfo)
        }
        return any.as(value, debugInfo)
      } else {
        const valueType = Type.of(params[0])
        valueType.assertIs(Type.truthy, debugInfo)
        return valueType
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert`, debugInfo),
  },
  'assert=': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (first !== second) {
          throw new AssertionError(`Expected ${first} to be ${second}.${message}`, debugInfo)
        }
        return null
      } else {
        const aType = Type.of(params[0])
        const bType = Type.of(params[1])
        aType.assertIntersects(bType, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert=`, debugInfo),
  },
  'assert-not=': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (first === second) {
          throw new AssertionError(`Expected ${first} not to be ${second}.${message}`, debugInfo)
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert-not=`, debugInfo),
  },
  'assert-equal': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (!deepEqual(any.as(first, debugInfo), any.as(second, debugInfo), debugInfo)) {
          throw new AssertionError(
            `Expected\n${JSON.stringify(first, null, 2)}\nto deep equal\n${JSON.stringify(second, null, 2)}.${message}`,
            debugInfo,
          )
        }
        return null
      } else {
        const aType = Type.of(params[0])
        const bType = Type.of(params[1])
        aType.assertIntersects(bType, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert-equal`, debugInfo),
  },
  'assert-not-equal': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (deepEqual(any.as(first, debugInfo), any.as(second, debugInfo), debugInfo)) {
          throw new AssertionError(
            `Expected ${JSON.stringify(first)} not to deep equal ${JSON.stringify(second)}.${message}`,
            debugInfo,
          )
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert-not-equal`, debugInfo),
  },
  'assert>': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (compare(first, second) <= 0) {
          throw new AssertionError(`Expected ${first} to be grater than ${second}.${message}`, debugInfo)
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert>`, debugInfo),
  },
  'assert>=': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (compare(first, second) < 0) {
          throw new AssertionError(`Expected ${first} to be grater than or equal to ${second}.${message}`, debugInfo)
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert>=`, debugInfo),
  },
  'assert<': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (compare(first, second) >= 0) {
          throw new AssertionError(`Expected ${first} to be less than ${second}.${message}`, debugInfo)
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert<`, debugInfo),
  },
  'assert<=': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const [first, second] = params
        let message = params[2]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (compare(first, second) > 0) {
          throw new AssertionError(`Expected ${first} to be less than or equal to ${second}.${message}`, debugInfo)
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2, max: 3 }, arity, `assert<=`, debugInfo),
  },
  'assert-true': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const first = params[0]
        let message = params[0]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (first !== true) {
          throw new AssertionError(`Expected ${first} to be true.${message}`, debugInfo)
        }
        return null
      } else {
        const type = Type.of(params[0])
        type.assertIntersects(Type.false, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-true`, debugInfo),
  },
  'assert-false': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const first = params[0]
        let message = params[0]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (first !== false) {
          throw new AssertionError(`Expected ${first} to be false.${message}`, debugInfo)
        }
        return null
      } else {
        const type = Type.of(params[0])
        type.assertIntersects(Type.false, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-false`, debugInfo),
  },
  'assert-truthy': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const first = params[0]
        let message = params[0]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (!first) {
          throw new AssertionError(`Expected ${first} to be truthy.${message}`, debugInfo)
        }
        return null
      } else {
        const type = Type.of(params[0])
        type.assertIntersects(Type.truthy, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-truthy`, debugInfo),
  },
  'assert-falsy': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const first = params[0]
        let message = params[0]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (first) {
          throw new AssertionError(`Expected ${first} to be falsy.${message}`, debugInfo)
        }
        return null
      } else {
        const type = Type.of(params[0])
        type.assertIntersects(Type.falsy, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-falsy`, debugInfo),
  },
  'assert-nil': {
    evaluate: (params, debugInfo): null | Type => {
      if (params.every(Type.isNotType)) {
        const first = params[0]
        let message = params[0]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        if (first !== null) {
          throw new AssertionError(`Expected ${first} to be nil.${message}`, debugInfo)
        }
        return null
      } else {
        const type = Type.of(params[0])
        type.assertIntersects(Type.nil, debugInfo)
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-nil`, debugInfo),
  },
  'assert-throws': {
    evaluate: (params, debugInfo, contextStack, { executeFunction }): null | Type => {
      if (params.every(Type.isNotType)) {
        const func = params[0]
        let message = params[1]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        litsFunction.assert(func, debugInfo)
        try {
          executeFunction(func, [], contextStack, debugInfo)
        } catch {
          return null
        }

        throw new AssertionError(`Expected function to throw.${message}`, debugInfo)
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-throws`, debugInfo),
  },
  'assert-throws-error': {
    evaluate: (params, debugInfo, contextStack, { executeFunction }): null | Type => {
      if (params.every(Type.isNotType)) {
        const [func, throwMessage] = params
        let message = params[2]
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
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) =>
      assertNumberOfParams({ min: 2, max: 3 }, arity, `assert-throws-error`, debugInfo),
  },
  'assert-not-throws': {
    evaluate: (params, debugInfo, contextStack, { executeFunction }): null | Type => {
      if (params.every(Type.isNotType)) {
        const func = params[0]
        let message = params[0]
        message = typeof message === `string` && message ? ` "${message}"` : ``
        litsFunction.assert(func, debugInfo)
        try {
          executeFunction(func, [], contextStack, debugInfo)
        } catch {
          throw new AssertionError(`Expected function not to throw.${message}`, debugInfo)
        }
        return null
      } else {
        return Type.nil
      }
    },
    validateArity: (arity, debugInfo) =>
      assertNumberOfParams({ min: 1, max: 2 }, arity, `assert-not-throws`, debugInfo),
  },
}
