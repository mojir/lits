import type { LitsError } from '../../../errors'
import { AssertionError } from '../../../errors'
import type { Any } from '../../../interface'
import { compare, deepEqual } from '../../../utils'
import type { BuiltinNormalExpressions } from '../../../builtin/interface'
import { asAny, assertFunctionLike, isColl, isObj, isRegularExpression, isSeq } from '../../../typeGuards/lits'
import { isLitsFunction } from '../../../typeGuards/litsFunction'
import { isNumber } from '../../../typeGuards/number'
import { assertString, assertStringOrNumber } from '../../../typeGuards/string'
import { isGrid, isMatrix, isVector } from '../../../typeGuards/annotatedArrays'
import type { LitsModule } from '../interface'
import { moduleDocs } from './docs'

const assertNormalExpression: BuiltinNormalExpressions = {
  'assert': {
    evaluate: (params, sourceCodeInfo): Any => {
      const value = params[0]
      const message = params.length === 2 ? params[1] : `${value}`
      assertString(message, sourceCodeInfo)
      if (!value)
        throw new AssertionError(message, sourceCodeInfo)

      return asAny(value, sourceCodeInfo)
    },
    arity: { min: 1, max: 2 },
  },
  'assert=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
        throw new AssertionError(
          `Expected ${JSON.stringify(first, null, 2)} to deep equal ${JSON.stringify(second, null, 2)}.${message}`,
          sourceCodeInfo,
        )
      }
      return null
    },
    arity: { min: 2, max: 3 },
  },
  'assert!=': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
        throw new AssertionError(
          `Expected ${JSON.stringify(first)} not to deep equal ${JSON.stringify(second)}.${message}`,
          sourceCodeInfo,
        )
      }
      return null
    },
    arity: { min: 2, max: 3 },
  },
  'assert-gt': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (compare(first, second, sourceCodeInfo) <= 0)
        throw new AssertionError(`Expected ${first} to be grater than ${second}.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 2, max: 3 },
  },
  'assert-gte': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (compare(first, second, sourceCodeInfo) < 0)
        throw new AssertionError(`Expected ${first} to be grater than or equal to ${second}.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 2, max: 3 },
  },
  'assert-lt': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (compare(first, second, sourceCodeInfo) >= 0)
        throw new AssertionError(`Expected ${first} to be less than ${second}.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 2, max: 3 },
  },
  'assert-lte': {
    evaluate: ([first, second, message], sourceCodeInfo): null => {
      assertStringOrNumber(first, sourceCodeInfo)
      assertStringOrNumber(second, sourceCodeInfo)
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (compare(first, second, sourceCodeInfo) > 0)
        throw new AssertionError(`Expected ${first} to be less than or equal to ${second}.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 2, max: 3 },
  },
  'assert-true': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (first !== true)
        throw new AssertionError(`Expected ${first} to be true.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-false': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (first !== false)
        throw new AssertionError(`Expected ${first} to be false.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-truthy': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!first)
        throw new AssertionError(`Expected ${first} to be truthy.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-falsy': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (first)
        throw new AssertionError(`Expected ${first} to be falsy.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-null': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (first !== null)
        throw new AssertionError(`Expected ${first} to be null.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-throws': {
    evaluate: ([func, message], sourceCodeInfo, contextStack, { executeFunction }): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      assertFunctionLike(func, sourceCodeInfo)
      try {
        executeFunction(func, [], contextStack, sourceCodeInfo)
      }
      catch {
        return null
      }
      throw new AssertionError(`Expected function to throw.${message}`, sourceCodeInfo)
    },
    arity: { min: 1, max: 2 },
  },
  'assert-throws-error': {
    evaluate: ([func, throwMessage, message], sourceCodeInfo, contextStack, { executeFunction }): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      assertString(throwMessage, sourceCodeInfo)
      assertFunctionLike(func, sourceCodeInfo)
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
    arity: { min: 2, max: 3 },
  },
  'assert-not-throws': {
    evaluate: ([func, message], sourceCodeInfo, contextStack, { executeFunction }): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      assertFunctionLike(func, sourceCodeInfo)
      try {
        executeFunction(func, [], contextStack, sourceCodeInfo)
      }
      catch {
        throw new AssertionError(`Expected function not to throw.${message}`, sourceCodeInfo)
      }
      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-array': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!Array.isArray(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be an array.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-boolean': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (typeof first !== 'boolean')
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a boolean.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-collection': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isColl(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a collection.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-function': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isLitsFunction(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a function.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-grid': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isGrid(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a grid.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-integer': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (typeof first !== 'number' || !isNumber(first, { integer: true }))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be an integer.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-matrix': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isMatrix(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a matrix.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-number': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (typeof first !== 'number')
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a number.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-object': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isObj(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be an object.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-regexp': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isRegularExpression(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a regexp.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-sequence': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isSeq(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a sequence.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-string': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (typeof first !== 'string')
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a string.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
  'assert-vector': {
    evaluate: ([first, message], sourceCodeInfo): null => {
      if (message !== undefined) {
        assertString(message, sourceCodeInfo)
        message = ` ${message}`
      }
      message ??= ''
      if (!isVector(first))
        throw new AssertionError(`Expected ${JSON.stringify(first)} to be a vector.${message}`, sourceCodeInfo)

      return null
    },
    arity: { min: 1, max: 2 },
  },
}

for (const [key, docs] of Object.entries(moduleDocs)) {
  if (assertNormalExpression[key])
    assertNormalExpression[key].docs = docs
}

export const assertModule: LitsModule = {
  name: 'Assert',
  functions: assertNormalExpression,
}
