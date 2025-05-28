import { describe, expect, it, test } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import { LitsError } from '../errors'
import { assertNumberOfParams, canBeOperator } from '../utils/arity'
import {
  asNonUndefined,
  asUnknownRecord,
  assertNonUndefined,
  assertUnknownRecord,
  isUnknownRecord,
} from '.'

describe('typeGuards index file', () => {
  test('canBeOperator', () => {
    expect(canBeOperator({ max: 1 })).toBe(false)
  })

  it('asNotUndefined', () => {
    expect(() => asNonUndefined(undefined)).toThrow(LitsError)
    expect(asNonUndefined(null)).toBe(null)
    expect(asNonUndefined(false)).toBe(false)
    expect(asNonUndefined(true)).toBe(true)
    expect(asNonUndefined(0)).toBe(0)
    const obj = {}
    expect(asNonUndefined(obj)).toBe(obj)
  })
  it('assertNotUndefined', () => {
    expect(() => assertNonUndefined(undefined)).toThrow(LitsError)
    expect(() => assertNonUndefined(undefined)).toThrow(LitsError)
    expect(() => assertNonUndefined(null)).not.toThrow()
    expect(() => assertNonUndefined(false)).not.toThrow()
    expect(() => assertNonUndefined(true)).not.toThrow()
    expect(() => assertNonUndefined(0)).not.toThrow()
    expect(() => assertNonUndefined({})).not.toThrow()
  })
  it('assertLength', () => {
    expect(() => assertNumberOfParams({ min: 0, max: 0 }, 0, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 0, max: 0 }, 1, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 1, max: 1 }, 1, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 1 }, 0, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 1, max: 1 }, 2, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 2, max: 2 }, 2, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 2, max: 2 }, 1, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 2, max: 2 }, 3, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({}, 0, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, 5, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, 4, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, 3, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, 2, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, 1, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, 0, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ max: 3 }, 5, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ max: 3 }, 4, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ max: 3 }, 3, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, 2, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, 1, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, 0, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 0, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 1, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 2, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 3, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 4, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 5, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, 6, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 0, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 1, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 2, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 3, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 4, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 5, undefined)).toThrow(LitsError)
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, 6, undefined)).toThrow(LitsError)
  })

  it('unknownRecord', () => {
    const valid = [{}, { a: 1 }]
    const invalid = [undefined, null, 0, false, true, '', 'foo', []]
    testTypeGuars(
      {
        valid,
        invalid,
      },
      { is: isUnknownRecord, as: asUnknownRecord, assert: assertUnknownRecord },
    )
  })
})
