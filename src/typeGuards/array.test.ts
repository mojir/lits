import { describe, it } from 'vitest'
import { testTypeGuars } from '../../__tests__/testUtils'
import {
  asArray,
  asCharArray,
  asStringArray,
  assertArray,
  assertCharArray,
  assertStringArray,
  isCharArray,
  isStringArray,
} from './array'

describe('array type guards', () => {
  const nonArrays: unknown[] = [0, 1, true, false, null, undefined, {}, { 1: 1 }, /foo/, 'bar', '']
  const stringArrays: string[][] = [['foo'], ['foo', 'c']]
  const charArrays: string[][] = [['f'], ['f', 'c']]
  const unknownArray: unknown[] = ['foo', null]

  const allStringArrays = [[], ...stringArrays, ...charArrays]
  const allArrays = [...allStringArrays, unknownArray]

  it('array', () => {
    testTypeGuars(
      {
        valid: allArrays,
        invalid: nonArrays,
      },
      { is: undefined, as: asArray, assert: assertArray },
    )
  })

  it('stringArray', () => {
    testTypeGuars(
      {
        valid: allStringArrays,
        invalid: [...nonArrays, unknownArray],
      },
      { is: isStringArray, as: asStringArray, assert: assertStringArray },
    )
  })

  it('charArray', () => {
    testTypeGuars(
      {
        valid: [[], ...charArrays],
        invalid: [...nonArrays, unknownArray, ...stringArrays],
      },
      { is: isCharArray, as: asCharArray, assert: assertCharArray },
    )
  })
})
