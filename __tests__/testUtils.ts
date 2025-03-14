import { expect } from 'vitest'
import type { Context } from '../src'
import { ContextStackImpl } from '../src/evaluator/ContextStack'
import type { Obj } from '../src/interface'
import { isRegularExpression } from '../src/typeGuards/lits'

export interface TypeGuardTestData {
  valid: unknown[]
  invalid: unknown[]
}

export function testTypeGuars(
  testData: TypeGuardTestData,
  { is, as, assert }: { is: Function | undefined, as: Function, assert: Function },
) {
  testData.valid.forEach((validData) => {
    if (is)
      expect(is(validData)).toBe(true)

    // eslint-disable-next-line ts/no-unsafe-return
    expect(() => assert(validData)).not.toThrow()
    expect(as(validData)).toBe(validData)
  })
  testData.invalid.forEach((validData) => {
    if (is)
      expect(is(validData)).toBe(false)

    // eslint-disable-next-line ts/no-unsafe-return
    expect(() => assert(validData)).toThrow()
    // eslint-disable-next-line ts/no-unsafe-return
    expect(() => as(validData)).toThrow()
  })
}

interface Primitives extends Obj {
  string: string
  emptyString: ''
  integer: number
  float: number
  negativeNumber: number
  negativeFloat: number
  zero: 0
  null: null
  boolean: boolean
  true: true
  false: false
}

export interface TestData extends Primitives {
  simpleObject: Primitives
  stringArray: string[]
  numberArray: number[]
  nestedArray: unknown[]
  mixedArray: unknown[]
  emptyArray: []
  emptyObject: Record<string, unknown>
}

const privateTestData: TestData = {
  boolean: true,
  emptyString: '',
  false: false,
  float: 42.42,
  integer: 42,
  mixedArray: [true, 0, 'Albert', null, '', 42, -42.42],
  negativeFloat: -42.42,
  negativeNumber: -42,
  null: null,
  numberArray: [0, 1, 2, 3, 4, 5],
  simpleObject: {
    boolean: true,
    emptyString: '',
    false: false,
    float: 42.42,
    integer: 42,
    negativeFloat: -42.42,
    negativeNumber: -42,
    null: null,
    string: 'Albert',
    true: true,
    zero: 0,
  },
  string: 'Albert',
  stringArray: ['Albert', 'Mojir', 'Lits', 'Immutable'],
  true: true,
  zero: 0,
  emptyArray: [],
  nestedArray: [2, [3, 4], 5],
  emptyObject: {},
}

let testData: TestData

export function createTestData(): TestData {
  // eslint-disable-next-line ts/no-unsafe-assignment
  testData = JSON.parse(JSON.stringify(privateTestData))
  return testData
}

export function checkTestData(): void {
  const stringifiedTestData = JSON.stringify(testData, null, 4)
  const stringifiedPrivateTestData = JSON.stringify(privateTestData, null, 4)
  if (stringifiedTestData !== stringifiedPrivateTestData)
    throw new Error(`Expected testData to not change got:\n${stringifiedTestData}\nExpected${stringifiedPrivateTestData}`)
}

export function regexpEquals(udr: unknown, r: RegExp): boolean {
  if (!isRegularExpression(udr))
    return false

  const sortedUdrFlags = udr.f.split('').sort().join('')
  const sortedRFlags = r.flags.split('').sort().join('')
  return udr.s === r.source && sortedRFlags === sortedUdrFlags
}

export function createContextStackWithGlobalContext(context: Context) {
  return new ContextStackImpl({ contexts: [context] })
}
