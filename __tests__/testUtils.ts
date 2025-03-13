import { expect } from 'vitest'
import type { Context, LitsFunction, LitsParams } from '../src'
import { Lits } from '../src'
import type { Analysis } from '../src/analyze'
import type { LitsError } from '../src/errors'
import { ContextStackImpl } from '../src/evaluator/ContextStack'
import type { Obj } from '../src/interface'
import { isUnknownRecord } from '../src/typeGuards'
import { isLitsFunction } from '../src/typeGuards/litsFunction'
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

export function getUndefinedSymbolNames(result: Analysis): Set<string> {
  const names = [...result.unresolvedSymbols].map(entry => entry.symbol)
  return new Set<string>(names)
}

export function getLitsVariants() {
  const variants = [new Lits(), new Lits({ debug: true })]
  return {
    run(program: string, LitsParams?: LitsParams): unknown {
      const [result1, result2] = variants.map((l) => {
        try {
          return l.run(program, LitsParams)
        }
        catch (error) {
          return { _error_: error }
        }
      })

      if (isUnknownRecord(result1) && result1._error_ instanceof Error) {
        expect(isUnknownRecord(result2)).toBe(true)
        expect((result2 as Record<string, unknown>)._error_).toBeInstanceOf(Error)
        expect(((result2 as Record<string, unknown>)._error_ as LitsError).name).toBe(
          (result1._error_ as LitsError).name,
        )
        throw result1._error_
      }

      if (isLitsFunction(result1)) {
        expect(isLitsFunction(result2)).toBe(true)
        expect((result2 as LitsFunction).t).toBe(result1.t)
        return result1
      }
      if (result1 instanceof Error) {
        expect(result2).toBeInstanceOf(Error)
        expect((result2 as LitsError).name).toBe(result1.name)
        return result1
      }
      expect(result1).toStrictEqual(result2)
      return result1
    },
    analyze(program: string): Analysis {
      const results = variants.map(l => l.analyze(program))
      const result1 = results[0] as Analysis
      const result2 = results[1] as Analysis
      const us1 = getUndefinedSymbolNames(result1)
      const us2 = getUndefinedSymbolNames(result2)
      expect(us1).toStrictEqual(us2)
      return result1
    },
  }
}

export function createContextStackWithGlobalContext(context: Context) {
  return new ContextStackImpl({ contexts: [context] })
}
