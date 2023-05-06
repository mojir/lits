/* istanbul ignore file */

import { AnalyzeResult } from '../src/analyze/interface'
import { Obj } from '../src/interface'
import { regularExpression } from '../src/utils/assertion'

interface Primitives extends Obj {
  string: string
  emptyString: ``
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
  emptyString: ``,
  false: false,
  float: 42.42,
  integer: 42,
  mixedArray: [true, 0, `Albert`, null, ``, 42, -42.42],
  negativeFloat: -42.42,
  negativeNumber: -42,
  null: null,
  numberArray: [0, 1, 2, 3, 4, 5],
  simpleObject: {
    boolean: true,
    emptyString: ``,
    false: false,
    float: 42.42,
    integer: 42,
    negativeFloat: -42.42,
    negativeNumber: -42,
    null: null,
    string: `Albert`,
    true: true,
    zero: 0,
  },
  string: `Albert`,
  stringArray: [`Albert`, `Mojir`, `Lits`, `Immutable`],
  true: true,
  zero: 0,
  emptyArray: [],
  nestedArray: [2, [3, 4], 5],
  emptyObject: {},
}

let testData: TestData

export function createTestData(): TestData {
  testData = JSON.parse(JSON.stringify(privateTestData))
  return testData
}

export function checkTestData(): void {
  const stringifiedTestData = JSON.stringify(testData, null, 4)
  const stringifiedPrivateTestData = JSON.stringify(privateTestData, null, 4)
  if (stringifiedTestData !== stringifiedPrivateTestData) {
    throw Error(`Expected testData to not change got:\n${stringifiedTestData}\nExpected${stringifiedPrivateTestData}`)
  }
}

export function regexpEquals(udr: unknown, r: RegExp): boolean {
  if (!regularExpression.is(udr)) {
    return false
  }
  const sortedUdrFlags = udr.flags.split(``).sort().join(``)
  const sortedRFlags = r.flags.split(``).sort().join(``)
  return udr.source === r.source && sortedRFlags === sortedUdrFlags
}

export function getUndefinedSymbolNames(result: AnalyzeResult): Set<string> {
  const names = [...result.undefinedSymbols].map(entry => entry.symbol)
  return new Set<string>(names)
}
