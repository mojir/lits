/* istanbul ignore file */

import { Type } from '../src/Lits/Lits'
import { combinations, getParamCombinations, getSampleValuesForType } from './testUtils'

describe(`testUtils`, () => {
  test(`combinations`, () => {
    expect(combinations([])).toEqual([])
    expect(combinations([`X`])).toEqual([[`X`]])
    expect(combinations([`X`, `Y`])).toEqual([
      [`X`, `Y`],
      [`Y`, `X`],
    ])
    expect(combinations([`X`, `Y`, `Z`])).toEqual([
      [`X`, `Y`, `Z`],
      [`X`, `Z`, `Y`],
      [`Y`, `X`, `Z`],
      [`Y`, `Z`, `X`],
      [`Z`, `X`, `Y`],
      [`Z`, `Y`, `X`],
    ])
    expect(combinations([`X`, `Y`, `Z`, `foo`]).length).toBe(24)
  })
  xtest(`getSampleValuesForType`, () => {
    expect(getSampleValuesForType(Type.array)).toEqual([`[]`, `[1 2 3]`])
  })

  test(`getParamCombinations`, () => {
    expect(
      getParamCombinations([
        [`A`, `B`],
        [`X`, `Y`],
        [`1`, `2`, `3`],
      ]),
    ).toEqual([
      [`A`, `X`, `1`],
      [`B`, `X`, `1`],
      [`A`, `Y`, `1`],
      [`B`, `Y`, `1`],
      [`A`, `X`, `2`],
      [`B`, `X`, `2`],
      [`A`, `Y`, `2`],
      [`B`, `Y`, `2`],
      [`A`, `X`, `3`],
      [`B`, `X`, `3`],
      [`A`, `Y`, `3`],
      [`B`, `Y`, `3`],
    ])
  })
})
