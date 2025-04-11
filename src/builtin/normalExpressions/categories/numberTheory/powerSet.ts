import type { Arr } from '../../../../interface'
import { assertArray } from '../../../../typeGuards/array'
import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'

function powerSet(set: Arr): Arr[] {
  const result: Arr[] = [[]]

  for (const value of set) {
    const newSubsets = result.map(subset => [...subset, value])
    result.push(...newSubsets)
  }

  return result
}

export const powerSetNormalExpressions: BuiltinNormalExpressions = {
  'n:power-set': {
    evaluate: ([set], sourceCodeInfo): Arr[] => {
      assertArray(set, sourceCodeInfo)
      return powerSet(set)
    },
    paramCount: 1,
  },
  'n:count-power-set': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      if (n >= 53) {
        // Number.MAX_SAFE_INTEGER is 2^53 - 1
        return Infinity
      }

      return 2 ** n
    },
    paramCount: 1,
  },
}
