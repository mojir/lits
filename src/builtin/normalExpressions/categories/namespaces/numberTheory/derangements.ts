import type { Arr } from '../../../../../interface'
import { assertArray } from '../../../../../typeGuards/array'
import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'

function getAllDerangements(arr: Arr): Arr[] {
  const n = arr.length
  const result: Arr[] = []
  const used = Array.from({ length: n }, () => false)
  const temp: Arr = Array.from({ length: n })

  function generateDerangements(pos: number): void {
    if (pos === n) {
      result.push([...temp])
      return
    }

    for (let i = 0; i < n; i++) {
      // Skip if element is already used or would be in its original position
      if (used[i] || i === pos) {
        continue
      }

      used[i] = true
      temp[pos] = arr[i]!
      generateDerangements(pos + 1)
      used[i] = false
    }
  }

  generateDerangements(0)
  return result
}

function countDerangements(n: number): number {
  if (n === 0)
    return 1
  if (n === 1)
    return 0

  let a = 1 // !0
  let b = 0 // !1
  let result = 0

  for (let i = 2; i <= n; i++) {
    result = (i - 1) * (a + b)
    a = b
    b = result
  }

  return result
}

export const derangementsNormalExpressions: BuiltinNormalExpressions = {
  'nth:derangements': {
    evaluate: ([set], sourceCodeInfo): Arr => {
      assertArray(set, sourceCodeInfo)
      return getAllDerangements(set)
    },
    paramCount: 1,
  },
  'nth:count-derangements': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return countDerangements(n)
    },
    paramCount: 1,
  },
}
