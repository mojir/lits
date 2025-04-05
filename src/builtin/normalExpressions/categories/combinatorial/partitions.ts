import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { partitionNumbers } from './sequences/partition'

function partitions(n: number): number[][] {
  // Base cases
  if (n <= 0)
    return []
  if (n === 1)
    return [[1]]

  const result: number[][] = []

  // Helper function to generate partitions recursively
  function generatePartitions(remaining: number, max: number, current: number[]): void {
    if (remaining === 0) {
      result.push([...current])
      return
    }

    // Try all possible numbers from 1 up to max
    for (let i = Math.min(max, remaining); i >= 1; i--) {
      current.push(i)
      generatePartitions(remaining - i, i, current)
      current.pop()
    }
  }

  generatePartitions(n, n, [])
  return result
}

export const partitionsNormalExpressions: BuiltinNormalExpressions = {
  'c:partitions': {
    evaluate: ([n], sourceCodeInfo): number[][] => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      return partitions(n)
    },
    paramCount: 1,
  },
  'c:count-partitions': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      if (n === 0)
        return 1

      if (n > partitionNumbers.length) {
        throw new Error(`n is too large. The maximum value is ${partitionNumbers.length - 1}.`)
      }

      return partitionNumbers[n - 1]!
    },
    paramCount: 1,
  },
}
