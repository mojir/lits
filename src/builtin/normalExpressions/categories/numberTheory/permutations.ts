import type { Arr } from '../../../../interface'
import { assertArray } from '../../../../typeGuards/array'
import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { factorialOf } from './factorial'

/**
 * Generates all possible permutations of a collection.
 * @param collection The input collection to generate permutations from
 * @returns An array of arrays, where each inner array is a permutation of the input collection
 */
function permutations<T>(collection: T[]): T[][] {
  // Base case: empty array has one permutation - itself
  if (collection.length === 0) {
    return [[]]
  }

  const result: T[][] = []

  // For each element in the array
  for (let i = 0; i < collection.length; i++) {
    // Extract the current element
    const current = collection[i]!

    // Create a new array without the current element
    const remainingElements = [...collection.slice(0, i), ...collection.slice(i + 1)]

    // Generate all permutations of the remaining elements
    const subPermutations = permutations(remainingElements)

    // Add the current element to the beginning of each sub-permutation
    for (const subPerm of subPermutations) {
      result.push([current, ...subPerm])
    }
  }

  return result
}

export const permutationsNormalExpressions: BuiltinNormalExpressions = {
  'n:permutations': {
    evaluate: ([set], sourceCodeInfo): Arr[] => {
      assertArray(set, sourceCodeInfo)
      return permutations(set)
    },
    paramCount: 1,
  },
  'n:count-permutations': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      assertNumber(k, sourceCodeInfo, { integer: true, nonNegative: true, lte: n })
      return factorialOf(n) / factorialOf(n - k)
    },
    paramCount: 2,
  },
}
