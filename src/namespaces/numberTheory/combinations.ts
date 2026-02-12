import type { Arr } from '../../interface'
import { assertArray } from '../../typeGuards/array'
import { assertNumber } from '../../typeGuards/number'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../../builtin/interface'
import { binomialCoefficient } from './binomialCefficient'

/**
 * Generates all possible combinations of a specified size from a collection.
 * @param collection The input collection to generate combinations from
 * @param size The size of each combination
 * @returns An array of arrays, where each inner array is a combination of the specified size
 */
function combinations<T>(collection: T[], size: number): T[][] {
  // Base case: if size is 1, return each element as its own combination
  if (size === 1) {
    return collection.map(item => [item])
  }

  const result: T[][] = []

  // Recursive approach to build combinations
  for (let i = 0; i <= collection.length - size; i++) {
    // Take the current element
    const current = collection[i]!

    // Get all combinations of size-1 from the rest of the elements
    const subCombinations = combinations(
      collection.slice(i + 1),
      size - 1,
    )

    // Add the current element to each sub-combination
    for (const subComb of subCombinations) {
      result.push([current, ...subComb])
    }
  }

  return result
}

export const combinationsNormalExpressions: BuiltinNormalExpressions = {
  'combinations': {
    evaluate: ([set, n], sourceCodeInfo): Arr[] => {
      assertArray(set, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true, lte: set.length })
      if (n === 0)
        return [[]]
      return combinations(set, n)
    },
    arity: toFixedArity(2),
  },
  'count-combinations': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      assertNumber(k, sourceCodeInfo, { integer: true, nonNegative: true, lte: n })
      return binomialCoefficient(n, k)
    },
    aliases: ['binomial'],
    arity: toFixedArity(2),
  },
}
