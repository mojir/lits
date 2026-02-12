import { assertNumber } from '../../typeGuards/number'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../../builtin/interface'

/**
 * Returns the prime factorization of a number as an array of its prime factors.
 * For example, factors(12) returns [2, 2, 3].
 * Special case: factors(1) returns an empty array [].
 *
 * @param n - A positive integer to factorize
 * @returns An array of prime factors in ascending order
 */
export function primeFactors(n: number): number[] {
  // Special case: 1 has no prime factors
  if (n === 1) {
    return []
  }

  const factors: number[] = []
  let divisor = 2

  // Find factors by trial division
  while (n > 1) {
    // If divisor divides n evenly
    if (n % divisor === 0) {
      // Add divisor to the factors list
      factors.push(divisor)
      // Divide n by the found factor
      n /= divisor
    }
    else {
      // Move to the next potential divisor
      divisor++
    }
  }

  return factors
}

export const primeFactorsNormalExpressions: BuiltinNormalExpressions = {
  'prime-factors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return primeFactors(number)
    },
    arity: toFixedArity(1),
  },
  'distinct-prime-factors': {
    evaluate: ([n], sourceCodeInfo): number[] => {
      assertNumber(n, sourceCodeInfo, { finite: true, integer: true, positive: true })
      const factors = primeFactors(n)
      const distinctFactors = new Set(factors)
      return Array.from(distinctFactors)
    },
    arity: toFixedArity(1),
  },
  'count-prime-factors': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return primeFactors(n).length
    },
    arity: toFixedArity(1),
  },
  'count-distinct-prime-factors': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { finite: true, integer: true, positive: true })
      const factors = primeFactors(n)
      const distinctFactors = new Set(factors)
      return distinctFactors.size
    },
    arity: toFixedArity(1),
  },
}
