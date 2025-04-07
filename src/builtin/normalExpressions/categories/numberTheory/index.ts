import { LitsError } from '../../../../errors'
import type { Arr } from '../../../../interface'
import { assertVector } from '../../../../typeGuards/annotatedArrays'
import { assertArray } from '../../../../typeGuards/array'
import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { combinationsNormalExpressions } from './combinations'
import { derangementsNormalExpressions } from './derangements'
import { divisorsNormalExpressions, getDivisors, getProperDivisors } from './divisors'
import { factorialNormalExpressions, factorialOf } from './factorial'
import { partitionsNormalExpressions } from './partitions'
import { permutationsNormalExpressions } from './permutations'
import { powerSetNormalExpressions } from './powerSet'
import { primeFactors, primeFactorsNormalExpressions } from './primeFactors'
import { sequenceNormalExpressions } from './sequences'
import { perfectPower } from './sequences/perfectPower'

function gcd(a: number, b: number): number {
  while (b !== 0) {
    const temp = b
    b = a % b
    a = temp
  }
  return Math.abs(a)
}

function lcm(a: number, b: number): number {
  return Math.floor((a * b) / gcd(a, b))
}

function mobius(n: number): number {
  if (n === 1)
    return 1

  const factors = primeFactors(n)
  const uniqueFactors = new Set(factors)

  // If n has a repeated prime factor (not square-free)
  if (uniqueFactors.size !== factors.length)
    return 0

  // If square-free with even number of prime factors: return 1
  // If square-free with odd number of prime factors: return -1
  return factors.length % 2 === 0 ? 1 : -1
}

/**
 * Efficiently computes (base^exponent) % modulus using the square-and-multiply algorithm
 * Based on the pseudocode algorithm for modular exponentiation
 *
 * @param base - The base number
 * @param exponent - The exponent (must be non-negative)
 * @param modulus - The modulus (must be positive)
 * @returns The result of (base^exponent) % modulus
 */
function modExp(base: number, exponent: number, modulus: number): number {
  // Edge case: modulus is 1
  if (modulus === 1) {
    return 0
  }

  // Assert: (modulus - 1) * (modulus - 1) does not overflow base
  // This is a limitation of using regular JavaScript numbers instead of BigInt

  // Initialize result
  let result = 1

  // Apply modulo to base first
  base = base % modulus

  // Square and multiply algorithm
  while (exponent > 0) {
    // If current bit of exponent is 1, multiply result with current base
    if (exponent % 2 === 1) {
      result = (result * base) % modulus
    }

    // Shift exponent right (divide by 2)
    exponent = exponent >> 1

    // Square the base for next iteration
    base = (base * base) % modulus
  }

  return result
}

/**
 * Extended Euclidean Algorithm
 * Finds gcd(a,b) and coefficients x,y such that ax + by = gcd(a,b)
 */
function extendedGcd(a: number, b: number): [number, number, number] {
  if (b === 0) {
    return [a, 1, 0]
  }

  const [g, x, y] = extendedGcd(b, a % b)
  return [g, y, x - Math.floor(a / b) * y]
}

/**
 * Modular Multiplicative Inverse
 * Finds x such that (a * x) % m = 1
 */
function modInverse(a: number, m: number): number {
  const [g, x] = extendedGcd(a, m)

  if (g !== 1) {
    throw new Error(`Modular inverse does not exist (gcd(${a}, ${m}) = ${g})`)
  }

  return ((x % m) + m) % m // Ensure positive result
}

/**
 * Chinese Remainder Theorem
 * Solve system of congruences: x ≡ remainders[i] (mod moduli[i])
 * Returns the smallest positive integer that satisfies all congruences
 */
function chineseRemainder(remainders: number[], moduli: number[]): number {
  // Verify moduli are pairwise coprime
  for (let i = 0; i < moduli.length; i++) {
    for (let j = i + 1; j < moduli.length; j++) {
      const extGcd = extendedGcd(moduli[i]!, moduli[j]!)[0]
      if (extGcd !== 1) {
        throw new Error(`Moduli must be pairwise coprime, but gcd(${moduli[i]}, ${moduli[j]}) = ${extGcd}`)
      }
    }
  }

  // Calculate product of all moduli
  const product = moduli.reduce((acc, val) => acc * val, 1)

  let sum = 0

  for (let i = 0; i < remainders.length; i++) {
    const ai = remainders[i]!
    const ni = moduli[i]!
    const bi = product / ni

    // Find modular multiplicative inverse of bi modulo ni
    const inverse = modInverse(bi, ni)

    // Add contribution from this congruence
    sum = (sum + ai * bi * inverse) % product
  }

  return sum
}

export const combinatoricalNormalExpression: BuiltinNormalExpressions = {
  'n:coprime?': {
    evaluate: ([a, b], sourceCodeInfo): boolean => {
      assertNumber(a, sourceCodeInfo, { integer: true })
      assertNumber(b, sourceCodeInfo, { integer: true })
      return gcd(a, b) === 1
    },
    paramCount: 2,
  },
  'n:divisible-by?': {
    evaluate: ([value, divisor], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true })
      assertNumber(divisor, sourceCodeInfo, { integer: true })
      if (divisor === 0)
        return false
      return value % divisor === 0
    },
    paramCount: 2,
  },
  'n:gcd': {
    evaluate: ([a, b], sourceCodeInfo): number => {
      assertNumber(a, sourceCodeInfo)
      assertNumber(b, sourceCodeInfo)
      return gcd(a, b)
    },
    paramCount: 2,
  },
  'n:lcm': {
    evaluate: ([a, b], sourceCodeInfo): number => {
      assertNumber(a, sourceCodeInfo)
      assertNumber(b, sourceCodeInfo)
      return lcm(a, b)
    },
    paramCount: 2,
  },

  'n:multinomial': {
    evaluate: ([...args], sourceCodeInfo): number => {
      assertVector(args, sourceCodeInfo)
      const sum = args.reduce((acc: number, curr) => {
        assertNumber(curr, sourceCodeInfo, { integer: true, nonNegative: true })
        return acc + curr
      }, 0)
      return factorialOf(sum) / args.reduce((acc, curr) => acc * factorialOf(curr), 1)
    },
    paramCount: { min: 1 },
  },
  'n:amicable?': {
    evaluate: ([a, b], sourceCodeInfo): boolean => {
      assertNumber(a, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(b, sourceCodeInfo, { integer: true, positive: true })
      const sumA = getProperDivisors(a).reduce((acc, curr) => acc + curr, 0)
      const sumB = getProperDivisors(b).reduce((acc, curr) => acc + curr, 0)
      return sumA === b && sumB === a && a !== b
    },
    paramCount: 2,
  },
  'n:euler-totient': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      let result = n
      for (let p = 2; p * p <= n; p += 1) {
        if (n % p === 0) {
          while (n % p === 0)
            n /= p
          result -= result / p
        }
      }
      if (n > 1)
        result -= result / n
      return result
    },
    paramCount: 1,
  },
  'n:mobius': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      if (n === 1)
        return 1

      const factors = primeFactors(n)
      const uniqueFactors = new Set(factors)

      // If n has a repeated prime factor (not square-free)
      if (uniqueFactors.size !== factors.length)
        return 0

      // If square-free with even number of prime factors: return 1
      // If square-free with odd number of prime factors: return -1
      return factors.length % 2 === 0 ? 1 : -1
    },
    paramCount: 1,
    aliases: ['n:möbius'],
  },
  'n:mertens': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      if (n === 1)
        return 1
      let result = 0
      for (let i = 1; i <= n; i++) {
        const mobiusValue = mobius(i)
        result += mobiusValue// * Math.floor(n / i)
      }
      return result
    },
    paramCount: 1,
    aliases: ['n:mertens'],
  },
  'n:sigma': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return getDivisors(n).reduce((acc, curr) => acc + curr, 0)
    },
    paramCount: 1,
  },
  'n:carmichael-lambda': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      if (n === 1) {
        return 1
      }

      // Count occurrences of each prime factor
      const primes = primeFactors(n)
      const factorMap = new Map<number, number>()

      for (const prime of primes) {
        factorMap.set(prime, (factorMap.get(prime) || 0) + 1)
      }

      const lambdaValues: number[] = []

      // Calculate lambda for each prime power
      for (const [p, k] of factorMap.entries()) {
        if (p === 2) {
          if (k === 1) {
            lambdaValues.push(1) // λ(2) = 1
          }
          else if (k === 2) {
            lambdaValues.push(2) // λ(4) = 2
          }
          else {
            lambdaValues.push(2 ** (k - 2)) // λ(2^k) = 2^(k-2) for k >= 3
          }
        }
        else {
          // For odd prime powers p^k: λ(p^k) = (p-1)*p^(k-1)
          lambdaValues.push((p - 1) * p ** (k - 1))
        }
      }

      // Find LCM of all lambda values
      return lambdaValues.reduce((acc, val) => lcm(acc, val), 1)
    },
    paramCount: 1,
  },
  'n:cartesian-product': {
    evaluate: (params, sourceCodeInfo): Arr[] => {
      params.forEach((set) => {
        assertArray(set, sourceCodeInfo)
      })
      const sets = params as Arr[]
      return sets.reduce((acc: Arr[], set) => {
        const result: Arr[] = []
        acc.forEach((arr) => {
          set.forEach((value) => {
            result.push([...arr, value])
          })
        })
        return result
      }, [[]])
    },
    paramCount: { min: 1 },
  },
  'n:perfect-power': {
    evaluate: ([n], sourceCodeInfo): [number, number] | null => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const result = perfectPower(n)
      return result || null
    },
    paramCount: 1,
  },
  'n:mod-exp': {
    evaluate: ([base, exponent, modulus], sourceCodeInfo): number => {
      assertNumber(base, sourceCodeInfo, { finite: true })
      assertNumber(exponent, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(modulus, sourceCodeInfo, { integer: true, positive: true })

      return modExp(base, exponent, modulus)
    },
    paramCount: 3,
  },
  'n:mod-inv': {
    evaluate: ([a, m], sourceCodeInfo): number => {
      assertNumber(a, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(m, sourceCodeInfo, { integer: true, positive: true })

      return modInverse(a, m)
    },
    paramCount: 2,
  },
  'n:extended-gcd': {
    evaluate: ([a, b], sourceCodeInfo): [number, number, number] => {
      assertNumber(a, sourceCodeInfo, { integer: true })
      assertNumber(b, sourceCodeInfo, { integer: true })

      return extendedGcd(a, b)
    },
    paramCount: 2,
  },
  'n:chinese-remainder': {
    evaluate: ([remainders, moduli], sourceCodeInfo): number => {
      assertVector(remainders, sourceCodeInfo)
      assertVector(moduli, sourceCodeInfo)
      if (remainders.length !== moduli.length) {
        throw new Error('Remainders and moduli must have the same length.')
      }
      try {
        return chineseRemainder(remainders, moduli)
      }
      catch (error) {
        throw new LitsError((error as Error).message, sourceCodeInfo)
      }
    },
    paramCount: 2,
  },
  'n:stirling-first': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(k, sourceCodeInfo, { integer: true, positive: true, lte: n })

      // Create a table to store results
      const dp: number[][] = Array.from({ length: n + 1 }, () => Array<number>(k + 1).fill(0))

      // Base case
      dp[0]![0] = 1

      // Fill the table using the recurrence relation
      for (let i = 1; i <= n; i++) {
        for (let j = 1; j <= Math.min(i, k); j++) {
          dp[i]![j] = dp[i - 1]![j - 1]! + (i - 1) * dp[i - 1]![j]!
        }
      }

      return dp[n]![k]!
    },
    paramCount: 2,
  },
  'n:stirling-second': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(k, sourceCodeInfo, { integer: true, positive: true, lte: n })
      if (k === 1)
        return 1 // Only one way to put n objects into one subset
      if (k === n)
        return 1 // Only one way to put n objects into n subsets (one object per subset)

      // Create a 2D array for memoization
      const dp: number[][] = Array.from({ length: n + 1 }, () => Array<number>(k + 1).fill(0))

      // Initialize base cases
      dp[0]![0] = 1

      // Fill the dp table using the recurrence relation:
      // S(n,k) = k * S(n-1,k) + S(n-1,k-1)
      for (let i = 1; i <= n; i++) {
        for (let j = 1; j <= Math.min(i, k); j++) {
          dp[i]![j] = j * dp[i - 1]![j]! + dp[i - 1]![j - 1]!
        }
      }

      return dp[n]![k]!
    },
    paramCount: 2,
  },
}

addSequences(sequenceNormalExpressions)
addNormalExpressions(factorialNormalExpressions)
addNormalExpressions(divisorsNormalExpressions)
addNormalExpressions(combinationsNormalExpressions)
addNormalExpressions(permutationsNormalExpressions)
addNormalExpressions(partitionsNormalExpressions)
addNormalExpressions(primeFactorsNormalExpressions)
addNormalExpressions(derangementsNormalExpressions)
addNormalExpressions(powerSetNormalExpressions)

function addSequences(sequences: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(sequences)) {
    /* v8 ignore next 3 */
    if (combinatoricalNormalExpression[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    combinatoricalNormalExpression[key] = value
  }
}

function addNormalExpressions(normalExpressions: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(normalExpressions)) {
    /* v8 ignore next 3 */
    if (combinatoricalNormalExpression[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    combinatoricalNormalExpression[key] = value
  }
}
