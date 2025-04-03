import { LitsError } from '../../../../errors'
import type { Arr } from '../../../../interface'
import { assertArray } from '../../../../typeGuards/array'
import { asNumber, assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { assertVector } from '../vector'
import { isPerfectSquare } from './isPerfectSquare'
import { sequenceNormalExpression } from './sequences'
import { factorialNumbers } from './sequences/factorial'
import { isPrime } from './sequences/prime'

const armstrongNumbers = [
  // 1 digit (all single digits are narcissistic)
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,

  // 3 digits
  153,
  370,
  371,
  407,

  // 4 digits
  1634,
  8208,
  9474,

  // 5 digits
  54748,
  92727,
  93084,

  // 6 digits
  548834,

  // 7 digits
  1741725,
  4210818,
  9800817,
  9926315,

  // 8 digits
  24678050,
  24678051,
  88593477,

  // 9 digits
  146511208,
  472335975,
  534494836,
  912985153,

  // 10 digits
  4679307774,

  // 11 digits
  32164049650,
  32164049651,
  40028394225,
  42678290603,
  44708635679,
  49388550606,
  82693916578,
  94204591914,

  // 14 digits
  28116440335967,

  // 16 digits
  4338281769391370,
  4338281769391371,
]

/**
 * Checks if a number is a perfect power and returns the base and exponent if it is.
 * A perfect power is a number that can be expressed as an integer power of another integer.
 *
 * @param n - The number to check
 * @returns [base, exponent] if n is a perfect power, null otherwise
 */
function perfectPower(n: number): [number, number] | null {
  // Handle edge cases
  if (n < 2) {
    if (n === 1) {
      // 1 is 1^k for any k, we return [1, 2] as the simplest representation
      return [1, 2]
    }
    if (n === 0) {
      // 0 is 0^k for any k > 0, we return [0, 2] as the simplest representation
      return [0, 2]
    }
    return null // Negative numbers are not handled in this implementation
  }

  // For each possible exponent k, try to find base b such that b^k = n
  const maxK = Math.floor(Math.log2(n)) + 1

  for (let k = 2; k <= maxK; k++) {
    // Calculate the potential base as n^(1/k)
    const b = n ** (1 / k)
    const roundedB = Math.round(b)

    // Check if roundedB^k is equal to n (within a small epsilon to account for floating point errors)
    const epsilon = 1e-10
    if (Math.abs(roundedB ** k - n) < epsilon) {
      return [roundedB, k]
    }
  }

  return null // Not a perfect power
}

/**
 * Generates all possible combinations of a specified size from a collection.
 * @param collection The input collection to generate combinations from
 * @param size The size of each combination
 * @returns An array of arrays, where each inner array is a combination of the specified size
 */
function combinations<T>(collection: T[], size: number): T[][] {
  // Return empty array if invalid inputs
  if (size <= 0 || size > collection.length) {
    return []
  }

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

function derangement(n: number): number {
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

function powerSet(set: Arr): Arr[] {
  const result: Arr[] = [[]]

  for (const value of set) {
    const newSubsets = result.map(subset => [...subset, value])
    result.push(...newSubsets)
  }

  return result
}


function calcPartitions(n: number): number {
  const partition: number[] = Array.from({ length: n + 1 }, () => 0)
  partition[0] = 1
  for (let i = 1; i <= n; i += 1) {
    for (let j = i; j <= n; j += 1) {
      partition[j]! += partition[j - i]!
    }
  }
  return partition[n]!
}

function gcd(a: number, b: number): number {
  while (b !== 0) {
    const temp = b
    b = a % b
    a = temp
  }
  return Math.abs(a)
}

function factorsOf(n: number): number[] {
  const factors: number[] = []
  for (let i = 1; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      factors.push(i)
      if (i !== n / i) {
        factors.push(n / i)
      }
    }
  }
  return factors
}
export function factorialOf(n: number): number {
  if (n < 0)
    throw new Error('Factorial is not defined for negative numbers')

  if (n === 0 || n === 1)
    return 1

  if (n <= 18) {
    return factorialNumbers[n]!
  }
  let result = factorialNumbers[18]!
  for (let i = 19; i <= n; i++)
    result *= i

  return result
}

export function binomialCoefficient(n: number, k: number): number {
  if (k < 0 || k > n)
    return 0

  if (k === 0 || k === n)
    return 1

  let result = 1
  for (let i = 0; i < k; i++)
    result *= (n - i) / (i + 1)

  return result
}

function modularExponentiation(base: number, exponent: number, mod: number): number {
  if (mod === 1)
    return 0 // Any number mod 1 is 0
  let result = 1
  base = base % mod

  while (exponent > 0) {
    if (exponent % 2 === 1) {
      result = (result * base) % mod
    }
    exponent = Math.floor(exponent / 2)
    base = (base * base) % mod
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
  if (remainders.length !== moduli.length) {
    throw new Error('Number of remainders must equal number of moduli')
  }

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
  'c:factors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true })
      return factorsOf(number)
    },
    paramCount: 1,
  },
  'c:prime-factors': {
    evaluate: ([number], sourceCodeInfo): number[] => {
      assertNumber(number, sourceCodeInfo, { finite: true })
      const factors: number[] = []
      for (let i = 2; i <= number; i += 1) {
        if (number % i === 0 && isPrime(i)) {
          factors.push(i)
        }
      }
      return factors
    },
    paramCount: 1,
  },
  'c:divisible-by?': {
    evaluate: ([value, divisor], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo)
      assertNumber(divisor, sourceCodeInfo)
      if (divisor === 0)
        return false
      return value % divisor === 0
    },
    paramCount: 2,
  },
  'c:gcd': {
    evaluate: ([a, b], sourceCodeInfo): number => {
      assertNumber(a, sourceCodeInfo)
      assertNumber(b, sourceCodeInfo)
      return gcd(a, b)
    },
    paramCount: 2,
  },
  'c:lcm': {
    evaluate: ([a, b], sourceCodeInfo): number => {
      assertNumber(a, sourceCodeInfo)
      assertNumber(b, sourceCodeInfo)
      return Math.abs((a * b) / gcd(a, b))
    },
    paramCount: 2,
  },
  'c:factorial': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      if (n > 170) {
        // Factorial of numbers greater than 170 exceeds the maximum safe integer in JavaScript
        throw new LitsError('Factorial is too large to compute safely', sourceCodeInfo)
      }
      return factorialOf(n)
    },
    aliases: ['c:!'],
    paramCount: 1,
  },
  'c:generate-combinations': {
    evaluate: ([set, n], sourceCodeInfo): Arr[] => {
      assertArray(set, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true, lte: set.length })
      return combinations(set, n)
    },
    paramCount: 1,
  },
  'c:generate-permutations': {
    evaluate: ([set], sourceCodeInfo): Arr[] => {
      assertArray(set, sourceCodeInfo)
      return permutations(set)
    },
    paramCount: 1,
  },
  'c:count-permutations': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      assertNumber(k, sourceCodeInfo, { integer: true, nonNegative: true, lte: n })
      return factorialOf(n) / factorialOf(n - k)
    },
    paramCount: 2,
  },
  'c:count-combinations': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      assertNumber(k, sourceCodeInfo, { integer: true, nonNegative: true, lte: n })
      return binomialCoefficient(n, k)
    },
    aliases: ['c:binomial-coefficient'],
    paramCount: 2,
  },
  'c:multinomial': {
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
  'c:composite?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true })
      return !isPrime(value) && value > 1
    },
    paramCount: 1,
  },
  'c:abundant?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true, positive: true })
      const factors = factorsOf(value - 1)
      const sum = factors.reduce((acc, curr) => acc + curr, 0)
      return sum > value
    },
    paramCount: 1,
  },
  'c:deficient?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true, positive: true })
      const factors = factorsOf(value - 1)
      const sum = factors.reduce((acc, curr) => acc + curr, 0)
      return sum < value
    },
    paramCount: 1,
  },
  'c:amicable?': {
    evaluate: ([a, b], sourceCodeInfo): boolean => {
      assertNumber(a, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(b, sourceCodeInfo, { integer: true, positive: true })
      const sumA = factorsOf(a).reduce((acc, curr) => acc + curr, 0)
      const sumB = factorsOf(b).reduce((acc, curr) => acc + curr, 0)
      return sumA === b && sumB === a && a !== b
    },
    paramCount: 2,
  },
  'c:narcissistic?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true, positive: true })
      return armstrongNumbers.includes(value)
    },
    aliases: ['c:armstrong?'],
    paramCount: 1,
  },
  'c-narcissistic-seq': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const length = asNumber(params[0] ?? armstrongNumbers.length, sourceCodeInfo, { integer: true, positive: true, lte: armstrongNumbers.length })
      return armstrongNumbers.slice(0, length)
    },
    aliases: ['c:armstrong-seq'],
    paramCount: { max: 1 },
  },
  'c-narcissistic-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: armstrongNumbers.length })
      return armstrongNumbers[n - 1]!
    },
    aliases: ['c:armstrong-nth'],
    paramCount: 1,
  },
  'c:euler-totient': {
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
  'c:mobius': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      if (n === 1)
        return 1
      const factors = factorsOf(n)
      const uniqueFactors = new Set(factors)
      return uniqueFactors.size === factors.length ? -1 : 0
    },
    paramCount: 1,
    aliases: ['c:möbius'],
  },
  'c:sigma': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const factors = factorsOf(n)
      return factors.reduce((acc, curr) => acc + curr, 0)
    },
    paramCount: 1,
  },
  'c:divisor-count': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const factors = factorsOf(n)
      return factors.length
    },
    paramCount: 1,
  },
  'c:carmichael-lambda': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      if (n === 1)
        return 1
      let result = n
      const factors = factorsOf(n)
      for (const factor of factors) {
        result *= (factor - 1) / factor
      }
      return result
    },
    paramCount: 1,
  },
  'c:derangement': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return derangement(n)
    },
    paramCount: 1,
  },
  'c:power-set': {
    evaluate: ([set], sourceCodeInfo): Arr[] => {
      assertArray(set, sourceCodeInfo)
      return powerSet(set)
    },
    paramCount: 1,
  },
  'c:cartesian-product': {
    evaluate: (params, sourceCodeInfo): Arr[] => {
      params.forEach(set => assertArray(set, sourceCodeInfo))
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
  'c:perfect-power?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return perfectPower(n) !== null
    },
    paramCount: 1,
  },
  'c:perfect-power': {
    evaluate: ([n], sourceCodeInfo): [number, number] | null => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return perfectPower(n)
    },
    paramCount: 1,
  },
  'c:perfect-power-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const perfectPowers: number[] = [] as unknown as number[]
      for (let i = 1; perfectPowers.length < length; i++) {
        if (perfectPower(i)) {
          perfectPowers.push(i)
        }
      }
      return perfectPowers
    },
    paramCount: 1,
  },
  'c:perfect-power-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      let count = 0
      let current = 1

      while (count < n) {
        if (perfectPower(current)) {
          count++
        }
        current++
      }

      return current - 1
    },
    paramCount: 1,
  },
  'c:collatz-seq': {
    evaluate: (params, sourceCodeInfo): number[] => {
      let x = asNumber(params[0], sourceCodeInfo, { integer: true, positive: true })

      const collatz = [x]
      while (x !== 1) {
        if (x % 2 === 0) {
          x /= 2
        }
        else {
          x = 3 * x + 1
        }
        collatz.push(x)
      }
      return collatz
    },
    paramCount: 1,
  },
  'c:mod-exp': {
    evaluate: ([base, exponent, modulus], sourceCodeInfo): number => {
      assertNumber(base, sourceCodeInfo, { finite: true })
      assertNumber(exponent, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(modulus, sourceCodeInfo, { integer: true, positive: true })

      return modularExponentiation(base, exponent, modulus)
    },
    paramCount: 3,
  },
  'c:mod-inv': {
    evaluate: ([a, m], sourceCodeInfo): number => {
      assertNumber(a, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(m, sourceCodeInfo, { integer: true, positive: true })

      return modInverse(a, m)
    },
    paramCount: 2,
  },
  'c:extended-gcd': {
    evaluate: ([a, b], sourceCodeInfo): number[] => {
      assertNumber(a, sourceCodeInfo, { integer: true })
      assertNumber(b, sourceCodeInfo, { integer: true })

      return extendedGcd(a, b)
    },
    paramCount: 2,
  },
  'c:chinese-remainder': {
    evaluate: ([remainders, moduli], sourceCodeInfo): number => {
      assertVector(remainders, sourceCodeInfo)
      assertVector(moduli, sourceCodeInfo)
      if (remainders.length !== moduli.length) {
        throw new Error('Remainders and moduli must have the same length.')
      }

      return chineseRemainder(remainders, moduli)
    },
    paramCount: 2,
  },
  'c:perfect-square?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isPerfectSquare(n)
    },
    paramCount: 1,
  },
  'c:perfect-square-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const perfectSquares = []
      for (let i = 0; perfectSquares.length < length; i++) {
        perfectSquares.push(i * i)
      }
      return perfectSquares
    },
    paramCount: 1,
  },
  'c:perfect-square-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n * n
    },
    paramCount: 1,
  },
  'c:perfect-cube?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const cbrt = Math.cbrt(n)
      return Math.floor(cbrt) === cbrt
    },
    paramCount: 1,
  },
  'c:perfect-cube-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const perfectCubes = []
      for (let i = 0; perfectCubes.length < length; i++) {
        perfectCubes.push(i * i * i)
      }
      return perfectCubes
    },
    paramCount: 1,
  },
  'c:perfect-cube-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n * n * n
    },
    paramCount: 1,
  },
  'c:palindrome?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true, nonNegative: true })
      const strValue = value.toString()
      return strValue === strValue.split('').reverse().join('')
    },
    paramCount: 1,
  },
  'c:stirling-first-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const stirlingFirst: number[][] = []
      for (let n = 0; n < length; n += 1) {
        stirlingFirst[n] = []
        for (let k = 0; k <= n; k += 1) {
          if (k === 0 || k === n)
            stirlingFirst[n]![k] = 1
          else
            stirlingFirst[n]![k] = (n - 1) * (stirlingFirst[n - 1]![k]! + stirlingFirst[n - 1]![k - 1]!)
        }
      }
      return stirlingFirst[length - 1] as number[]
    },
    paramCount: 1,
  },
  'c:stirling-first-nth': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(k, sourceCodeInfo, { integer: true, positive: true, lte: n })
      if (k === 0 || k === n)
        return 1
      return (n - 1) * (calcPartitions(n - 1) + calcPartitions(n - k - 1))
    },
    paramCount: 2,
  },
  'c:stirling-second-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const stirlingSecond: number[][] = []
      for (let n = 0; n < length; n += 1) {
        stirlingSecond[n] = []
        for (let k = 0; k <= n; k += 1) {
          if (k === 0 || k === n)
            stirlingSecond[n]![k] = 1
          else
            stirlingSecond[n]![k] = k * stirlingSecond[n - 1]![k]! + stirlingSecond[n - 1]![k - 1]!
        }
      }
      return stirlingSecond[length - 1] as number[]
    },
    paramCount: 1,
  },
  'c:stirling-second-nth': {
    evaluate: ([n, k], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(k, sourceCodeInfo, { integer: true, positive: true, lte: n })
      if (k === 0 || k === n)
        return 1
      return k * calcPartitions(n - 1) + calcPartitions(n - k - 1)
    },
    paramCount: 2,
  },
  'c:bernoulli-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const bernoulli = [1]
      for (let n = 1; n < length; n += 1) {
        let sum = 0
        for (let k = 0; k < n; k += 1) {
          sum += binomialCoefficient(n + 1, k) * bernoulli[k]!
        }
        bernoulli[n] = -sum / (n + 1)
      }
      return bernoulli
    },
    paramCount: 1,
  },
  'c:bernoulli-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      if (n === 0)
        return 1
      let sum = 0
      for (let k = 0; k < n; k += 1) {
        sum += binomialCoefficient(n + 1, k) * calcPartitions(k)
      }
      return -sum / (n + 1)
    },
    paramCount: 1,
  },
  'c:brenoulli?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n % 2 === 0 && n !== 2
    },
    paramCount: 1,
  },
  'c:sylvester-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const sylvester = [2]
      for (let n = 1; n < length; n += 1) {
        sylvester[n] = sylvester[n - 1]! + Math.sqrt(sylvester[n - 1]!)
      }
      return sylvester
    },
    paramCount: 1,
  },
  'c:sylvester-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      let sylvester = 2
      for (let i = 1; i < n; i += 1) {
        sylvester += Math.sqrt(sylvester)
      }
      return sylvester
    },
    paramCount: 1,
  },
  'c:sylvester?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n === 2 || (n > 2 && n % Math.sqrt(n) === 0)
    },
    paramCount: 1,
  },
  'c:golomb-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const golomb = [0]
      for (let n = 1; n < length; n += 1) {
        golomb[n] = 1 + golomb[n - golomb[golomb[n - 1]!]!]!
      }
      return golomb
    },
    paramCount: 1,
  },
  'c:golomb-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const golomb = [0]
      for (let i = 1; i <= n; i += 1) {
        golomb[i] = 1 + golomb[i - golomb[golomb[i - 1]!]!]!
      }
      return golomb[n]!
    },
    paramCount: 1,
  },
  'c:golomb?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n === 1 || (n > 1 && n % Math.sqrt(n) === 0)
    },
    paramCount: 1,
  },
  'c:juggler-seq': {
    evaluate: ([start], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { integer: true, positive: true })

      let next = start
      const juggler = [next]
      let index = 0
      while (next > 1) {
        next = next % 2 === 0
          ? Math.floor(Math.sqrt(juggler[index]!))
          : Math.floor(juggler[index]! * Math.sqrt(juggler[index]!))
        index += 1
        juggler.push(next)
      }
      return juggler
    },
    paramCount: 1,
  },
}

addSequences(sequenceNormalExpression)

function addSequences(sequences: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(sequences)) {
    if (combinatoricalNormalExpression[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    combinatoricalNormalExpression[key] = value
  }
}
