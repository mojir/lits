import { LitsError } from '../../../../errors'
import type { Arr } from '../../../../interface'
import { assertArray } from '../../../../typeGuards/array'
import { asNumber, assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../interface'
import { assertVector } from '../vector'
import { sequenceNormalExpression } from './sequences'

// Multinomial coefficient calculation - You have binomial coefficients (presumably through count-combinations), but multinomial coefficients are their natural extension and widely used.
// Gray code generation - This is a fundamental combinatorial structure used in many algorithms and applications, particularly in coding theory and computer science.
// Partition by specific constraints - Your partition functions are great, but adding specialized versions for common constraints (like partitions into distinct parts or odd parts) would enhance their utility.

const perfectNumbers = [6, 28, 496, 8128, 33550336, 8589869056, 137438691328]
const mersennePrimes = [3, 7, 31, 127, 2047, 8191, 131071, 524287, 2147483647]
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
 * Checks if a number is a perfect square.
 *
 * @param {number} n - The number to check
 * @return {boolean} - True if n is a perfect square, false otherwise
 */
function isPerfectSquare(n: number): boolean {
  const sqrt = Math.sqrt(n)
  return Math.floor(sqrt) === sqrt
}

/**
 * Checks if a number is in the Fibonacci sequence.
 *
 * A number is in the Fibonacci sequence if and only if one of
 * (5n² + 4) or (5n² - 4) is a perfect square.
 * This is based on Binet's formula.
 *
 * @param {number} n - The number to check
 * @return {boolean} - True if n is a Fibonacci number, false otherwise
 */
function isFibonacciNumber(n: number): boolean {
  // Handle edge cases
  if (n < 0)
    return false
  if (n === 0 || n === 1)
    return true

  // Check if 5n² + 4 or 5n² - 4 is a perfect square
  const test1 = 5 * n * n + 4
  const test2 = 5 * n * n - 4

  return isPerfectSquare(test1) || isPerfectSquare(test2)
}

/**
 * Checks if a number is in the Lucas sequence.
 *
 * The Lucas sequence starts with L(0) = 2, L(1) = 1 and each subsequent number
 * is the sum of the two previous ones: L(n) = L(n-1) + L(n-2).
 *
 * @param {number} n - The number to check
 * @return {boolean} - True if n is a Lucas number, false otherwise
 */
function isLucasNumber(n: number): boolean {
  // Handle edge cases
  if (n <= 0)
    return false

  // Direct check for small numbers
  if (n === 1 || n === 2)
    return true

  // Use the iterative approach for all numbers
  let a = 2 // L(0)
  let b = 1 // L(1)

  while (b < n) {
    // Calculate the next Lucas number
    const temp = a + b
    a = b
    b = temp

    // Check if we've found our number
    if (b === n) {
      return true
    }
  }

  // If we've exceeded n without finding it, it's not a Lucas number
  return false
}

/**
 * Checks if a number is part of the Tribonacci sequence.
 * The Tribonacci sequence starts with 0, 0, 1 and each subsequent
 * number is the sum of the three preceding ones.
 *
 * @param num - The number to check
 * @returns True if the number is in the Tribonacci sequence, false otherwise
 */
function isTribonacciNumber(num: number): boolean {
  // Handle edge cases
  if (num < 0)
    return false
  if (num === 0 || num === 1)
    return true

  // Special case for 2, which is in the sequence
  if (num === 2)
    return true

  // Initialize the first three numbers of the sequence
  let a = 0
  let b = 0
  let c = 1

  // Generate the sequence until we reach or exceed the input number
  while (c < num) {
    const next = a + b + c
    a = b
    b = c
    c = next
  }

  // If c equals the input number, it's in the sequence
  return c === num
}

/**
 * Checks if a number is a triangular number.
 * A triangular number is a number that can be represented as the sum of consecutive integers from 1 to n.
 * The formula for the nth triangular number is n(n+1)/2.
 *
 * @param num - The number to check
 * @returns True if the number is triangular, false otherwise
 */
function isTriangular(num: number): boolean {
  // Negative numbers and non-integers cannot be triangular
  if (num < 0 || !Number.isInteger(num)) {
    return false
  }

  // Edge case: 0 is considered triangular (0th triangular number)
  if (num === 0) {
    return true
  }

  // Using the formula for triangular numbers: n(n+1)/2 = num
  // This can be rewritten as n² + n - 2*num = 0
  // Using the quadratic formula: n = (-1 + √(1 + 8*num))/2
  // If n is a positive integer, then the number is triangular

  const discriminant = 1 + 8 * num
  const sqrtDiscriminant = Math.sqrt(discriminant)

  // Check if the result is an integer
  // Due to floating point precision, we check if the value is very close to an integer
  const n = (-1 + sqrtDiscriminant) / 2
  return Math.abs(Math.round(n) - n) < 1e-10
}

/* Checks if a number is a hexagonal number.
* A hexagonal number is a figurate number that can be represented as a hexagonal arrangement of points.
* The formula for the nth hexagonal number is n(2n-1).
*
* @param num - The number to check
* @returns True if the number is hexagonal, false otherwise
*/
function isHexagonal(num: number): boolean {
  // Negative numbers and non-integers cannot be hexagonal
  if (num < 0 || !Number.isInteger(num)) {
    return false
  }

  // Edge case: 0 is considered the 0th hexagonal number
  if (num === 0) {
    return true
  }

  // Using the formula for hexagonal numbers: H_n = n(2n-1)
  // We can rewrite this as 2n² - n - num = 0
  // Using the quadratic formula: n = (1 + √(1 + 8*num))/4
  // If n is a positive integer, then the number is hexagonal

  const discriminant = 1 + 8 * num
  const sqrtDiscriminant = Math.sqrt(discriminant)

  // Check if the result is an integer
  // Due to floating point precision, we check if the value is very close to an integer
  const n = (1 + sqrtDiscriminant) / 4
  return Math.abs(Math.round(n) - n) < 1e-10
}

/**
 * Checks if a number is a pentagonal number.
 * A pentagonal number is a figurate number that can be represented as a pentagonal arrangement of points.
 * The formula for the nth pentagonal number is n(3n-1)/2.
 *
 * The first few pentagonal numbers are:
 * 0, 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, 176, 210, 247, 287, 330, ...
 *
 * @param num - The number to check
 * @returns True if the number is pentagonal, false otherwise
 */
function isPentagonal(num: number): boolean {
  // Negative numbers and non-integers cannot be pentagonal
  if (num < 0 || !Number.isInteger(num)) {
    return false
  }

  // Edge case: 0 is considered the 0th pentagonal number
  if (num === 0) {
    return true
  }

  // Using the formula for pentagonal numbers: P_n = n(3n-1)/2
  // We can rewrite this as 3n² - n - 2*num = 0
  // Using the quadratic formula: n = (1 + √(1 + 24*num))/6
  // If n is a positive integer, then the number is pentagonal

  const discriminant = 1 + 24 * num
  const sqrtDiscriminant = Math.sqrt(discriminant)

  // Check if the result is an integer
  // Due to floating point precision, we check if the value is very close to an integer
  const n = (1 + sqrtDiscriminant) / 6
  return Math.abs(Math.round(n) - n) < 1e-10
}

/**
 * Checks if a number is a pentatope number.
 * A pentatope number (also known as a 4-simplex number or tetrahedral pyramidal number)
 * is a 4-dimensional figurate number that represents the number of points in a 4-simplex.
 *
 * The formula for the nth pentatope number is: n(n+1)(n+2)(n+3)/24
 *
 * The first few pentatope numbers are:
 * 0, 1, 5, 15, 35, 70, 126, 210, 330, 495, 715, 1001, 1365, 1820, 2380, ...
 *
 * @param num - The number to check
 * @returns True if the number is a pentatope number, false otherwise
 */
function isPentatope(num: number): boolean {
  // Negative numbers and non-integers cannot be pentatope numbers
  if (num < 0 || !Number.isInteger(num)) {
    return false
  }

  // Edge case: 0 is considered the 0th pentatope number
  if (num === 0) {
    return true
  }

  // For a more efficient approach, we can use an approximate solution
  // and then check nearby values

  // A rough approximation for n given P(n) = num is n ≈ (24*num)^(1/4)
  const estimatedN = (24 * num) ** 0.25

  // Check a small range around the estimate (typically only need to check 1-2 values)
  // We'll check n ± 2 to be safe
  const lowerBound = Math.max(1, Math.floor(estimatedN) - 2)
  const upperBound = Math.ceil(estimatedN) + 2

  for (let n = lowerBound; n <= upperBound; n++) {
    const pentatope = (n * (n + 1) * (n + 2) * (n + 3)) / 24
    if (pentatope === num) {
      return true
    }
    // Since pentatope numbers grow monotonically, we can exit early
    // if we've exceeded the target number
    if (pentatope > num) {
      return false
    }
  }

  return false
}

/**
 * Checks if a number is a hexacube number.
 * A hexacube number is a perfect 6th power (n^6 for some integer n).
 *
 * The first few hexacube numbers are:
 * 0, 1, 64, 729, 4096, 15625, 46656, 117649, 262144, 531441, 1000000, ...
 *
 * @param num - The number to check
 * @returns True if the number is a hexacube, false otherwise
 */
function isHexacube(num: number): boolean {
  // Non-integers cannot be hexacubes
  if (!Number.isInteger(num)) {
    return false
  }

  // Edge case: 0^6 = 0
  if (num === 0) {
    return true
  }

  // A key insight: n^6 is always positive for any real n
  // So negative numbers cannot be hexacubes
  if (num < 0) {
    return false
  }

  // For numbers within the safe integer range, use a direct approach
  if (num <= Number.MAX_SAFE_INTEGER) {
    // Calculate the 6th root
    const sixthRoot = num ** (1 / 6)

    // Round to the nearest integer
    const roundedRoot = Math.round(sixthRoot)

    // Calculate the 6th power of the rounded root
    const calculatedHexacube = roundedRoot ** 6

    // Check for exact equality
    return calculatedHexacube === num
  }
  // For extremely large numbers, use a safer approach that accounts for precision limits
  else {
    // Get a more accurate estimate of the 6th root using logarithms
    // log(n^6) = 6*log(n), so n = 10^(log(num)/6)
    const estimatedRoot = 10 ** (Math.log10(num) / 6)
    const roundedRoot = Math.round(estimatedRoot)

    // For very large numbers, instead of computing roundedRoot^6 directly
    // (which might lose precision), compare logs
    const originalLog = Math.log10(num)
    const hexacubeLog = 6 * Math.log10(roundedRoot)

    // If the logs are very close, it's likely a hexacube
    return Math.abs(originalLog - hexacubeLog) < 1e-10
  }
}

/**
 * Checks if a number is a factorial number.
 * A factorial number is a number that equals n! for some integer n >= 0.
 *
 * The first few factorial numbers are:
 * 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, ...
 * (Note: 0! = 1 and 1! = 1)
 *
 * @param num - The number to check
 * @returns True if the number is a factorial, false otherwise
 */
function isFactorial(num: number): boolean {
  // Non-integers cannot be factorials
  if (!Number.isInteger(num)) {
    return false
  }

  // Negative numbers cannot be factorials
  if (num < 0) {
    return false
  }

  // Special case: 0 is not a factorial (0! = 1, but 0 itself is not)
  if (num === 0) {
    return false
  }

  // Special case: 1 is both 0! and 1!
  if (num === 1) {
    return true
  }

  // Pre-calculate factorials for efficient lookup (up to a reasonable limit)
  const factorials = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600, 6227020800, 87178291200, 1307674368000, 20922789888000, 355687428096000, 6402373705728000, 121645100408832000, 2432902008176640000]

  // Direct lookup for known factorials
  if (factorials.includes(num)) {
    return true
  }

  // For numbers smaller than largest pre-computed factorial but not in the list
  if (num < factorials[factorials.length - 1]!) {
    return false
  }

  // For numbers larger than the pre-computed list, use the division approach
  // This is more expensive but necessary for large numbers
  let divisor = 2
  let remainder = num

  while (remainder > 1) {
    if (remainder % divisor !== 0) {
      return false
    }
    remainder /= divisor
    divisor++

    // Safety check to avoid precision issues
    if (divisor > 25 || remainder > Number.MAX_SAFE_INTEGER / divisor) {
      // If we get here, either:
      // 1. The number is too large to safely check
      // 2. We've exceeded our divisor limit (unlikely to be a factorial)
      return false
    }
  }

  return remainder === 1
}

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

function getLuckyNumbers(length: number): number[] {
  const numbers: number[] = []
  for (let i = 1; i <= length * 10; i += 2) { // Generate more than needed
    numbers.push(i)
  }

  // Apply the sieve process
  let idx = 1 // Start from the second element (index 1, which is 3)

  while (idx < numbers.length) {
    const step = numbers[idx]! // Current lucky number

    // Remove every step-th number from the list
    // Count from the beginning each time, and account for changing indices
    let j = 0
    let count = 0
    while (j < numbers.length) {
      count++
      if (count % step === 0) {
        numbers.splice(j, 1)
      }
      else {
        j++ // Only increment if we didn't remove an element
      }
    }

    // Get the new index of the next element (which may have changed)
    idx++
  }

  // Return the first 'length' lucky numbers
  return numbers.slice(0, length)
}

function isHappyNumber(n: number): boolean {
  // A happy number is defined by the following process:
  // 1. Starting with any positive integer, replace the number by the sum of the squares of its digits
  // 2. Repeat until either:
  //    - The number equals 1 (in which case it's a happy number)
  //    - It enters a cycle that doesn't include 1 (in which case it's not a happy number)

  if (n <= 0)
    return false

  // Use a set to detect cycles
  const seen = new Set()

  // Continue until we either reach 1 or detect a cycle
  while (n !== 1 && !seen.has(n)) {
    seen.add(n)
    n = getSumOfSquaredDigits(n)
  }

  // If we reached 1, it's a happy number
  return n === 1
}

function getSumOfSquaredDigits(n: number): number {
  let sum = 0

  while (n > 0) {
    const digit = n % 10
    sum += digit * digit
    n = Math.floor(n / 10)
  }

  return sum
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

  let result = 1
  for (let i = 2; i <= n; i++)
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

export function isPrime(num: number): boolean {
  if (num <= 1) {
    return false
  }
  if (num <= 3) {
    return true
  }
  if (num % 2 === 0 || num % 3 === 0) {
    return false
  }

  for (let i = 5; i * i <= num; i += 6) {
    if (num % i === 0 || num % (i + 2) === 0) {
      return false
    }
  }
  return true
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
  'c:prime?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true })
      return isPrime(value)
    },
    paramCount: 1,
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
  'c:perfect?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true, positive: true })
      return perfectNumbers.includes(value)
    },
    paramCount: 1,
  },
  'c:mersenne?': {
    evaluate: ([value], sourceCodeInfo): boolean => {
      assertNumber(value, sourceCodeInfo, { integer: true, positive: true })
      return mersennePrimes.includes(value)
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
  'c:lucky-number-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      return getLuckyNumbers(length)
    },
    paramCount: 1,
  },
  'c:lucky-number-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return getLuckyNumbers(n).at(-1)!
    },
    paramCount: 1,
  },
  'c:lucky-number?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return getLuckyNumbers(n * 5).includes(n)
    },
    paramCount: 1,
  },
  'c:happy-number-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const happyNumbers: number[] = []
      for (let i = 1; happyNumbers.length < length; i++) {
        let n = i
        const seen = new Set<number>()
        while (n !== 1 && !seen.has(n)) {
          seen.add(n)
          n = String(n)
            .split('')
            .reduce((sum, digit) => sum + Number(digit) ** 2, 0)
        }
        if (n === 1)
          happyNumbers.push(i)
      }
      return happyNumbers
    },
    paramCount: 1,
  },
  'c:happy-number-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })

      let happyCount = 0
      let currentNumber = 1

      while (happyCount < n) {
        let num = currentNumber
        const seen = new Set<number>()
        while (num !== 1 && !seen.has(num)) {
          seen.add(num)
          num = String(num)
            .split('')
            .reduce((sum, digit) => sum + Number(digit) ** 2, 0)
        }
        if (num === 1)
          happyCount++
        currentNumber++
      }
      return currentNumber - 1
    },
    paramCount: 1,
  },
  'c:happy-number?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isHappyNumber(n)
    },
    paramCount: 1,
  },
  'c:juggler-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const juggler = [1]
      for (let n = 1; n < length; n += 1) {
        juggler[n] = Math.floor(Math.sqrt(juggler[n - 1]!))
      }
      return juggler
    },
    paramCount: 1,
  },
  'c:juggler-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      let juggler = 1
      for (let i = 1; i < n; i += 1) {
        juggler = Math.floor(Math.sqrt(juggler))
      }
      return juggler
    },
    paramCount: 1,
  },
  'c:juggler?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n === 1 || (n > 1 && n % Math.sqrt(n) === 0)
    },
    paramCount: 1,
  },
  'c:partition-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const result = [1] as number[] // First partition number is always 1

      for (let m = 1; m < length; m++) {
        result.push(calcPartitions(m))
      }

      return result
    },
    paramCount: 1,
  },
  'c:partition-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return calcPartitions(n)
    },
    paramCount: 1,
  },
  'c:partition?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n === 1 || (n > 1 && n % Math.sqrt(n) === 0)
    },
    paramCount: 1,
  },
  'c:perfect-seq': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const length = asNumber(params[0] ?? perfectNumbers.length, sourceCodeInfo, { integer: true, positive: true, lte: perfectNumbers.length })
      return perfectNumbers.slice(0, length)
    },
    paramCount: { max: 1 },
  },
  'c:perfect-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: perfectNumbers.length })
      return perfectNumbers[n - 1]!
    },
    paramCount: 1,
  },
  'c:mersenne-seq': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const length = asNumber(params[0] ?? mersennePrimes.length, sourceCodeInfo, { integer: true, positive: true, lte: mersennePrimes.length })
      return mersennePrimes.slice(0, length)
    },
    paramCount: { max: 1 },
  },
  'c:mersenne-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: mersennePrimes.length })
      return mersennePrimes[n - 1]!
    },
    paramCount: 1,
  },
  'c:arithmetic-seq': {
    evaluate: ([start, step, length], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return Array.from({ length }, (_, i) => start + i * step)
    },
    paramCount: 3,
  },
  'c:arithmetic-nth': {
    evaluate: ([start, step, n], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return start + (n - 1) * step
    },
    paramCount: 3,
  },
  'c:geometric-seq': {
    evaluate: ([start, ratio, length], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return Array.from({ length }, (_, i) => start * ratio ** i)
    },
    paramCount: 3,
  },
  'c:geometric-nth': {
    evaluate: ([start, ratio, n], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return start * ratio ** (n - 1)
    },
    paramCount: 3,
  },
  'c:fibonacci-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const fib = [0, 1]
      for (let i = 2; i < length; i += 1) {
        fib[i] = fib[i - 1]! + fib[i - 2]!
      }
      return fib.slice(0, length)
    },
    paramCount: 1,
  },
  'c:fibonacci-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })

      if (n === 1)
        return 0
      if (n === 2)
        return 1

      let a = 0
      let b = 1
      for (let i = 3; i <= n; i += 1) {
        const temp = a + b
        a = b
        b = temp
      }
      return b
    },
    paramCount: 1,
  },
  'c:fibbonacci?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isFibonacciNumber(n)
    },
    paramCount: 1,
  },
  'c:lucas-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const lucas = [2, 1]
      for (let i = 2; i < length; i += 1) {
        lucas[i] = lucas[i - 1]! + lucas[i - 2]!
      }
      return lucas.slice(0, length)
    },
    paramCount: 1,
  },
  'c:lucas-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })

      if (n === 1)
        return 2
      if (n === 2)
        return 1

      let a = 2
      let b = 1
      for (let i = 3; i <= n; i += 1) {
        const temp = a + b
        a = b
        b = temp
      }
      return b
    },
    paramCount: 1,
  },
  'c:lucas?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isLucasNumber(n)
    },
    paramCount: 1,
  },
  'c:tribonacci-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const tribonacci = [0, 1, 1]
      for (let i = 3; i < length; i += 1) {
        tribonacci[i] = tribonacci[i - 1]! + tribonacci[i - 2]! + tribonacci[i - 3]!
      }
      return tribonacci.slice(0, length)
    },
    paramCount: 1,
  },
  'c:tribonacci-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })

      if (n === 1)
        return 0
      if (n === 2)
        return 1
      if (n === 3)
        return 1

      let a = 0
      let b = 1
      let c = 1
      for (let i = 4; i <= n; i += 1) {
        const temp = a + b + c
        a = b
        b = c
        c = temp
      }
      return c
    },
    paramCount: 1,
  },
  'c:tribonacci?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isTribonacciNumber(n)
    },
    paramCount: 1,
  },
  'c:prime-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const primes = []
      let num = 2
      while (primes.length < length) {
        if (isPrime(num)) {
          primes.push(num)
        }
        num += 1
      }
      return primes
    },
    paramCount: 1,
  },
  'c:prime-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })

      let count = 0
      let num = 2
      while (count < n) {
        if (isPrime(num)) {
          count += 1
        }
        num += 1
      }
      return num - 1
    },
    paramCount: 1,
  },
  'c:triangular-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const triangular = []
      for (let i = 0; i < length; i += 1) {
        triangular[i] = (i * (i + 1)) / 2
      }
      return triangular
    },
    paramCount: 1,
  },
  'c:triangular-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return (n * (n + 1)) / 2
    },
    paramCount: 1,
  },
  'c:triangular?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isTriangular(n)
    },
    paramCount: 1,
  },
  'c:square-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const square = []
      for (let i = 0; i < length; i += 1) {
        square[i] = i ** 2
      }
      return square
    },
    paramCount: 1,
  },
  'c:square-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n ** 2
    },
    paramCount: 1,
  },
  'c:hexagonal-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const hexagonal = []
      for (let i = 0; i < length; i += 1) {
        hexagonal[i] = i * (2 * i - 1)
      }
      return hexagonal
    },
    paramCount: 1,
  },
  'c:hexagonal-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n * (2 * n - 1)
    },
    paramCount: 1,
  },
  'c:hexagonal?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isHexagonal(n)
    },
    paramCount: 1,
  },
  'c:pentagonal-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const pentagonal = []
      for (let i = 0; i < length; i += 1) {
        pentagonal[i] = (3 * i ** 2 - i) / 2
      }
      return pentagonal
    },
    paramCount: 1,
  },
  'c:pentagonal-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return (3 * n ** 2 - n) / 2
    },
    paramCount: 1,
  },
  'c:pentagonal?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isPentagonal(n)
    },
    paramCount: 1,
  },
  'c:cubic-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const cubic = []
      for (let i = 0; i < length; i += 1) {
        cubic[i] = i ** 3
      }
      return cubic
    },
    paramCount: 1,
  },
  'c:cubic-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return n ** 3
    },
    paramCount: 1,
  },
  'c:pentatope-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const pentatope = []
      for (let i = 0; i < length; i += 1) {
        pentatope[i] = (i * (i + 1) * (i + 2)) / 6
      }
      return pentatope
    },
    paramCount: 1,
  },
  'c:pentatope-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return (n * (n + 1) * (n + 2)) / 6
    },
    paramCount: 1,
  },
  'c:pentatope?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isPentatope(n)
    },
    paramCount: 1,
  },
  'c:hexacube-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      const hexacube = []
      for (let i = 0; i < length; i += 1) {
        hexacube[i] = (i * (i + 1) * (i + 2) * (i + 3)) / 24
      }
      return hexacube
    },
    paramCount: 1,
  },
  'c:hexacube-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return (n * (n + 1) * (n + 2) * (n + 3)) / 24
    },
    paramCount: 1,
  },
  'c:hexacube?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isHexacube(n)
    },
    paramCount: 1,
  },
  'c:polygonal-seq': {
    evaluate: ([length, sides], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(sides, sourceCodeInfo, { integer: true, positive: true })

      const polygonal = []
      for (let i = 0; i < length; i += 1) {
        polygonal[i] = (sides * i * (i - 1)) / 2
      }
      return polygonal
    },
    paramCount: 2,
  },
  'c:polygonal-nth': {
    evaluate: ([n, sides], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(sides, sourceCodeInfo, { integer: true, positive: true })
      return (sides * n * (n - 1)) / 2
    },
    paramCount: 2,
  },
  'c:factorial-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const factorial = []
      for (let i = 0; i < length; i += 1) {
        factorial[i] = factorialOf(i)
      }
      return factorial
    },
    paramCount: 1,
  },
  'c:factorial-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return factorialOf(n)
    },
    paramCount: 1,
  },
  'c:factorial?': {
    evaluate: ([n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return isFactorial(n)
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
