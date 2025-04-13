import { matrixMultiply } from './matrixMultiply'
import { identity } from './identity'
import { inverse } from './inverse'

/**
 * Compute the nth power of a square matrix
 *
 * @param A - A square matrix
 * @param n - The power to raise the matrix to
 * @returns The matrix A raised to power n
 */
export function pow(A: number[][], n: number): number[][] {
  const rows = A.length
  let result = identity(rows)
  // Handle special cases
  if (n === 0) {
    return result
  }

  if (n < 0) {
    const inverseA = inverse(A)
    if (!inverseA) {
      throw new Error('Matrix is not invertible')
    }
    A = inverseA
    n = -n
  }

  // Binary exponentiation method (faster than naive repeated multiplication)
  let power = A.map(row => [...row]) // Create a deep copy of A

  while (n > 0) {
    if (n % 2 === 1) {
      result = matrixMultiply(result, power)
    }
    power = matrixMultiply(power, power)
    n = Math.floor(n / 2)
  }

  return result
}
