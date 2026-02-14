import { approxZero } from '../../../../utils'
import { gaussJordanElimination } from './gaussJordanElimination'

/**
 * Solves a system of linear equations Ax = b
 *
 * @param A - The coefficient matrix
 * @param b - The constant vector
 * @returns The solution vector x, or null if no unique solution exists
 */
export function solve(A: number[][], b: number[]): number[] | null {
  const n = A.length

  // Create augmented matrix [A|b]
  const augmented = A.map((row, i) => [...row, b[i]!])

  // Convert to row echelon form using your existing function
  const [echelon] = gaussJordanElimination(augmented)

  // Check if the system has a unique solution
  for (let i = 0; i < n; i += 1) {
    if (approxZero(echelon[i]![i]!)) {
      return null // No unique solution
    }
  }

  // Back substitution
  const x = Array.from({ length: n }, () => 0)
  for (let i = n - 1; i >= 0; i--) {
    let sum = 0
    for (let j = i + 1; j < n; j++) {
      sum += echelon[i]![j]! * x[j]!
    }
    x[i] = (echelon[i]![n]! - sum) / echelon[i]![i]!
  }

  return x
}
