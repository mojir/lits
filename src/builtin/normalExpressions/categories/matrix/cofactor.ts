import { determinant } from './determinant'
import { minor } from './minor'

export function cofactor(matrix: number[][]): number[][] {
  const n = matrix.length
  const cofactors: number[][] = []

  // Create a new matrix to store cofactors
  for (let i = 0; i < n; i++) {
    cofactors[i] = []
    for (let j = 0; j < n; j++) {
      // Get the minor by removing row i and column j
      const min = minor(matrix, i, j)
      const sign = (-1) ** (i + j)
      cofactors[i]![j] = sign * determinant(min)
    }
  }

  return cofactors
}
