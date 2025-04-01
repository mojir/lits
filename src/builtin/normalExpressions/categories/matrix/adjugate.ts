import { determinant } from './determinant'
import { minor } from './minor'

export function adjugate(matrix: number[][]): number[][] {
  const n = matrix.length
  const adj: number[][] = []
  for (let i = 0; i < n; i++) {
    adj[i] = []
    for (let j = 0; j < n; j++) {
      const min = minor(matrix, j, i)
      const sign = (-1) ** (i + j)
      const cofactor = sign * determinant(min)
      adj[i]![j] = cofactor
    }
  }
  return adj
}
