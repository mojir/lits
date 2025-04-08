import { isSquare } from './isSquare'

const epsilon = 1e-10
export function isIdentity(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }
  const n = matrix.length

  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (i === j) {
        if (Math.abs(matrix[i]![j]! - 1) > epsilon) {
          return false
        }
      }
      else {
        if (Math.abs(matrix[i]![j]!) > epsilon) {
          return false
        }
      }
    }
  }

  return true
}
