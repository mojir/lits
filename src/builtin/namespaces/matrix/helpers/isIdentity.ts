import { approxEqual, approxZero } from '../../../../utils'
import { isSquare } from './isSquare'

export function isIdentity(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }
  const n = matrix.length

  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (i === j) {
        if (!approxEqual(matrix[i]![j]!, 1)) {
          return false
        }
      }
      else {
        if (!approxZero(matrix[i]![j]!)) {
          return false
        }
      }
    }
  }

  return true
}
