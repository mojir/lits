import { transpose } from '../../grid/transpose'
import { isIdentity } from './isIdentity'
import { isSquare } from './isSquare'
import { multiply } from './multiply'

export function isOrthogonal(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }

  // Calculate matrix transpose
  const transposed = transpose(matrix)

  // Check if matrix * transpose = Identity
  const product = multiply(matrix, transposed)
  if (!product) {
    return false
  }

  // Check if the product is an identity matrix
  return isIdentity(product)
}
