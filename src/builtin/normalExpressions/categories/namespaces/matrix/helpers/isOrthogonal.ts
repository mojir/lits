import { transpose } from '../../../../../../namespaces/grid/transpose'
import { matrixMultiply } from './matrixMultiply'
import { isIdentity } from './isIdentity'
import { isSquare } from './isSquare'

export function isOrthogonal(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }

  // Calculate matrix transpose
  const transposed = transpose(matrix)

  // Check if matrix * transpose = Identity
  const product = matrixMultiply(matrix, transposed)

  // Check if the product is an identity matrix
  return isIdentity(product)
}
