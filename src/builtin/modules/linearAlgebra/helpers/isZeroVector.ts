import { approxZero } from '../../../../utils'

export function isZeroVector(vector: number[]): boolean {
  return vector.every(component => approxZero(component))
}
