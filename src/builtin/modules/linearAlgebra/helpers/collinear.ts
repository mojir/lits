import { approxEqual, approxZero } from '../../../../utils'

export function areVectorsCollinear(vectorA: number[], vectorB: number[]): boolean {
  // Check if either vector is zero
  const isZeroVector = (vec: number[]): boolean =>
    vec.every(component => approxZero(component))

  if (isZeroVector(vectorA) || isZeroVector(vectorB)) {
    return true // Zero vector is technically collinear to all vectors
  }

  // Find the first non-zero component in vectorA
  let index = 0
  while (index < vectorA.length && approxZero(vectorA[index]!)) {
    index++
  }

  // Calculate the scale factor
  const ratio = vectorB[index]! / vectorA[index]!

  // Check if all other components maintain the same ratio
  for (let i = 0; i < vectorA.length; i++) {
    // Skip components where both vectors have (near) zero values
    if (approxZero(vectorA[i]!) && approxZero(vectorB[i]!))
      continue

    // If vectorA component is near zero but vectorB is not, vectors are not collinear
    if (approxZero(vectorA[i]!))
      return false

    // Check if the ratio is consistent
    if (!approxEqual(vectorB[i]! / vectorA[i]!, ratio)) {
      return false
    }
  }

  return true
}

export function areVectorsParallel(vectorA: number[], vectorB: number[]): boolean {
  if (!areVectorsCollinear(vectorA, vectorB)) {
    return false
  }

  // Then verify they point in the same direction
  // Find first non-zero component in both vectors
  for (let i = 0; i < vectorA.length; i++) {
    if (!approxZero(vectorA[i]!) && !approxZero(vectorB[i]!)) {
      return Math.sign(vectorA[i]!) === Math.sign(vectorB[i]!)
    }
  }

  // If we get here, one of the vectors must be zero
  return true
}
