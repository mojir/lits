/**
 * Calculates Kendall's Tau-b rank correlation coefficient between two vectors
 * This implementation handles ties and uses an epsilon value for floating-point comparisons
 *
 * @param vectorA - First vector of numbers
 * @param vectorB - Second vector of numbers
 * @param epsilon - Threshold for considering two values as equal (default: 1e-10)
 * @returns The Kendall's Tau-b correlation coefficient
 */
export function kendallTau(vectorA: number[], vectorB: number[], epsilon: number = 1e-10): number {
  let concordant = 0
  let discordant = 0
  let tiesInA = 0
  let tiesInB = 0

  // Compare all pairs
  for (let i = 0; i < vectorA.length; i++) {
    for (let j = i + 1; j < vectorA.length; j++) {
      // Calculate differences
      const diffA = vectorA[i]! - vectorA[j]!
      const diffB = vectorB[i]! - vectorB[j]!

      // Check for ties using epsilon
      const isTieA = Math.abs(diffA) < epsilon
      const isTieB = Math.abs(diffB) < epsilon

      if (isTieA && isTieB) {
        // Tied in both vectors
        continue
      }
      else if (isTieA) {
        // Tied in vector A only
        tiesInA += 1
      }
      else if (isTieB) {
        // Tied in vector B only
        tiesInB += 1
      }
      else if (diffA * diffB > 0) {
        // Concordant pair
        concordant += 1
      }
      else {
        // Discordant pair
        discordant += 1
      }
    }
  }

  const n1 = concordant + discordant + tiesInA
  const n2 = concordant + discordant + tiesInB

  // Handle edge cases to avoid division by zero
  if (n1 === 0 || n2 === 0) {
    throw new Error('Not enough data to calculate Kendall\'s Tau')
  }

  // Kendall's Tau-b formula
  return (concordant - discordant) / Math.sqrt(n1 * n2)
}
