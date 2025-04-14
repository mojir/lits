export function calcFractionalRanks(vector: number[]): number[] {
  // Create array of indices and sort by values
  const indices = [...vector.keys()].sort((a, b) => vector[a]! - vector[b]!)

  // Create ranks array (same size as input vector)
  const ranks: number[] = Array.from<number>({ length: vector.length }).fill(0)

  // Assign ranks, handling ties properly
  let currentRank = 1
  let i = 0
  while (i < indices.length) {
    const value = vector[indices[i]!]
    let j = i

    // Find all indices with the same value
    while (j < indices.length && vector[indices[j]!] === value) {
      j++
    }

    // Calculate average rank for ties
    const averageRank = currentRank + (j - i - 1) / 2

    // Assign average rank to all tied elements
    for (let k = i; k < j; k++) {
      ranks[indices[k]!] = averageRank
    }

    // Update current rank and index
    currentRank += j - i
    i = j
  }

  return ranks
}
