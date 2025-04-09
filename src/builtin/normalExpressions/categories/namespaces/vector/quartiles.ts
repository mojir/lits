export function quartiles(vector: number[]): [number, number, number] {
  const sorted = [...vector].sort((a, b) => a - b)

  // Median calculation (Q2)
  const midIndex = sorted.length / 2
  let q2: number
  if (sorted.length % 2 === 0) {
    // Even length - average the two middle values
    q2 = (sorted[midIndex - 1]! + sorted[midIndex]!) / 2
  }
  else {
    // Odd length - take the middle value
    q2 = sorted[Math.floor(midIndex)]!
  }

  // Lower half for Q1
  const lowerHalf = sorted.slice(0, Math.floor(sorted.length / 2))
  // Upper half for Q3
  const upperHalf = sorted.slice(Math.ceil(sorted.length / 2))

  // Calculate Q1 and Q3 using the same median logic on the halves
  let q1: number, q3: number

  if (lowerHalf.length % 2 === 0) {
    const midLower = lowerHalf.length / 2
    q1 = (lowerHalf[midLower - 1]! + lowerHalf[midLower]!) / 2
  }
  else {
    q1 = lowerHalf[Math.floor(lowerHalf.length / 2)]!
  }

  if (upperHalf.length % 2 === 0) {
    const midUpper = upperHalf.length / 2
    q3 = (upperHalf[midUpper - 1]! + upperHalf[midUpper]!) / 2
  }
  else {
    q3 = upperHalf[Math.floor(upperHalf.length / 2)]!
  }

  return [q1, q2, q3]
}
