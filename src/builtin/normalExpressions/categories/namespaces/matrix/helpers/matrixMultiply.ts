/**
 * Performs cache-optimized matrix multiplication.
 * @param A The first input matrix (m x n)
 * @param B The second input matrix (n x p)
 * @returns The result matrix C (m x p) where C = A × B
 */
export function matrixMultiply(A: number[][], B: number[][]): number[][] {
  // Check if matrices can be multiplied
  if (A.length === 0 || B.length === 0 || A[0]!.length !== B.length) {
    throw new Error('Matrix dimensions do not match for multiplication')
  }

  const m = A.length // Number of rows in A
  const n = A[0]!.length // Number of columns in A / Number of rows in B
  const p = B[0]!.length // Number of columns in B

  // Initialize result matrix C with zeros
  const C: number[][] = (Array(m).fill(0) as number[]).map(() => Array(p).fill(0) as number[])

  // Perform multiplication with cache-optimized loop order (i-k-j)
  for (let i = 0; i < m; i++) {
    for (let k = 0; k < n; k++) {
      const aik = A[i]![k]! // Cache this value to avoid repeated lookups
      for (let j = 0; j < p; j++) {
        C[i]![j]! += aik * B[k]![j]!
      }
    }
  }

  return C
}
