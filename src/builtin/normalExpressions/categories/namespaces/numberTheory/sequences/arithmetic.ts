import { assertLitsFunction } from '../../../../../../typeGuards/litsFunction'
import { assertNumber } from '../../../../../../typeGuards/number'
import type { SequenceNormalExpressions } from '.'

/**
 * Checks if a number is a member of an arithmetic sequence.
 * @param start The first term of the sequence
 * @param step The common difference between terms
 * @param n The number to check
 * @returns true if the number is in the sequence, false otherwise
 */
function isInArithmeticSequence(
  start: number,
  step: number,
  n: number,
): boolean {
  // Special case: If step is 0, n must equal start
  if (step === 0) {
    return Math.abs(n - start) < 1e-12
  }

  // Check if n is a valid arithmetic sequence term
  const position = (n - start) / step

  // Position must be non-negative
  if (position < 0) {
    return false
  }

  // For tiny steps, do a direct calculation instead of using the position
  // This helps avoid floating point precision issues
  if (Math.abs(step) < 1e-6) {
    // Find the closest position (rounding to nearest integer)
    const roundedPosition = Math.round(position)
    const calculatedValue = start + step * roundedPosition

    // Direct comparison with tiny values should use absolute difference
    return Math.abs(calculatedValue - n) < 1e-12
  }

  // For normal cases, check if position is very close to an integer
  const roundedPosition = Math.round(position)
  if (Math.abs(roundedPosition - position) > 1e-12) {
    return false
  }

  // Double check by calculating the value at that position
  const calculatedValue = start + step * roundedPosition

  // For values very close to zero, use absolute difference
  if (Math.abs(n) < 1e-10 || Math.abs(calculatedValue) < 1e-10) {
    return Math.abs(calculatedValue - n) < 1e-12
  }

  // Otherwise use relative difference for better precision with large numbers
  const relativeDifference = Math.abs(calculatedValue - n)
    / Math.max(Math.abs(n), Math.abs(calculatedValue))
  return relativeDifference < 1e-12
}

export const arithmeticNormalExpressions: SequenceNormalExpressions<'arithmetic'> = {
  'nth:arithmetic-seq': {
    evaluate: ([start, step, length], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return Array.from({ length }, (_, i) => start + i * step)
    },
    paramCount: 3,
  },
  'nth:arithmetic-take-while': {
    evaluate: ([start, step, fn], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertLitsFunction(fn, sourceCodeInfo)

      const arithmetic = []
      for (let i = 0; ; i += 1) {
        const value = start + i * step
        if (!executeFunction(fn, [value, i], contextStack, sourceCodeInfo)) {
          break
        }
        arithmetic[i] = value
      }
      return arithmetic
    },
    paramCount: 3,
  },
  'nth:arithmetic-nth': {
    evaluate: ([start, step, n], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return start + (n - 1) * step
    },
    paramCount: 3,
  },
  'nth:arithmetic?': {
    evaluate: ([start, step, n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo)
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })

      return isInArithmeticSequence(start, step, n)
    },
    paramCount: 3,
  },
}
