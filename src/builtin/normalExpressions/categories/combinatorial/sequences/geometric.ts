import { assertLitsFunction } from '../../../../../typeGuards/litsFunction'
import { assertNumber } from '../../../../../typeGuards/number'
import type { SequenceNormalExpressions } from '.'

/**
 * Checks if a number is a member of a geometric sequence.
 * @param initialTerm The first term of the sequence (a)
 * @param ratio The common ratio of the sequence (r)
 * @param number The number to check
 * @returns true if the number is in the sequence, false otherwise
 */
function isInGeometricSequence(
  initialTerm: number,
  ratio: number,
  number: number,
): boolean {
  // Handle special cases
  if (number === 0 && initialTerm === 0)
    return true
  if (initialTerm === 0)
    return number === 0
  if (ratio === 1)
    return number === initialTerm
  if (ratio === 0)
    return number === 0 || number === initialTerm

  // For negative ratios, we need special handling
  if (ratio < 0) {
    // Calculate log using absolute values
    const logResult = Math.log(Math.abs(number / initialTerm)) / Math.log(Math.abs(ratio))

    // Check if logResult is very close to an integer
    const roundedLogResult = Math.round(logResult)
    if (Math.abs(roundedLogResult - logResult) > 1e-10 || logResult < 0) {
      return false
    }

    // For negative ratios, alternating terms have alternating signs
    // Check if sign matches what we expect based on the power
    const expectedSign = roundedLogResult % 2 === 0
      ? Math.sign(initialTerm)
      : -Math.sign(initialTerm)

    return Math.sign(number) === expectedSign
  }

  // For positive ratios

  // Quick check based on sequence direction
  // If ratio > 1, number should be >= initialTerm to be in the sequence
  // If 0 < ratio < 1, number should be <= initialTerm to be in the sequence
  if ((ratio > 1 && number < initialTerm)
    || (ratio < 1 && number > initialTerm)) {
    return false
  }

  // Calculate n in: number = initialTerm * (ratio^n)
  const logResult = Math.log(number / initialTerm) / Math.log(ratio)

  // Check if logResult is very close to an integer
  const roundedLogResult = Math.round(logResult)
  if (Math.abs(roundedLogResult - logResult) > 1e-10 || logResult < 0) {
    return false
  }

  // Verify calculated value matches the number exactly (within floating point precision)
  // This is important to avoid false positives due to floating point arithmetic
  const calculatedValue = initialTerm * ratio ** roundedLogResult
  const relativeDifference = Math.abs(calculatedValue - number)
    / Math.max(Math.abs(number), Math.abs(calculatedValue))

  return relativeDifference < 1e-10
}

export const geometricNormalExpressions: SequenceNormalExpressions<'geometric'> = {
  'c:geometric-seq': {
    evaluate: ([start, ratio, length], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return Array.from({ length }, (_, i) => start * ratio ** i)
    },
    paramCount: 3,
  },
  'c:geometric-take-while': {
    evaluate: ([start, ratio, fn], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertLitsFunction(fn, sourceCodeInfo)

      const geometric = []
      for (let i = 0; ; i += 1) {
        const value = start * ratio ** i
        if (!executeFunction(fn, [value, i], contextStack, sourceCodeInfo)) {
          break
        }
        geometric[i] = value
      }
      return geometric
    },
    paramCount: 3,
  },
  'c:geometric-nth': {
    evaluate: ([start, ratio, n], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return start * ratio ** (n - 1)
    },
    paramCount: 3,
  },
  'c:geometric?': {
    evaluate: ([start, ratio, n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo)
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })

      return isInGeometricSequence(start, ratio, n)
    },
    paramCount: 3,
  },
}
