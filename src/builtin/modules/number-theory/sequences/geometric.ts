import { assertFunctionLike } from '../../../../typeGuards/lits'
import { assertNumber } from '../../../../typeGuards/number'
import { approxEqual } from '../../../../utils'
import { toFixedArity } from '../../../../utils/arity'
import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
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
  if (approxEqual(initialTerm, 0)) {
    return approxEqual(number, 0)
  }
  if (approxEqual(ratio, 1)) {
    return approxEqual(number, initialTerm)
  }
  if (approxEqual(ratio, 0)) {
    return approxEqual(number, 0) || approxEqual(number, initialTerm)
  }

  // Check if the number is exactly the initial term
  if (approxEqual(number, initialTerm)) {
    return true
  }

  // Special case for ratio = -1 (alternating sequence)
  if (approxEqual(ratio, -1)) {
    // In an alternating sequence with ratio -1, the terms are just initialTerm and -initialTerm
    return approxEqual(number, initialTerm) || approxEqual(number, -initialTerm)
  }

  // For negative ratios, we need special handling
  if (ratio < 0) {
    // Calculate log using absolute values
    const logResult = Math.log(Math.abs(number / initialTerm)) / Math.log(Math.abs(ratio))

    // Check if logResult is very close to an integer
    const roundedLogResult = Math.round(logResult)
    if (!approxEqual(roundedLogResult, logResult) || roundedLogResult < 0) {
      return false
    }

    // For negative ratios, alternating terms have alternating signs
    // Check if sign matches what we expect based on the power
    const expectedSign = roundedLogResult % 2 === 0
      ? Math.sign(initialTerm)
      : Math.sign(initialTerm) * Math.sign(ratio)

    return Math.sign(number) === expectedSign
  }

  // For positive ratios

  // Quick check based on sequence direction
  if ((ratio > 1 && number < initialTerm) || (ratio < 1 && number > initialTerm)) {
    return false
  }

  // Calculate n in: number = initialTerm * (ratio^n)
  const logResult = Math.log(number / initialTerm) / Math.log(ratio)

  // Check if logResult is very close to an integer and non-negative
  const roundedLogResult = Math.round(logResult)
  if (!approxEqual(roundedLogResult, logResult) || roundedLogResult < 0) {
    return false
  }

  // Verify calculated value matches the number exactly
  const calculatedValue = initialTerm * ratio ** roundedLogResult

  return approxEqual(calculatedValue, number)
}
export const geometricNormalExpressions: SequenceNormalExpressions<'geometric'> = {
  'geometric-seq': {
    evaluate: ([start, ratio, length], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return Array.from({ length }, (_, i) => start * ratio ** i)
    },
    arity: toFixedArity(3),
  },
  'geometric-take-while': {
    evaluate: ([start, ratio, fn], sourceCodeInfo, contextStack, { executeFunction }) => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertFunctionLike(fn, sourceCodeInfo)
      const s = start
      const r = ratio
      const f = fn

      const geometric: number[] = []
      function loop(i: number): MaybePromise<number[]> {
        const value = s * r ** i
        return chain(executeFunction(f, [value, i], contextStack, sourceCodeInfo), (keep) => {
          if (!keep)
            return geometric
          geometric.push(value)
          return loop(i + 1)
        })
      }
      return loop(0)
    },
    arity: toFixedArity(3),
  },
  'geometric-nth': {
    evaluate: ([start, ratio, n], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return start * ratio ** (n - 1)
    },
    arity: toFixedArity(3),
  },
  'geometric?': {
    evaluate: ([start, ratio, n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo)
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(ratio, sourceCodeInfo, { finite: true })

      return isInGeometricSequence(start, ratio, n)
    },
    arity: toFixedArity(3),
  },
}
