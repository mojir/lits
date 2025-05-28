import { assertFunctionLike } from '../../../../../../typeGuards/lits'
import { assertNumber } from '../../../../../../typeGuards/number'
import { approxEqual } from '../../../../../../utils'
import { toFixedArity } from '../../../../../../utils/arity'
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
    return approxEqual(n, start)
  }

  // Calculate position in sequence
  const position = (n - start) / step

  // Position must be non-negative
  if (position < 0) {
    return false
  }

  // Find nearest integer position
  const roundedPosition = Math.round(position)

  // Calculate the value at that position
  const calculatedValue = start + step * roundedPosition

  // Check both if position is close to an integer and if the
  // calculated value is close to the input value
  return approxEqual(position, roundedPosition) && approxEqual(calculatedValue, n)
}

export const arithmeticNormalExpressions: SequenceNormalExpressions<'arithmetic'> = {
  'nth:arithmetic-seq': {
    evaluate: ([start, step, length], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return Array.from({ length }, (_, i) => start + i * step)
    },
    arity: toFixedArity(3),
  },
  'nth:arithmetic-take-while': {
    evaluate: ([start, step, fn], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertFunctionLike(fn, sourceCodeInfo)

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
    arity: toFixedArity(3),
  },
  'nth:arithmetic-nth': {
    evaluate: ([start, step, n], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return start + (n - 1) * step
    },
    arity: toFixedArity(3),
  },
  'nth:arithmetic?': {
    evaluate: ([start, step, n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo)
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })

      return isInArithmeticSequence(start, step, n)
    },
    arity: toFixedArity(3),
  },
}
