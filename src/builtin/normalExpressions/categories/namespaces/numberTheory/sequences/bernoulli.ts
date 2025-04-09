import { assertFunctionLike } from '../../../../../../typeGuards/lits'
import { assertNumber } from '../../../../../../typeGuards/number'
import { binomialCoefficient } from '../binomialCefficient'
import type { SequenceNormalExpressions } from '.'

function getBernoulliSeq(length: number): number[] {
  const bernoulli = [1]
  for (let n = 1; n < length; n += 1) {
    let sum = 0
    for (let k = 0; k < n; k += 1) {
      sum += binomialCoefficient(n + 1, k) * bernoulli[k]!
    }
    bernoulli[n] = n > 1 && n % 2 === 1 ? 0 : -sum / (n + 1)
  }
  return bernoulli
}

/**
 * Generates Bernoulli numbers as long as the predicate function returns true
 * @param predicate - Function that takes a Bernoulli number and its index and returns true if generation should continue
 * @returns Array of Bernoulli numbers generated until predicate returns false
 */
function generateBernoulli(
  predicate: (value: number, index: number) => boolean,
): number[] {
  const batchSize = 100
  // Start with computing the Bernoulli numbers
  const bernoulli: number[] = [1]
  let n = 1

  // Continue generating as long as the predicate returns true
  while (true) {
    // Generate a batch of numbers at a time for efficiency
    const targetLength = bernoulli.length + batchSize

    for (; n < targetLength; n++) {
      let sum = 0
      for (let k = 0; k < n; k++) {
        sum += binomialCoefficient(n + 1, k) * bernoulli[k]!
      }

      const newValue = n > 1 && n % 2 === 1 ? 0 : -sum / (n + 1)

      // Check if we should continue
      if (!predicate(newValue, n)) {
        // We're done, return the generated sequence (including the last value)
        return bernoulli
      }
      bernoulli.push(newValue)
    }
  }
}

export const bernoulliNormalExpressions: Omit<SequenceNormalExpressions<'bernoulli'>, 'nth:bernoulli?'> = {
  'nth:bernoulli-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      return getBernoulliSeq(length)
    },
    paramCount: 1,
  },
  'nth:bernoulli-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const bernoulli = getBernoulliSeq(n)
      return bernoulli[n - 1]!
    },
    paramCount: 1,
  },
  'nth:bernoulli-take-while': {
    evaluate: ([fn], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertFunctionLike(fn, sourceCodeInfo)
      const bernoulli = generateBernoulli((value, index) => !!executeFunction(fn, [value, index], contextStack))
      return bernoulli
    },
    paramCount: 1,
  },
}
