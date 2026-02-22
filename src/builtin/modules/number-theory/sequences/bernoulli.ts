import { assertFunctionLike } from '../../../../typeGuards/lits'
import { assertNumber } from '../../../../typeGuards/number'
import { binomialCoefficient } from '../binomialCefficient'
import { toFixedArity } from '../../../../utils/arity'
import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
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
  predicate: (value: number, index: number) => MaybePromise<boolean>,
): MaybePromise<number[]> {
  const bernoulli: number[] = [1]

  function loop(n: number): MaybePromise<number[]> {
    let sum = 0
    for (let k = 0; k < n; k++) {
      sum += binomialCoefficient(n + 1, k) * bernoulli[k]!
    }

    const newValue = n > 1 && n % 2 === 1 ? 0 : -sum / (n + 1)

    return chain(predicate(newValue, n), (keep) => {
      if (!keep)
        return bernoulli
      bernoulli.push(newValue)
      return loop(n + 1)
    })
  }

  return loop(1)
}

export const bernoulliNormalExpressions: Omit<SequenceNormalExpressions<'bernoulli'>, 'bernoulli?'> = {
  'bernoulli-seq': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      return getBernoulliSeq(length)
    },
    arity: toFixedArity(1),
  },
  'bernoulli-nth': {
    evaluate: ([n], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      const bernoulli = getBernoulliSeq(n)
      return bernoulli[n - 1]!
    },
    arity: toFixedArity(1),
  },
  'bernoulli-take-while': {
    evaluate: ([fn], sourceCodeInfo, contextStack, { executeFunction }) => {
      assertFunctionLike(fn, sourceCodeInfo)
      const f = fn
      return generateBernoulli((value, index) => chain(executeFunction(f, [value, index], contextStack), val => !!val))
    },
    arity: toFixedArity(1),
  },
}
