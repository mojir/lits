import { assertNonEmptyVector, assertVector } from '../../typeGuards/annotatedArrays'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../interface'

function calcMedian(vector: number[]): number {
  const sorted = [...vector].sort((a, b) => a - b)
  const mid = Math.floor(sorted.length / 2)
  return sorted.length % 2 === 0
    ? (sorted[mid - 1]! + sorted[mid]!) / 2
    : sorted[mid]!
}

export const vectorNormalExpression: BuiltinNormalExpressions = {
  sum: {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      return vector.reduce((acc, val) => acc + val, 0)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'vector',
      returns: { type: 'number' },
      args: {
        vector: { type: 'vector', description: 'The vector to sum.' },
      },
      variants: [{ argumentNames: ['vector'] }],
      description: 'Returns the **sum** of all elements in the `vector`. Returns `0` for an empty vector.',
      seeAlso: ['prod', 'mean', 'median', 'vector.moving-sum', 'vector.centered-moving-sum', 'vector.running-sum', 'vector.cumsum'],
      examples: [
        'sum([1, 2, 3, 4, 5])',
        'sum([1, -2, 3])',
        'sum([])',
      ],
    },
  },

  prod: {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      return vector.reduce((acc, val) => acc * val, 1)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'vector',
      returns: { type: 'number' },
      args: {
        vector: { type: 'vector', description: 'The vector to multiply.' },
      },
      variants: [{ argumentNames: ['vector'] }],
      description: 'Returns the **product** of all elements in the `vector`. Returns `1` for an empty vector.',
      seeAlso: ['sum', 'mean', 'median', 'vector.moving-prod', 'vector.centered-moving-prod', 'vector.running-prod', 'vector.cumprod'],
      examples: [
        'prod([1, 2, 3, 4, 5])',
        'prod([1, -2, 3])',
        'prod([])',
      ],
    },
  },

  mean: {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return vector.reduce((acc, val) => acc + val, 0) / vector.length
    },
    arity: toFixedArity(1),
    docs: {
      category: 'vector',
      returns: { type: 'number' },
      args: {
        vector: { type: 'vector', description: 'The vector to calculate the mean of.' },
      },
      variants: [{ argumentNames: ['vector'] }],
      description: 'Returns the arithmetic **mean** of all elements in the `vector`. Throws for an empty vector.',
      seeAlso: ['median', 'sum', 'prod', 'vector.moving-mean', 'vector.centered-moving-mean', 'vector.running-mean', 'vector.geometric-mean', 'vector.harmonic-mean', 'vector.rms', 'vector.mode'],
      examples: [
        'mean([1, 2, 3, 4, 5])',
        'mean([1, -2, 3])',
      ],
    },
  },

  median: {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return calcMedian(vector)
    },
    arity: toFixedArity(1),
    docs: {
      category: 'vector',
      returns: { type: 'number' },
      args: {
        vector: { type: 'vector', description: 'The vector to calculate the median of.' },
      },
      variants: [{ argumentNames: ['vector'] }],
      description: 'Returns the **median** of all elements in the `vector`. For even-length vectors, returns the average of the two middle values. Throws for an empty vector.',
      seeAlso: ['mean', 'sum', 'prod', 'vector.moving-median', 'vector.centered-moving-median', 'vector.running-median', 'vector.mode', 'vector.quartiles', 'vector.percentile', 'vector.iqr', 'vector.medad'],
      examples: [
        'median([1, 2, 3, 4, 5])',
        'median([1, 2, 3, 4])',
        'median([3, 1, 4, 1, 5])',
      ],
    },
  },
}
