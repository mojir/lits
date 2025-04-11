import type { FunctionReference } from '../..'
import type { VectorReductionKeys } from '../../../src/builtin/normalExpressions/categories/namespaces/vector/reductionFunctions'
import type { VectorApiName } from '../../api'
import { meanReference } from './mean'
import { medianReference } from './median'
import { varianceReference } from './variance'
import { sampleVarianceReference } from './sampleVariance'
import { sumReference } from './sum'
import { prodReference } from './prod'
import { minReference } from './min'
import { maxReference } from './max'

export type VectorReductionReference<T extends string> = {
  [key in VectorReductionKeys<T>]: FunctionReference<'Vector'>
}

export const vectorReference: Record<VectorApiName, FunctionReference<'Vector'>> = {
  ...meanReference,
  ...medianReference,
  ...varianceReference,
  ...sampleVarianceReference,
  ...sumReference,
  ...prodReference,
  ...minReference,
  ...maxReference,
  'vec:vector?': {
    title: 'vec:vector?',
    category: 'Vector',
    description: 'Checks if a value is a vector.',
    linkName: 'vec-colon-vector-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      value: {
        type: 'any',
        description: 'The value to check.',
      },
    },
    variants: [
      { argumentNames: ['value'] },
    ],
    examples: [
      'vec:vector?(1)',
      'vec:vector?([1, 2, 3])',
      'vec:vector?([1, 2, "3"])',
    ],
  },
  'vec:monotonic?': {
    title: 'vec:monotonic?',
    category: 'Vector',
    description: 'Checks if a vector is monotonic.',
    linkName: 'vec-colon-monotonic-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:monotonic?([1, 2, 3])',
      'vec:monotonic?([1, 2, 2, 3])',
      'vec:monotonic?([3, 2, 1])',
      'vec:monotonic?([3, 2, 1, 1])',
      'vec:monotonic?([3, 2, 1, 2])',
      'vec:monotonic?([1])',
      'vec:monotonic?([])',
    ],
  },
  'vec:strictly-monotonic?': {
    title: 'vec:strictly-monotonic?',
    category: 'Vector',
    description: 'Checks if a vector is strictly monotonic.',
    linkName: 'vec-colon-strictly-monotonic-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:strictly-monotonic?([1, 2, 3])',
      'vec:strictly-monotonic?([1, 2, 2, 3])',
      'vec:strictly-monotonic?([3, 2, 1])',
      'vec:strictly-monotonic?([3, 2, 1, 1])',
      'vec:strictly-monotonic?([3, 2, 1, 2])',
      'vec:strictly-monotonic?([1])',
      'vec:strictly-monotonic?([])',
    ],
  },
  'vec:increasing?': {
    title: 'vec:increasing?',
    category: 'Vector',
    description: 'Checks if a vector is increasing.',
    linkName: 'vec-colon-increasing-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:increasing?([1, 2, 3])',
      'vec:increasing?([1, 2, 2, 3])',
      'vec:increasing?([3, 2, 1])',
      'vec:increasing?([3, 2, 1, 1])',
      'vec:increasing?([3, 2, 1, 2])',
      'vec:increasing?([1])',
      'vec:increasing?([])',
    ],
  },
  'vec:decreasing?': {
    title: 'vec:decreasing?',
    category: 'Vector',
    description: 'Checks if a vector is decreasing.',
    linkName: 'vec-colon-decreasing-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:decreasing?([1, 2, 3])',
      'vec:decreasing?([1, 2, 2, 3])',
      'vec:decreasing?([3, 2, 1])',
      'vec:decreasing?([3, 2, 1, 1])',
      'vec:decreasing?([3, 2, 1, 2])',
      'vec:decreasing?([1])',
      'vec:decreasing?([])',
    ],
  },
  'vec:strictly-increasing?': {
    title: 'vec:strictly-increasing?',
    category: 'Vector',
    description: 'Checks if a vector is strictly increasing.',
    linkName: 'vec-colon-strictly-increasing-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:strictly-increasing?([1, 2, 3])',
      'vec:strictly-increasing?([1, 2, 2, 3])',
      'vec:strictly-increasing?([3, 2, 1])',
      'vec:strictly-increasing?([3, 2, 1, 1])',
      'vec:strictly-increasing?([3, 2, 1, 2])',
      'vec:strictly-increasing?([1])',
      'vec:strictly-increasing?([])',
    ],
  },
  'vec:strictly-decreasing?': {
    title: 'vec:strictly-decreasing?',
    category: 'Vector',
    description: 'Checks if a vector is strictly decreasing.',
    linkName: 'vec-colon-strictly-decreasing-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:strictly-decreasing?([1, 2, 3])',
      'vec:strictly-decreasing?([1, 2, 2, 3])',
      'vec:strictly-decreasing?([3, 2, 1])',
      'vec:strictly-decreasing?([3, 2, 1, 1])',
      'vec:strictly-decreasing?([3, 2, 1, 2])',
      'vec:strictly-decreasing?([1])',
      'vec:strictly-decreasing?([])',
    ],
  },
  'vec:sum': {
    title: 'vec:sum',
    category: 'Vector',
    description: 'Returns the sum of all elements in the vector.',
    linkName: 'vec-colon-sum',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to sum.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sum([1, 2, 3])',
      'vec:sum([1, 2, -3])',
    ],
  },
  'vec:median': {
    title: 'vec:median',
    category: 'Vector',
    description: 'Returns the median of all elements in the vector.',
    linkName: 'vec-colon-median',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the median of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:median([1, 2, 3])',
      'vec:median([1, 2, -3])',
      'vec:median([1, 2, 3, 4])',
      'vec:median([1, 2, -3, 4])',
    ],
  },
  'vec:mode': {
    title: 'vec:mode',
    category: 'Vector',
    description: 'Returns the mode of all elements in the vector.',
    linkName: 'vec-colon-mode',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the mode of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:mode([1, 2, 3])',
      'vec:mode([1, 2, -3, 1])',
      'vec:mode([2, 2, 3, 3, 4])',
      'vec:mode([2, 2, 3, 3])',
      'vec:mode([1, 2, 3, 2, 1, 2])',
    ],
  },
  'vec:sample-variance': {
    title: 'vec:sample-variance',
    category: 'Vector',
    description: 'Returns the sample variance of all elements in the vector.',
    linkName: 'vec-colon-sample-variance',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample variance of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sample-variance([1, 2, 3])',
      'vec:sample-variance([1, 2, -3])',
      'vec:sample-variance([1, 2, 3, 4])',
      'vec:sample-variance([1, 2, -3, 4])',
      'vec:sample-variance([1, 2, 3, 40, 50])',
    ],
  },
  'vec:stdev': {
    title: 'vec:stdev',
    category: 'Vector',
    description: 'Returns the standard deviation of all elements in the vector.',
    linkName: 'vec-colon-stdev',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the standard deviation of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:stdev([1, 2, 3])',
      'vec:stdev([1, 2, -3])',
      'vec:stdev([1, 2, 3, 4])',
      'vec:stdev([1, 2, -3, 4])',
      'vec:stdev([1, 2, 3, 40, 50])',
    ],
  },
  'vec:sample-stdev': {
    title: 'vec:sample-stdev',
    category: 'Vector',
    description: 'Returns the sample standard deviation of all elements in the vector.',
    linkName: 'vec-colon-sample-stdev',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample standard deviation of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sample-stdev([1, 2, 3])',
      'vec:sample-stdev([1, 2, -3])',
      'vec:sample-stdev([1, 2, 3, 4])',
      'vec:sample-stdev([1, 2, -3, 4])',
      'vec:sample-stdev([1, 2, 3, 40, 50])',
    ],
  },
  'vec:min': {
    title: 'vec:min',
    category: 'Vector',
    description: 'Returns the minimum value of all elements in the vector.',
    linkName: 'vec-colon-min',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the minimum of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:min([1, 2, 3])',
      'vec:min([1, 1, 2, 3, 3])',
      'vec:min([1, 2, -3])',
      'vec:min([1, 2, 3, 4])',
      'vec:min([1, 2, -3, 4])',
    ],
  },
  'vec:max': {
    title: 'vec:max',
    category: 'Vector',
    description: 'Returns the maximum value of all elements in the vector.',
    linkName: 'vec-colon-max',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the maximum of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:max([1, 2, 3])',
      'vec:max([1, 1, 2, 3, 3])',
      'vec:max([1, 2, -3])',
      'vec:max([1, 2, 3, 4])',
      'vec:max([1, 2, -3, 4])',
    ],
  },
  'vec:min-index': {
    title: 'vec:min-index',
    category: 'Vector',
    description: 'Returns the index of the minimum value of all elements in the vector.',
    linkName: 'vec-colon-min-index',
    returns: {
      type: 'integer',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the minimum index of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:min-index([1, 2, 3])',
      'vec:min-index([1, 1, 2, 3, 3])',
      'vec:min-index([1, 2, -3])',
      'vec:min-index([1, 2, 3, 4])',
      'vec:min-index([1, 2, -3, 4])',
    ],
  },
  'vec:max-index': {
    title: 'vec:max-index',
    category: 'Vector',
    description: 'Returns the index of the maximum value of all elements in the vector.',
    linkName: 'vec-colon-max-index',
    returns: {
      type: 'integer',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the maximum index of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:max-index([1, 2, 3])',
      'vec:max-index([1, 1, 2, 3, 3])',
      'vec:max-index([1, 2, -3])',
      'vec:max-index([1, 2, 3, 4])',
      'vec:max-index([1, 2, -3, 4])',
    ],
  },
  'vec:sort-indices': {
    title: 'vec:sort-indices',
    category: 'Vector',
    description: 'Returns the indices of the elements in the vector sorted in ascending order.',
    linkName: 'vec-colon-sort-indices',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sorted indices of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sort-indices([1, 2, 3])',
      'vec:sort-indices([1, 1, 2, 3, 3])',
      'vec:sort-indices([1, 2, -3])',
      'vec:sort-indices([1, 2, 3, 4])',
      'vec:sort-indices([1, 2, -3, 4])',
    ],
  },
  'vec:count-values': {
    title: 'vec:count-values',
    category: 'Vector',
    description: 'Counts the number of occurrences of each value in the vector.',
    linkName: 'vec-colon-count-values',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to count the values of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:count-values([1, 2, 3])',
      'vec:count-values([1, 1, 2, 3, 3])',
      'vec:count-values([1, 2, -3])',
      'vec:count-values([1, 2, 2, 1, 3, 2, 4, 2, 1, 2, 2, 1, 3, 2, 4])',
    ],
  },
  'vec:linspace': {
    title: 'vec:linspace',
    category: 'Vector',
    description: 'Generates a vector of evenly spaced numbers between two values.',
    linkName: 'vec-colon-linspace',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting value.',
      },
      end: {
        type: 'number',
        description: 'The ending value.',
      },
      count: {
        type: 'integer',
        description: 'The number of values to generate.',
      },
    },
    variants: [
      { argumentNames: ['start', 'end', 'count'] },
    ],
    examples: [
      'vec:linspace(0, 10, 6)',
      'vec:linspace(10, 20, 25)',
    ],
  },
  'vec:ones': {
    title: 'vec:ones',
    category: 'Vector',
    description: 'Generates a vector of ones.',
    linkName: 'vec-colon-ones',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'vec:ones(5)',
      'vec:ones(10)',
      'vec:ones(0)',
    ],
  },
  'vec:zeros': {
    title: 'vec:zeros',
    category: 'Vector',
    description: 'Generates a vector of zeros.',
    linkName: 'vec-colon-zeros',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'vec:zeros(5)',
      'vec:zeros(10)',
      'vec:zeros(0)',
    ],
  },
  'vec:fill': {
    title: 'vec:fill',
    category: 'Vector',
    description: 'Generates a vector filled with a number.',
    linkName: 'vec-colon-fill',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
      },
      value: {
        type: 'number',
        description: 'The value to fill the vector with.',
      },
    },
    variants: [
      { argumentNames: ['length', 'value'] },
    ],
    examples: [
      'vec:fill(5, PI)',
      'vec:fill(10, -1)',
    ],
  },
  'vec:generate': {
    title: 'vec:generate',
    category: 'Vector',
    description: 'Generates a vector of numbers based on a function.',
    linkName: 'vec-colon-generate',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
      },
      func: {
        type: 'function',
        description: 'A function that takes an index and returns a number.',
      },
    },
    variants: [
      { argumentNames: ['length', 'func'] },
    ],
    examples: [
      'vec:generate(5, -> $ * 2)',
      'vec:generate(10, -> $ + 1)',
      'vec:generate(0, -> $ + 1)',
    ],
  },
  'vec:cumsum': {
    title: 'vec:cumsum',
    category: 'Vector',
    description: 'Calculates the cumulative sum of a vector.',
    linkName: 'vec-colon-cumsum',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the cumulative sum of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:cumsum([1, 2, 3])',
      'vec:cumsum([1, 2, -3])',
      'vec:cumsum([])',
    ],
  },
  'vec:cumprod': {
    title: 'vec:cumprod',
    category: 'Vector',
    description: 'Calculates the cumulative product of a vector.',
    linkName: 'vec-colon-cumprod',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the cumulative product of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:cumprod([1, 2, 3])',
      'vec:cumprod([1, 2, -3, 0, 10])',
      'vec:cumprod([])',
    ],
  },
  'vec:quartiles': {
    title: 'vec:quartiles',
    category: 'Vector',
    description: 'Calculates the quartiles of a vector. Returns an array containing the first, second (median), and third quartiles.',
    linkName: 'vec-colon-quartiles',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the quartiles of. Minimum length is 4.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:quartiles([1, 2, 3, 4])',
      'vec:quartiles([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'vec:quartiles(range(1, 1000))',
      'vec:quartiles(vec:generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'vec:quartiles(vec:generate(1000, -> log($ + 1)))',
    ],
  },
  'vec:iqr': {
    title: 'vec:iqr',
    category: 'Vector',
    description: 'Calculates the interquartile range of a vector. Returns the difference between the third and first quartiles.',
    linkName: 'vec-colon-iqr',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the interquartile range of. Minimum length is 4.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:iqr([1, 2, 3, 4])',
      'vec:iqr([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'vec:iqr(range(1, 1000))',
      'vec:iqr(vec:generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'vec:iqr(vec:generate(1000, -> log($ + 1)))',
    ],
  },
  'vec:percentile': {
    title: 'vec:percentile',
    category: 'Vector',
    description: 'Calculates the percentile of a vector. Returns the value at the specified percentile.',
    linkName: 'vec-colon-percentile',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The non empty vector to calculate the percentile of.',
      },
      percentile: {
        type: 'number',
        description: 'The percentile to calculate. Must be between 0 and 1.',
      },
    },
    variants: [
      { argumentNames: ['vector', 'percentile'] },
    ],
    examples: [
      'vec:percentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'vec:percentile(range(100) vec:^ 0.5, 0)',
      'vec:percentile(range(100) vec:^ 0.5, 10)',
      'vec:percentile(range(100) vec:^ 0.5, 20)',
      'vec:percentile(range(100) vec:^ 0.5, 30)',
      'vec:percentile(range(100) vec:^ 0.5, 40)',
      'vec:percentile(range(100) vec:^ 0.5, 50)',
      'vec:percentile(range(100) vec:^ 0.5, 60)',
      'vec:percentile(range(100) vec:^ 0.5, 70)',
      'vec:percentile(range(100) vec:^ 0.5, 80)',
      'vec:percentile(range(100) vec:^ 0.5, 90)',
      'vec:percentile(range(100) vec:^ 0.5, 100)',
    ],
  },
  'vec:quantile': {
    title: 'vec:quantile',
    category: 'Vector',
    description: 'Calculates the quantile of a vector. Returns the value at the specified quantile.',
    linkName: 'vec-colon-quantile',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The non empty vector to calculate the quantile of.',
      },
      quantile: {
        type: 'number',
        description: 'The quantile to calculate. Must be between 0 and 1.',
      },
    },
    variants: [
      { argumentNames: ['vector', 'quantile'] },
    ],
    examples: [
      'vec:quantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'vec:quantile(range(100) vec:^ 0.5, 0)',
      'vec:quantile(range(100) vec:^ 0.5, 0.1)',
      'vec:quantile(range(100) vec:^ 0.5, 0.2)',
      'vec:quantile(range(100) vec:^ 0.5, 0.3)',
      'vec:quantile(range(100) vec:^ 0.5, 0.4)',
      'vec:quantile(range(100) vec:^ 0.5, 0.5)',
      'vec:quantile(range(100) vec:^ 0.5, 0.6)',
      'vec:quantile(range(100) vec:^ 0.5, 0.7)',
      'vec:quantile(range(100) vec:^ 0.5, 0.8)',
      'vec:quantile(range(100) vec:^ 0.5, 0.9)',
      'vec:quantile(range(100) vec:^ 0.5, 1)',
    ],
  },
  'vec:span': {
    title: 'vec:span',
    category: 'Vector',
    description: 'Returns the difference between the maximum and minimum values in a vector.',
    linkName: 'vec-colon-span',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the span of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:span([1, 2, 3])',
      'vec:span([1, 1, 2, 3, 3])',
      'vec:span([1, 2, -3])',
    ],
  },
  'vec:skewness': {
    title: 'vec:skewness',
    category: 'Vector',
    description: 'Returns the `skewness` coefficient of all elements in the `vector`. Requires at least 3 elements in the `vector`. Measures the asymmetry of the data distribution - positive values indicate right skew (tail extends toward positive values), negative values indicate left skew, and zero indicates symmetry.',
    linkName: 'vec-colon-skewness',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the skewness of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:skewness([1, 2, 3])',
      'vec:skewness([1, 2, -3])',
      'vec:skewness([1, 2, 3, 4])',
      'vec:skewness([1, 2, -3, 4])',
      'vec:skewness([1, 2, 3, 40, 50])',
      'vec:skewness([-10, 2, 3, 4, 5])',
    ],
  },
  'vec:sample-skewness': {
    title: 'vec:sample-skewness',
    category: 'Vector',
    description: 'Returns the `sample skewness` coefficient of all elements in the `vector`. Requires at least 3 elements in the `vector`. Measures the asymmetry of the data distribution - positive values indicate right skew (tail extends toward positive values), negative values indicate left skew, and zero indicates symmetry.',
    linkName: 'vec-colon-sample-skewness',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample skewness of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sample-skewness([1, 2, 3])',
      'vec:sample-skewness([1, 2, -3])',
      'vec:sample-skewness([1, 2, 3, 4])',
      'vec:sample-skewness([1, 2, -3, 4])',
      'vec:sample-skewness([1, 2, 3, 40, 50])',
      'vec:sample-skewness([-10, 2, 3, 4, 5])',
    ],
  },
  'vec:kurtosis': {
    title: 'vec:kurtosis',
    category: 'Vector',
    description: 'Returns the `kurtosis` coefficient of all elements in the `vector`. Requires at least 4 elements in the `vector`. Measures the "tailedness" of the data distribution - positive values indicate a distribution with heavier tails than a normal distribution, negative values indicate lighter tails.',
    linkName: 'vec-colon-kurtosis',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the kurtosis of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:kurtosis([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])', // Uniform distribution
      'vec:kurtosis([-3, -2, -1, 0, 1, 2, 3])', // Evenly spaced values
      'vec:kurtosis([10, 20, 30, 40, 50, 60, 70, 80])', // Flat distribution
      'vec:kurtosis([0, 0.5, 1, 1.2, 1.5, 1.8, 2, 2.5, 3])', // Approximates normal distribution
      'vec:kurtosis([1, 1, 1, 1, 10, 1, 1, 1, 1])', // Mostly similar values with outliers
      'vec:kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0, 15])', // Heavy tailed
      'vec:kurtosis([5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 20])', // Peaked with an outlier
    ],
  },
  'vec:sample-kurtosis': {
    title: 'vec:sample-kurtosis',
    category: 'Vector',
    description: 'Returns the `sample kurtosis` coefficient of all elements in the `vector`. Requires at least 4 elements in the `vector`. Measures the "tailedness" of the data distribution - positive values indicate a distribution with heavier tails than a normal distribution, negative values indicate lighter tails.',
    linkName: 'vec-colon-sample-kurtosis',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample kurtosis of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sample-kurtosis([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])', // Uniform distribution
      'vec:sample-kurtosis([-3, -2, -1, 0, 1, 2, 3])', // Evenly spaced values
      'vec:sample-kurtosis([10, 20, 30, 40, 50, 60, 70, 80])', // Flat distribution
      'vec:sample-kurtosis([0, 0.5, 1, 1.2, 1.5, 1.8, 2, 2.5, 3])', // Approximates normal distribution
      'vec:sample-kurtosis([1, 1, 1, 1, 10, 1, 1, 1, 1])', // Mostly similar values with outliers
      'vec:sample-kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])', // Heavy tailed
      'vec:sample-kurtosis([5, 6, 6, 6, 6, 7, 7, 7])', // Peaked with an outlier
    ],
  },
  'vec:excess-kurtosis': {
    title: 'vec:excess-kurtosis',
    category: 'Vector',
    description: 'Returns the `excess kurtosis` coefficient of all elements in the `vector`. Requires at least 4 elements in the `vector`. Measures the "tailedness" of the data distribution - positive values indicate a distribution with heavier tails than a normal distribution, negative values indicate lighter tails.',
    linkName: 'vec-colon-excess-kurtosis',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the excess kurtosis of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:excess-kurtosis([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])', // Uniform distribution
      'vec:excess-kurtosis([-3, -2, -1, 0, 1, 2, 3])', // Evenly spaced values
      'vec:excess-kurtosis([10, 20, 30, 40, 50, 60, 70, 80])', // Flat distribution
      'vec:excess-kurtosis([0, 0.5, 1, 1.2, 1.5, 1.8, 2, 2.5])', // Approximates normal distribution
      'vec:excess-kurtosis([1, 1, 1, 1, 10, 1, 1, 1, 1])', // Mostly similar values with outliers
      'vec:excess-kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])', // Heavy tailed
      'vec:excess-kurtosis([5, 6, 6, 6, 6, 7, 7, 7])', // Peaked with an outlier
    ],
  },
  'vec:sample-excess-kurtosis': {
    title: 'vec:sample-excess-kurtosis',
    category: 'Vector',
    description: 'Returns the `sample excess kurtosis` coefficient of all elements in the `vector`. Requires at least 4 elements in the `vector`. Measures the "tailedness" of the data distribution - positive values indicate a distribution with heavier tails than a normal distribution, negative values indicate lighter tails.',
    linkName: 'vec-colon-sample-excess-kurtosis',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample excess kurtosis of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sample-excess-kurtosis([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])', // Uniform distribution
      'vec:sample-excess-kurtosis([-3, -2, -1, 0, 1, 2, 3])', // Evenly spaced values
      'vec:sample-excess-kurtosis([10, 20, 30, 40, 50, 60, 70, 80])', // Flat distribution
      'vec:sample-excess-kurtosis([0, 0.5, 1, 1.2, 1.5, 1.8])', // Approximates normal distribution
      'vec:excess-excess-kurtosis([1, 1, 1, 1, 10, 1, 1, 1, 1])', // Mostly similar values with outliers
      'vec:excess-excess-kurtosis([0, 1, 1, 2, 2, 2, 2, 2, 1, 1, 0])', // Heavy tailed
      'vec:excess-excess-kurtosis([5, 6, 6, 6, 6, 7, 7, 7])', // Peaked with an outlier
    ],
  },
  'vec:geometric-mean': {
    title: 'vec:geometric-mean',
    category: 'Vector',
    description: 'Returns the geometric mean of all elements in the vector.',
    linkName: 'vec-colon-geometric-mean',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the geometric mean of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:geometric-mean([1, 2, 3])',
      'vec:geometric-mean([1, 2, -3])',
      'vec:geometric-mean([1, 2, 3, 4])',
      'vec:geometric-mean([1, 2, -3, 4])',
      'vec:geometric-mean([1, 2, 3, 40, 50])',
      'vec:geometric-mean([1, 2, 3, 4, 5])',
    ],
  },
  'vec:harmonic-mean': {
    title: 'vec:harmonic-mean',
    category: 'Vector',
    description: 'Returns the harmonic mean of all elements in the vector.',
    linkName: 'vec-colon-harmonic-mean',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the harmonic mean of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:harmonic-mean([1, 2, 3])',
      'vec:harmonic-mean([1, 2, -3])',
      'vec:harmonic-mean([1, 2, 3, 4])',
      'vec:harmonic-mean([1, 2, -3, 4])',
      'vec:harmonic-mean([1, 2, 3, 40, 50])',
      'vec:harmonic-mean([1, 2, 3, 4, 5])',
    ],
  },
  'vec:rms': {
    title: 'vec:rms',
    category: 'Vector',
    description: 'Returns the root mean square of all elements in the vector.',
    linkName: 'vec-colon-rms',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the root mean square of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:rms([1, 2, 3])',
      'vec:rms([1, 2, -3])',
      'vec:rms([1, 2, 3, 4])',
      'vec:rms([1, 2, -3, 4])',
      'vec:rms([1, 2, 3, 40, 50])',
      'vec:rms([1, 2, 3, 4, 5])',
    ],
  },
  'vec:mad': {
    title: 'vec:mad',
    category: 'Vector',
    description: 'Returns the mean absolute deviation of all elements in the vector.',
    linkName: 'vec-colon-mad',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the mean absolute deviation of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:mad([1, 2, 3])',
      'vec:mad([1, 2, -3])',
      'vec:mad([1, 2, 3, 4])',
      'vec:mad([1, 2, -3, 4])',
      'vec:mad([1, 2, 3, 40, 50])',
    ],
  },
  'vec:medad': {
    title: 'vec:medad',
    category: 'Vector',
    description: 'Returns the median absolute deviation of all elements in the vector. Using scale 1.4826, the MedAD is a consistent estimator of the standard deviation for a normal distribution.',
    linkName: 'vec-colon-medad',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the median absolute deviation of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:medad([1, 2, 3])',
      'vec:medad([1, 2, -3])',
      'vec:medad([1, 2, 3, 4])',
      'vec:medad([1, 2, -3, 4])',
      'vec:medad([1, 2, 3, 40, 50])',
    ],
  },
  'vec:histogram': {
    title: 'vec:histogram',
    category: 'Vector',
    description: 'Creates a histogram from a numeric `array` by dividing the data range into the specified number of bins. Returns an `array` of `[binStart, binEnd, count]` tuples representing each bin\'s range and the number of values within it. Handles empty arrays, identical values, and properly places maximum values in the last bin.',
    linkName: 'vec-colon-histogram',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The numeric array to create a histogram from.',
      },
      bins: {
        type: 'integer',
        description: 'The number of bins to divide the data range into.',
      },
    },
    variants: [
      { argumentNames: ['vector', 'bins'] },
    ],
    examples: [
      'vec:histogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'vec:histogram([1, 2, 3, 4, 5], 5)',
      'vec:histogram([1, 2, 3, 4, 5], 10)',
      'vec:histogram([1, 2, 3, 4, 5], 0)',
    ],
  },
  'vec:ecdf': {
    title: 'vec:ecdf',
    category: 'Vector',
    description: 'Calculates the empirical cumulative distribution function value for a given threshold in a non empty dataset. Returns the proportion of values in the `array` that are less than or equal to the specified threshold.',
    linkName: 'vec-colon-ecdf',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The numeric array to calculate the ECDF from.',
      },
      threshold: {
        type: 'number',
        description: 'The threshold value to calculate the ECDF for.',
      },
    },
    variants: [
      { argumentNames: ['vector', 'threshold'] },
    ],
    examples: [
      'vec:ecdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)',
      'vec:ecdf([1, 2, 3, 4, 5], 3)',
      'vec:ecdf([1, 2, 3, 4, 5], 0)',
      'vec:ecdf([1, 2, 3, 4, 5], 10)',
      'vec:ecdf([1, 2, 3, 4, 5], 2)',
    ],
  },
  'vec:outliers?': {
    title: 'vec:outliers?',
    category: 'Vector',
    description: 'Checks if the `vector` contains outliers based on the interquartile range (IQR) method. Returns `true` if outliers are present, `false` otherwise.',
    linkName: 'vec-colon-outliers-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to check for outliers.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:outliers?([1, 2, 3])',
      'vec:outliers?([1, 2, -3])',
      'vec:outliers?([1, 2, 3, 2, 4, 120])',
    ],
  },
  'vec:outliers': {
    title: 'vec:outliers',
    category: 'Vector',
    description: 'Identifies outliers in the `vector` based on the interquartile range (IQR) method. Returns an array of outlier values.',
    linkName: 'vec-colon-outliers',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to check for outliers.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:outliers([1, 2, 3])',
      'vec:outliers([1, 2, -3])',
      'vec:outliers([1, 2, 3, 2, 4, 120])',
    ],
  },
}
