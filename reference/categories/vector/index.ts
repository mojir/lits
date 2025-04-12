import type { FunctionReference } from '../..'
import type { VectorReductionKeys } from '../../../src/builtin/normalExpressions/categories/namespaces/vector/reductionFunctions'
import { type VectorApiName, getOperatorArgs } from '../../api'
import { geometricMeanReference, harmonicMeanReference, meanReference } from './mean'
import { medianReference } from './median'
import { sampleVarianceReference, varianceReference } from './variance'
import { sampleStandardDeviationReference, standardDeviationReference } from './standardDeviation'
import { sampleSkewnessReference, skewnessReference } from './skewness'
import { interquartileRangeReference } from './iqr'
import { sumReference } from './sum'
import { prodReference } from './prod'
import { minReference } from './min'
import { maxReference } from './max'
import { spanReference } from './span'
import { excessKurtoisReference, kurtosisReference, sampleExcessKurtosisReference, sampleKurtosisReference } from './kurtosis'
import { rootMeanSquareReference } from './rms'
import { meanAbsoluteDeviationReference } from './mad'
import { medianAbsoluteDeviationReference } from './medad'
import { giniCoefficientReference } from './giniCoefficient'
import { entropyReference } from './entropy'

export type VectorReductionReference<T extends string> = {
  [key in VectorReductionKeys<T>]: FunctionReference<'Vector'>
}

export const vectorReference: Record<VectorApiName, FunctionReference<'Vector'>> = {
  ...meanReference,
  ...geometricMeanReference,
  ...harmonicMeanReference,
  ...medianReference,
  ...varianceReference,
  ...sampleVarianceReference,
  ...standardDeviationReference,
  ...sampleStandardDeviationReference,
  ...interquartileRangeReference,
  ...sumReference,
  ...prodReference,
  ...minReference,
  ...maxReference,
  ...spanReference,
  ...skewnessReference,
  ...sampleSkewnessReference,
  ...excessKurtoisReference,
  ...kurtosisReference,
  ...sampleExcessKurtosisReference,
  ...sampleKurtosisReference,
  ...rootMeanSquareReference,
  ...meanAbsoluteDeviationReference,
  ...medianAbsoluteDeviationReference,
  ...giniCoefficientReference,
  ...entropyReference,
  'vec:monotonic?': {
    title: 'vec:monotonic?',
    category: 'Vector',
    description: 'Checks if a vector is monotonic.',
    linkName: 'vec-colon-monotonic-question',
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
    linkName: 'vec-colon-strictly-monotonic-question',
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
    linkName: 'vec-colon-increasing-question',
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
    linkName: 'vec-colon-decreasing-question',
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
    linkName: 'vec-colon-strictly-increasing-question',
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
    linkName: 'vec-colon-strictly-decreasing-question',
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
      stop: {
        type: 'number',
        description: 'The ending value.',
      },
      n: {
        type: 'integer',
        description: 'The number of values to generate.',
      },
    },
    variants: [
      { argumentNames: ['start', 'stop', 'n'] },
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
      ...getOperatorArgs('number', 'integer'),
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
      ...getOperatorArgs('number', 'integer'),
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
      'vec:quartiles(vec:generate(1000, -> ln($ + 1)))',
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
      ...getOperatorArgs('number', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'percentile'] },
    ],
    examples: [
      'vec:percentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'vec:percentile(range(100) ^ 0.5, 0)',
      'vec:percentile(range(100) ^ 0.5, 10)',
      'vec:percentile(range(100) ^ 0.5, 20)',
      'vec:percentile(range(100) ^ 0.5, 30)',
      'vec:percentile(range(100) ^ 0.5, 40)',
      'vec:percentile(range(100) ^ 0.5, 50)',
      'vec:percentile(range(100) ^ 0.5, 60)',
      'vec:percentile(range(100) ^ 0.5, 70)',
      'vec:percentile(range(100) ^ 0.5, 80)',
      'vec:percentile(range(100) ^ 0.5, 90)',
      'vec:percentile(range(100) ^ 0.5, 100)',
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
      ...getOperatorArgs('number', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'quantile'] },
    ],
    examples: [
      'vec:quantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'vec:quantile(range(100) ^ 0.5, 0)',
      'vec:quantile(range(100) ^ 0.5, 0.1)',
      'vec:quantile(range(100) ^ 0.5, 0.2)',
      'vec:quantile(range(100) ^ 0.5, 0.3)',
      'vec:quantile(range(100) ^ 0.5, 0.4)',
      'vec:quantile(range(100) ^ 0.5, 0.5)',
      'vec:quantile(range(100) ^ 0.5, 0.6)',
      'vec:quantile(range(100) ^ 0.5, 0.7)',
      'vec:quantile(range(100) ^ 0.5, 0.8)',
      'vec:quantile(range(100) ^ 0.5, 0.9)',
      'vec:quantile(range(100) ^ 0.5, 1)',
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
      ...getOperatorArgs('number', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'bins'] },
    ],
    examples: [
      'vec:histogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'vec:histogram([1, 2, 3, 4, 5], 5)',
      'vec:histogram([1, 2, 3, 4, 5], 10)',
      'vec:histogram([1, 2, 3, 4, 5], 1)',
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
      ...getOperatorArgs('number', 'integer'),
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
    linkName: 'vec-colon-outliers-question',
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
  'vec:bincount': {
    title: 'vec:bincount',
    category: 'Vector',
    description: 'counts occurrences of each `integer` in a vector, returning an array where index `i` contains the count of value `i`, with optional **minimum size** and **weights parameters**.',
    linkName: 'vec-colon-bincount',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to count occurrences in.',
      },
      minSize: {
        type: 'integer',
        description: 'Optional minimum size of the output array.',
      },
      weights: {
        type: 'number',
        array: true,
        description: 'Optional weights for each element in the vector.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
      { argumentNames: ['vector', 'minSize'] },
      { argumentNames: ['vector', 'minSize', 'weights'] },
    ],
    examples: [
      'vec:bincount([1, 2, 3])',
      'vec:bincount([1, 2, 2, 3, 3])',
    ],
    noOperatorDocumentation: true,
  },
  'vec:winsorize': {
    title: 'vec:winsorize',
    category: 'Vector',
    description: 'Limits extreme values in a `vector` by replacing values below the **lower quantile** and above the **upper quantile** with the values at those quantiles. The function takes a `vector` of values and **quantile thresholds** (between 0 and 1), with the upper quantile. Winsorization reduces the influence of outliers while preserving the overall distribution shape, making statistical analyses more robust.',
    linkName: 'vec-colon-winsorize',
    returns: {
      type: 'vector',
    },
    args: {
      'vector': {
        type: 'vector',
        description: 'The vector to winsorize.',
      },
      'lower-quantile': {
        type: 'number',
        description: 'The lower quantile threshold (between 0 and 1).',
      },
      'upper-quantile': {
        type: 'number',
        description: 'Optional Upper quantile threshold (between 0 and 1). Defaults to `(1 - lower-quantile)` if `lower-quantile <= 0.5` otherwise `1`.',
      },
    },
    variants: [
      { argumentNames: ['vector', 'lower-quantile'] },
      { argumentNames: ['vector', 'lower-quantile', 'upper-quantile'] },
    ],
    examples: [
      'vec:winsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25)',
      'vec:winsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.75)',
      'vec:winsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.5)',
    ],
    noOperatorDocumentation: true,
  },
  'vec:mse': {
    title: 'vec:mse',
    category: 'Vector',
    description: 'Calculates the **Mean Squared Error (MSE)** between two vectors. Returns the average of the squared differences between corresponding elements.',
    linkName: 'vec-colon-mse',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'vec:mse([1, 2, 3], [1, 2, 3])',
      'vec:mse([1, 2, 3], [4, 5, 6])',
      'vec:mse([1, 2, 3], [2, 2, 2])',
      'vec:mse([1, 2], [3, 3])',
      'vec:mse([1], [3])',
    ],
  },
  'vec:rmse': {
    title: 'vec:rmse',
    category: 'Vector',
    description: 'Calculates the **Root Mean Squared Error (RMSE)** between two vectors. Returns the square root of the average of the squared differences between corresponding elements.',
    linkName: 'vec-colon-rmse',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'vec:rmse([1, 2, 3], [1, 2, 3])',
      'vec:rmse([1, 2, 3], [4, 5, 6])',
      'vec:rmse([1, 2, 3], [2, 2, 2])',
      'vec:rmse([1, 2], [3, 3])',
      'vec:rmse([1], [3])',
    ],
  },
  'vec:mae': {
    title: 'vec:mae',
    category: 'Vector',
    description: 'Calculates the **Mean Absolute Error (MAE)** between two vectors. Returns the average of the absolute differences between corresponding elements.',
    linkName: 'vec-colon-mae',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'vec:mae([1, 2, 3], [1, 2, 3])',
      'vec:mae([1, 2, 3], [4, 5, 6])',
      'vec:mae([1, 2, 3], [2, 2, 2])',
      'vec:mae([1, 2], [3, 3])',
      'vec:mae([1], [3])',
    ],
  },
  'vec:smape': {
    title: 'vec:smape',
    category: 'Vector',
    description: 'Calculates the **Symmetric Mean Absolute Percentage Error (SMAPE)** between two vectors. Returns the average of the absolute percentage differences between corresponding elements.',
    linkName: 'vec-colon-smape',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'vec:smape([1, 2, 3], [1, 2, 3])',
      'vec:smape([1, 2, 3], [4, 5, 6])',
      'vec:smape([1, 2, 3], [2, 2, 2])',
      'vec:smape([1, 2], [3, 3])',
      'vec:smape([1], [3])',
    ],
  },
}
