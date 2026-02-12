import type { FunctionReference } from '../..'
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

type VectorReductionKey<T extends string> = `vec.${T}`
type VectorMovingWindowKey<T extends string> = `vec.moving-${T}`
type VectorCenteredMovingWindowKey<T extends string> = `vec.centered-moving-${T}`
type VectorRunningKey<T extends string> = `vec.running-${T}`
type VectorReductionKeys<T extends string> = VectorReductionKey<T> | VectorMovingWindowKey<T> | VectorCenteredMovingWindowKey<T> | VectorRunningKey<T>

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
  'vec.monotonic?': {
    title: 'vec.monotonic?',
    category: 'Vector',
    description: 'Checks if a vector is monotonic.',
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
      'let vec = import("vec");\nvec.monotonic?([1, 2, 3])',
      'let vec = import("vec");\nvec.monotonic?([1, 2, 2, 3])',
      'let vec = import("vec");\nvec.monotonic?([3, 2, 1])',
      'let vec = import("vec");\nvec.monotonic?([3, 2, 1, 1])',
      'let vec = import("vec");\nvec.monotonic?([3, 2, 1, 2])',
      'let vec = import("vec");\nvec.monotonic?([1])',
      'let vec = import("vec");\nvec.monotonic?([])',
    ],
  },
  'vec.strictly-monotonic?': {
    title: 'vec.strictly-monotonic?',
    category: 'Vector',
    description: 'Checks if a vector is strictly monotonic.',
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
      'let vec = import("vec");\nvec.strictly-monotonic?([1, 2, 3])',
      'let vec = import("vec");\nvec.strictly-monotonic?([1, 2, 2, 3])',
      'let vec = import("vec");\nvec.strictly-monotonic?([3, 2, 1])',
      'let vec = import("vec");\nvec.strictly-monotonic?([3, 2, 1, 1])',
      'let vec = import("vec");\nvec.strictly-monotonic?([3, 2, 1, 2])',
      'let vec = import("vec");\nvec.strictly-monotonic?([1])',
      'let vec = import("vec");\nvec.strictly-monotonic?([])',
    ],
  },
  'vec.increasing?': {
    title: 'vec.increasing?',
    category: 'Vector',
    description: 'Checks if a vector is increasing.',
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
      'let vec = import("vec");\nvec.increasing?([1, 2, 3])',
      'let vec = import("vec");\nvec.increasing?([1, 2, 2, 3])',
      'let vec = import("vec");\nvec.increasing?([3, 2, 1])',
      'let vec = import("vec");\nvec.increasing?([3, 2, 1, 1])',
      'let vec = import("vec");\nvec.increasing?([3, 2, 1, 2])',
      'let vec = import("vec");\nvec.increasing?([1])',
      'let vec = import("vec");\nvec.increasing?([])',
    ],
  },
  'vec.decreasing?': {
    title: 'vec.decreasing?',
    category: 'Vector',
    description: 'Checks if a vector is decreasing.',
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
      'let vec = import("vec");\nvec.decreasing?([1, 2, 3])',
      'let vec = import("vec");\nvec.decreasing?([1, 2, 2, 3])',
      'let vec = import("vec");\nvec.decreasing?([3, 2, 1])',
      'let vec = import("vec");\nvec.decreasing?([3, 2, 1, 1])',
      'let vec = import("vec");\nvec.decreasing?([3, 2, 1, 2])',
      'let vec = import("vec");\nvec.decreasing?([1])',
      'let vec = import("vec");\nvec.decreasing?([])',
    ],
  },
  'vec.strictly-increasing?': {
    title: 'vec.strictly-increasing?',
    category: 'Vector',
    description: 'Checks if a vector is strictly increasing.',
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
      'let vec = import("vec");\nvec.strictly-increasing?([1, 2, 3])',
      'let vec = import("vec");\nvec.strictly-increasing?([1, 2, 2, 3])',
      'let vec = import("vec");\nvec.strictly-increasing?([3, 2, 1])',
      'let vec = import("vec");\nvec.strictly-increasing?([3, 2, 1, 1])',
      'let vec = import("vec");\nvec.strictly-increasing?([3, 2, 1, 2])',
      'let vec = import("vec");\nvec.strictly-increasing?([1])',
      'let vec = import("vec");\nvec.strictly-increasing?([])',
    ],
  },
  'vec.strictly-decreasing?': {
    title: 'vec.strictly-decreasing?',
    category: 'Vector',
    description: 'Checks if a vector is strictly decreasing.',
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
      'let vec = import("vec");\nvec.strictly-decreasing?([1, 2, 3])',
      'let vec = import("vec");\nvec.strictly-decreasing?([1, 2, 2, 3])',
      'let vec = import("vec");\nvec.strictly-decreasing?([3, 2, 1])',
      'let vec = import("vec");\nvec.strictly-decreasing?([3, 2, 1, 1])',
      'let vec = import("vec");\nvec.strictly-decreasing?([3, 2, 1, 2])',
      'let vec = import("vec");\nvec.strictly-decreasing?([1])',
      'let vec = import("vec");\nvec.strictly-decreasing?([])',
    ],
  },
  'vec.sum': {
    title: 'vec.sum',
    category: 'Vector',
    description: 'Returns the sum of all elements in the vector.',
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
      'let vec = import("vec");\nvec.sum([1, 2, 3])',
      'let vec = import("vec");\nvec.sum([1, 2, -3])',
    ],
  },
  'vec.median': {
    title: 'vec.median',
    category: 'Vector',
    description: 'Returns the median of all elements in the vector.',
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
      'let vec = import("vec");\nvec.median([1, 2, 3])',
      'let vec = import("vec");\nvec.median([1, 2, -3])',
      'let vec = import("vec");\nvec.median([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.median([1, 2, -3, 4])',
    ],
  },
  'vec.mode': {
    title: 'vec.mode',
    category: 'Vector',
    description: 'Returns the mode of all elements in the vector.',
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
      'let vec = import("vec");\nvec.mode([1, 2, 3])',
      'let vec = import("vec");\nvec.mode([1, 2, -3, 1])',
      'let vec = import("vec");\nvec.mode([2, 2, 3, 3, 4])',
      'let vec = import("vec");\nvec.mode([2, 2, 3, 3])',
      'let vec = import("vec");\nvec.mode([1, 2, 3, 2, 1, 2])',
    ],
  },
  'vec.sample-variance': {
    title: 'vec.sample-variance',
    category: 'Vector',
    description: 'Returns the sample variance of all elements in the vector.',
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
      'let vec = import("vec");\nvec.sample-variance([1, 2, 3])',
      'let vec = import("vec");\nvec.sample-variance([1, 2, -3])',
      'let vec = import("vec");\nvec.sample-variance([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.sample-variance([1, 2, -3, 4])',
      'let vec = import("vec");\nvec.sample-variance([1, 2, 3, 40, 50])',
    ],
  },
  'vec.stdev': {
    title: 'vec.stdev',
    category: 'Vector',
    description: 'Returns the standard deviation of all elements in the vector.',
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
      'let vec = import("vec");\nvec.stdev([1, 2, 3])',
      'let vec = import("vec");\nvec.stdev([1, 2, -3])',
      'let vec = import("vec");\nvec.stdev([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.stdev([1, 2, -3, 4])',
      'let vec = import("vec");\nvec.stdev([1, 2, 3, 40, 50])',
    ],
  },
  'vec.sample-stdev': {
    title: 'vec.sample-stdev',
    category: 'Vector',
    description: 'Returns the sample standard deviation of all elements in the vector.',
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
      'let vec = import("vec");\nvec.sample-stdev([1, 2, 3])',
      'let vec = import("vec");\nvec.sample-stdev([1, 2, -3])',
      'let vec = import("vec");\nvec.sample-stdev([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.sample-stdev([1, 2, -3, 4])',
      'let vec = import("vec");\nvec.sample-stdev([1, 2, 3, 40, 50])',
    ],
  },
  'vec.min': {
    title: 'vec.min',
    category: 'Vector',
    description: 'Returns the minimum value of all elements in the vector.',
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
      'let vec = import("vec");\nvec.min([1, 2, 3])',
      'let vec = import("vec");\nvec.min([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.min([1, 2, -3])',
      'let vec = import("vec");\nvec.min([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.min([1, 2, -3, 4])',
    ],
  },
  'vec.max': {
    title: 'vec.max',
    category: 'Vector',
    description: 'Returns the maximum value of all elements in the vector.',
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
      'let vec = import("vec");\nvec.max([1, 2, 3])',
      'let vec = import("vec");\nvec.max([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.max([1, 2, -3])',
      'let vec = import("vec");\nvec.max([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.max([1, 2, -3, 4])',
    ],
  },
  'vec.min-index': {
    title: 'vec.min-index',
    category: 'Vector',
    description: 'Returns the index of the minimum value of all elements in the vector.',
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
      'let vec = import("vec");\nvec.min-index([1, 2, 3])',
      'let vec = import("vec");\nvec.min-index([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.min-index([1, 2, -3])',
      'let vec = import("vec");\nvec.min-index([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.min-index([1, 2, -3, 4])',
    ],
  },
  'vec.max-index': {
    title: 'vec.max-index',
    category: 'Vector',
    description: 'Returns the index of the maximum value of all elements in the vector.',
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
      'let vec = import("vec");\nvec.max-index([1, 2, 3])',
      'let vec = import("vec");\nvec.max-index([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.max-index([1, 2, -3])',
      'let vec = import("vec");\nvec.max-index([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.max-index([1, 2, -3, 4])',
    ],
  },
  'vec.sort-indices': {
    title: 'vec.sort-indices',
    category: 'Vector',
    description: 'Returns the indices of the elements in the vector sorted in ascending order.',
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
      'let vec = import("vec");\nvec.sort-indices([1, 2, 3])',
      'let vec = import("vec");\nvec.sort-indices([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.sort-indices([1, 2, -3])',
      'let vec = import("vec");\nvec.sort-indices([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.sort-indices([1, 2, -3, 4])',
    ],
  },
  'vec.count-values': {
    title: 'vec.count-values',
    category: 'Vector',
    description: 'Counts the number of occurrences of each value in the vector.',
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
      'let vec = import("vec");\nvec.count-values([1, 2, 3])',
      'let vec = import("vec");\nvec.count-values([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.count-values([1, 2, -3])',
      'let vec = import("vec");\nvec.count-values([1, 2, 2, 1, 3, 2, 4, 2, 1, 2, 2, 1, 3, 2, 4])',
    ],
  },
  'vec.linspace': {
    title: 'vec.linspace',
    category: 'Vector',
    description: 'Generates a vector of evenly spaced numbers between two values.',
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
      'let vec = import("vec");\nvec.linspace(0, 10, 6)',
      'let vec = import("vec");\nvec.linspace(10, 20, 25)',
    ],
  },
  'vec.ones': {
    title: 'vec.ones',
    category: 'Vector',
    description: 'Generates a vector of ones.',
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
      'let vec = import("vec");\nvec.ones(5)',
      'let vec = import("vec");\nvec.ones(10)',
      'let vec = import("vec");\nvec.ones(0)',
    ],
  },
  'vec.zeros': {
    title: 'vec.zeros',
    category: 'Vector',
    description: 'Generates a vector of zeros.',
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
      'let vec = import("vec");\nvec.zeros(5)',
      'let vec = import("vec");\nvec.zeros(10)',
      'let vec = import("vec");\nvec.zeros(0)',
    ],
  },
  'vec.fill': {
    title: 'vec.fill',
    category: 'Vector',
    description: 'Generates a vector filled with a number.',
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
      'let vec = import("vec");\nvec.fill(5, PI)',
      'let vec = import("vec");\nvec.fill(10, -1)',
    ],
  },
  'vec.generate': {
    title: 'vec.generate',
    category: 'Vector',
    description: 'Generates a vector of numbers based on a function.',
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
      'let vec = import("vec");\nvec.generate(5, -> $ * 2)',
      'let vec = import("vec");\nvec.generate(10, -> $ + 1)',
      'let vec = import("vec");\nvec.generate(0, -> $ + 1)',
    ],
  },
  'vec.cumsum': {
    title: 'vec.cumsum',
    category: 'Vector',
    description: 'Calculates the cumulative sum of a vector.',
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
      'let vec = import("vec");\nvec.cumsum([1, 2, 3])',
      'let vec = import("vec");\nvec.cumsum([1, 2, -3])',
      'let vec = import("vec");\nvec.cumsum([])',
    ],
  },
  'vec.cumprod': {
    title: 'vec.cumprod',
    category: 'Vector',
    description: 'Calculates the cumulative product of a vector.',
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
      'let vec = import("vec");\nvec.cumprod([1, 2, 3])',
      'let vec = import("vec");\nvec.cumprod([1, 2, -3, 0, 10])',
      'let vec = import("vec");\nvec.cumprod([])',
    ],
  },
  'vec.quartiles': {
    title: 'vec.quartiles',
    category: 'Vector',
    description: 'Calculates the quartiles of a vector. Returns an array containing the first, second (median), and third quartiles.',
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
      'let vec = import("vec");\nvec.quartiles([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.quartiles([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let vec = import("vec");\nvec.quartiles(range(1, 1000))',
      'let vec = import("vec");\nvec.quartiles(vec.generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let vec = import("vec");\nvec.quartiles(vec.generate(1000, -> ln($ + 1)))',
    ],
  },
  'vec.percentile': {
    title: 'vec.percentile',
    category: 'Vector',
    description: 'Calculates the percentile of a vector. Returns the value at the specified percentile.',
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
      'let vec = import("vec");\nvec.percentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 0)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 10)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 20)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 30)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 40)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 50)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 60)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 70)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 80)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 90)',
      'let vec = import("vec");\nvec.percentile(range(100) ^ 0.5, 100)',
    ],
  },
  'vec.quantile': {
    title: 'vec.quantile',
    category: 'Vector',
    description: 'Calculates the quantile of a vector. Returns the value at the specified quantile.',
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
      'let vec = import("vec");\nvec.quantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.1)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.2)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.3)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.4)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.5)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.6)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.7)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.8)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 0.9)',
      'let vec = import("vec");\nvec.quantile(range(100) ^ 0.5, 1)',
    ],
  },
  'vec.span': {
    title: 'vec.span',
    category: 'Vector',
    description: 'Returns the difference between the maximum and minimum values in a vector.',
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
      'let vec = import("vec");\nvec.span([1, 2, 3])',
      'let vec = import("vec");\nvec.span([1, 1, 2, 3, 3])',
      'let vec = import("vec");\nvec.span([1, 2, -3])',
    ],
  },
  'vec.histogram': {
    title: 'vec.histogram',
    category: 'Vector',
    description: 'Creates a histogram from a numeric `array` by dividing the data range into the specified number of bins. Returns an `array` of `[binStart, binEnd, count]` tuples representing each bin\'s range and the number of values within it. Handles empty arrays, identical values, and properly places maximum values in the last bin.',
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
      'let vec = import("vec");\nvec.histogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'let vec = import("vec");\nvec.histogram([1, 2, 3, 4, 5], 5)',
      'let vec = import("vec");\nvec.histogram([1, 2, 3, 4, 5], 10)',
      'let vec = import("vec");\nvec.histogram([1, 2, 3, 4, 5], 1)',
    ],
  },
  'vec.ecdf': {
    title: 'vec.ecdf',
    category: 'Vector',
    description: 'Calculates the empirical cumulative distribution function value for a given threshold in a non empty dataset. Returns the proportion of values in the `array` that are less than or equal to the specified threshold.',
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
      'let vec = import("vec");\nvec.ecdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)',
      'let vec = import("vec");\nvec.ecdf([1, 2, 3, 4, 5], 3)',
      'let vec = import("vec");\nvec.ecdf([1, 2, 3, 4, 5], 0)',
      'let vec = import("vec");\nvec.ecdf([1, 2, 3, 4, 5], 10)',
      'let vec = import("vec");\nvec.ecdf([1, 2, 3, 4, 5], 2)',
    ],
  },
  'vec.outliers?': {
    title: 'vec.outliers?',
    category: 'Vector',
    description: 'Checks if the `vector` contains outliers based on the interquartile range (IQR) method. Returns `true` if outliers are present, `false` otherwise.',
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
      'let vec = import("vec");\nvec.outliers?([1, 2, 3])',
      'let vec = import("vec");\nvec.outliers?([1, 2, -3])',
      'let vec = import("vec");\nvec.outliers?([1, 2, 3, 2, 4, 120])',
    ],
  },
  'vec.outliers': {
    title: 'vec.outliers',
    category: 'Vector',
    description: 'Identifies outliers in the `vector` based on the interquartile range (IQR) method. Returns an array of outlier values.',
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
      'let vec = import("vec");\nvec.outliers([1, 2, 3])',
      'let vec = import("vec");\nvec.outliers([1, 2, -3])',
      'let vec = import("vec");\nvec.outliers([1, 2, 3, 2, 4, 120])',
    ],
  },
  'vec.bincount': {
    title: 'vec.bincount',
    category: 'Vector',
    description: 'counts occurrences of each `integer` in a vector, returning an array where index `i` contains the count of value `i`, with optional **minimum size** and **weights parameters**.',
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
      'let vec = import("vec");\nvec.bincount([1, 2, 3])',
      'let vec = import("vec");\nvec.bincount([1, 2, 2, 3, 3])',
    ],
    noOperatorDocumentation: true,
  },
  'vec.winsorize': {
    title: 'vec.winsorize',
    category: 'Vector',
    description: 'Limits extreme values in a `vector` by replacing values below the **lower quantile** and above the **upper quantile** with the values at those quantiles. The function takes a `vector` of values and **quantile thresholds** (between 0 and 1), with the upper quantile. Winsorization reduces the influence of outliers while preserving the overall distribution shape, making statistical analyses more robust.',
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
      'let vec = import("vec");\nvec.winsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25)',
      'let vec = import("vec");\nvec.winsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.75)',
      'let vec = import("vec");\nvec.winsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.5)',
    ],
    noOperatorDocumentation: true,
  },
  'vec.mse': {
    title: 'vec.mse',
    category: 'Vector',
    description: 'Calculates the **Mean Squared Error (MSE)** between two vectors. Returns the average of the squared differences between corresponding elements.',
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
      'let vec = import("vec");\nvec.mse([1, 2, 3], [1, 2, 3])',
      'let vec = import("vec");\nvec.mse([1, 2, 3], [4, 5, 6])',
      'let vec = import("vec");\nvec.mse([1, 2, 3], [2, 2, 2])',
      'let vec = import("vec");\nvec.mse([1, 2], [3, 3])',
      'let vec = import("vec");\nvec.mse([1], [3])',
    ],
  },
  'vec.rmse': {
    title: 'vec.rmse',
    category: 'Vector',
    description: 'Calculates the **Root Mean Squared Error (RMSE)** between two vectors. Returns the square root of the average of the squared differences between corresponding elements.',
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
      'let vec = import("vec");\nvec.rmse([1, 2, 3], [1, 2, 3])',
      'let vec = import("vec");\nvec.rmse([1, 2, 3], [4, 5, 6])',
      'let vec = import("vec");\nvec.rmse([1, 2, 3], [2, 2, 2])',
      'let vec = import("vec");\nvec.rmse([1, 2], [3, 3])',
      'let vec = import("vec");\nvec.rmse([1], [3])',
    ],
  },
  'vec.mae': {
    title: 'vec.mae',
    category: 'Vector',
    description: 'Calculates the **Mean Absolute Error (MAE)** between two vectors. Returns the average of the absolute differences between corresponding elements.',
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
      'let vec = import("vec");\nvec.mae([1, 2, 3], [1, 2, 3])',
      'let vec = import("vec");\nvec.mae([1, 2, 3], [4, 5, 6])',
      'let vec = import("vec");\nvec.mae([1, 2, 3], [2, 2, 2])',
      'let vec = import("vec");\nvec.mae([1, 2], [3, 3])',
      'let vec = import("vec");\nvec.mae([1], [3])',
    ],
  },
  'vec.smape': {
    title: 'vec.smape',
    category: 'Vector',
    description: 'Calculates the **Symmetric Mean Absolute Percentage Error (SMAPE)** between two vectors. Returns the average of the absolute percentage differences between corresponding elements.',
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
      'let vec = import("vec");\nvec.smape([1, 2, 3], [1, 2, 3])',
      'let vec = import("vec");\nvec.smape([1, 2, 3], [4, 5, 6])',
      'let vec = import("vec");\nvec.smape([1, 2, 3], [2, 2, 2])',
      'let vec = import("vec");\nvec.smape([1, 2], [3, 3])',
      'let vec = import("vec");\nvec.smape([1], [3])',
    ],
  },
}
