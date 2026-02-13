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
type VectorReductionKeys<T extends string, B extends string = T> = VectorReductionKey<T> | VectorMovingWindowKey<B> | VectorCenteredMovingWindowKey<B> | VectorRunningKey<B>

export type VectorReductionReference<T extends string, B extends string = T> = {
  [key in VectorReductionKeys<T, B>]: FunctionReference<'Vector'>
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
      'let { monotonic? } = import("vec");\nmonotonic?([1, 2, 3])',
      'let { monotonic? } = import("vec");\nmonotonic?([1, 2, 2, 3])',
      'let { monotonic? } = import("vec");\nmonotonic?([3, 2, 1])',
      'let { monotonic? } = import("vec");\nmonotonic?([3, 2, 1, 1])',
      'let { monotonic? } = import("vec");\nmonotonic?([3, 2, 1, 2])',
      'let { monotonic? } = import("vec");\nmonotonic?([1])',
      'let { monotonic? } = import("vec");\nmonotonic?([])',
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
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([1, 2, 3])',
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([1, 2, 2, 3])',
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([3, 2, 1])',
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([3, 2, 1, 1])',
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([3, 2, 1, 2])',
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([1])',
      'let { strictly-monotonic? } = import("vec");\nstrictly-monotonic?([])',
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
      'let { increasing? } = import("vec");\nincreasing?([1, 2, 3])',
      'let { increasing? } = import("vec");\nincreasing?([1, 2, 2, 3])',
      'let { increasing? } = import("vec");\nincreasing?([3, 2, 1])',
      'let { increasing? } = import("vec");\nincreasing?([3, 2, 1, 1])',
      'let { increasing? } = import("vec");\nincreasing?([3, 2, 1, 2])',
      'let { increasing? } = import("vec");\nincreasing?([1])',
      'let { increasing? } = import("vec");\nincreasing?([])',
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
      'let { decreasing? } = import("vec");\ndecreasing?([1, 2, 3])',
      'let { decreasing? } = import("vec");\ndecreasing?([1, 2, 2, 3])',
      'let { decreasing? } = import("vec");\ndecreasing?([3, 2, 1])',
      'let { decreasing? } = import("vec");\ndecreasing?([3, 2, 1, 1])',
      'let { decreasing? } = import("vec");\ndecreasing?([3, 2, 1, 2])',
      'let { decreasing? } = import("vec");\ndecreasing?([1])',
      'let { decreasing? } = import("vec");\ndecreasing?([])',
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
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([1, 2, 3])',
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([1, 2, 2, 3])',
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([3, 2, 1])',
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([3, 2, 1, 1])',
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([3, 2, 1, 2])',
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([1])',
      'let { strictly-increasing? } = import("vec");\nstrictly-increasing?([])',
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
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([1, 2, 3])',
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([1, 2, 2, 3])',
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([3, 2, 1])',
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([3, 2, 1, 1])',
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([3, 2, 1, 2])',
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([1])',
      'let { strictly-decreasing? } = import("vec");\nstrictly-decreasing?([])',
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
      'let { sum } = import("vec");\nsum([1, 2, 3])',
      'let { sum } = import("vec");\nsum([1, 2, -3])',
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
      'let { median } = import("vec");\nmedian([1, 2, 3])',
      'let { median } = import("vec");\nmedian([1, 2, -3])',
      'let { median } = import("vec");\nmedian([1, 2, 3, 4])',
      'let { median } = import("vec");\nmedian([1, 2, -3, 4])',
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
      'let { mode } = import("vec");\nmode([1, 2, 3])',
      'let { mode } = import("vec");\nmode([1, 2, -3, 1])',
      'let { mode } = import("vec");\nmode([2, 2, 3, 3, 4])',
      'let { mode } = import("vec");\nmode([2, 2, 3, 3])',
      'let { mode } = import("vec");\nmode([1, 2, 3, 2, 1, 2])',
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
      'let { sample-variance } = import("vec");\nsample-variance([1, 2, 3])',
      'let { sample-variance } = import("vec");\nsample-variance([1, 2, -3])',
      'let { sample-variance } = import("vec");\nsample-variance([1, 2, 3, 4])',
      'let { sample-variance } = import("vec");\nsample-variance([1, 2, -3, 4])',
      'let { sample-variance } = import("vec");\nsample-variance([1, 2, 3, 40, 50])',
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
      'let { stdev } = import("vec");\nstdev([1, 2, 3])',
      'let { stdev } = import("vec");\nstdev([1, 2, -3])',
      'let { stdev } = import("vec");\nstdev([1, 2, 3, 4])',
      'let { stdev } = import("vec");\nstdev([1, 2, -3, 4])',
      'let { stdev } = import("vec");\nstdev([1, 2, 3, 40, 50])',
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
      'let { sample-stdev } = import("vec");\nsample-stdev([1, 2, 3])',
      'let { sample-stdev } = import("vec");\nsample-stdev([1, 2, -3])',
      'let { sample-stdev } = import("vec");\nsample-stdev([1, 2, 3, 4])',
      'let { sample-stdev } = import("vec");\nsample-stdev([1, 2, -3, 4])',
      'let { sample-stdev } = import("vec");\nsample-stdev([1, 2, 3, 40, 50])',
    ],
  },
  'vec.TEMP-min': {
    title: 'vec.TEMP-min',
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
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 2, 3])',
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 1, 2, 3, 3])',
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 2, -3])',
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 2, 3, 4])',
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 2, -3, 4])',
    ],
  },
  'vec.TEMP-max': {
    title: 'vec.TEMP-max',
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
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 2, 3])',
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 1, 2, 3, 3])',
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 2, -3])',
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 2, 3, 4])',
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 2, -3, 4])',
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
      'let { min-index } = import("vec");\nmin-index([1, 2, 3])',
      'let { min-index } = import("vec");\nmin-index([1, 1, 2, 3, 3])',
      'let { min-index } = import("vec");\nmin-index([1, 2, -3])',
      'let { min-index } = import("vec");\nmin-index([1, 2, 3, 4])',
      'let { min-index } = import("vec");\nmin-index([1, 2, -3, 4])',
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
      'let { max-index } = import("vec");\nmax-index([1, 2, 3])',
      'let { max-index } = import("vec");\nmax-index([1, 1, 2, 3, 3])',
      'let { max-index } = import("vec");\nmax-index([1, 2, -3])',
      'let { max-index } = import("vec");\nmax-index([1, 2, 3, 4])',
      'let { max-index } = import("vec");\nmax-index([1, 2, -3, 4])',
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
      'let { sort-indices } = import("vec");\nsort-indices([1, 2, 3])',
      'let { sort-indices } = import("vec");\nsort-indices([1, 1, 2, 3, 3])',
      'let { sort-indices } = import("vec");\nsort-indices([1, 2, -3])',
      'let { sort-indices } = import("vec");\nsort-indices([1, 2, 3, 4])',
      'let { sort-indices } = import("vec");\nsort-indices([1, 2, -3, 4])',
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
      'let { count-values } = import("vec");\ncount-values([1, 2, 3])',
      'let { count-values } = import("vec");\ncount-values([1, 1, 2, 3, 3])',
      'let { count-values } = import("vec");\ncount-values([1, 2, -3])',
      'let { count-values } = import("vec");\ncount-values([1, 2, 2, 1, 3, 2, 4, 2, 1, 2, 2, 1, 3, 2, 4])',
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
      'let { linspace } = import("vec");\nlinspace(0, 10, 6)',
      'let { linspace } = import("vec");\nlinspace(10, 20, 25)',
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
      'let { ones } = import("vec");\nones(5)',
      'let { ones } = import("vec");\nones(10)',
      'let { ones } = import("vec");\nones(0)',
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
      'let { zeros } = import("vec");\nzeros(5)',
      'let { zeros } = import("vec");\nzeros(10)',
      'let { zeros } = import("vec");\nzeros(0)',
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
      'let { fill } = import("vec");\nfill(5, PI)',
      'let { fill } = import("vec");\nfill(10, -1)',
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
      'let { generate } = import("vec");\ngenerate(5, -> $ * 2)',
      'let { generate } = import("vec");\ngenerate(10, -> $ + 1)',
      'let { generate } = import("vec");\ngenerate(0, -> $ + 1)',
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
      'let { cumsum } = import("vec");\ncumsum([1, 2, 3])',
      'let { cumsum } = import("vec");\ncumsum([1, 2, -3])',
      'let { cumsum } = import("vec");\ncumsum([])',
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
      'let { cumprod } = import("vec");\ncumprod([1, 2, 3])',
      'let { cumprod } = import("vec");\ncumprod([1, 2, -3, 0, 10])',
      'let { cumprod } = import("vec");\ncumprod([])',
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
      'let { quartiles } = import("vec");\nquartiles([1, 2, 3, 4])',
      'let { quartiles } = import("vec");\nquartiles([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { quartiles } = import("vec");\nquartiles(range(1, 1000))',
      'let { quartiles } = import("vec");\nquartiles(vec.generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let { quartiles } = import("vec");\nquartiles(vec.generate(1000, -> ln($ + 1)))',
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
      'let { percentile } = import("vec");\npercentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 0)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 10)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 20)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 30)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 40)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 50)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 60)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 70)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 80)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 90)',
      'let { percentile } = import("vec");\npercentile(range(100) ^ 0.5, 100)',
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
      'let { quantile } = import("vec");\nquantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.1)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.2)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.3)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.4)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.5)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.6)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.7)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.8)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 0.9)',
      'let { quantile } = import("vec");\nquantile(range(100) ^ 0.5, 1)',
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
      'let { span } = import("vec");\nspan([1, 2, 3])',
      'let { span } = import("vec");\nspan([1, 1, 2, 3, 3])',
      'let { span } = import("vec");\nspan([1, 2, -3])',
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
      'let { histogram } = import("vec");\nhistogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'let { histogram } = import("vec");\nhistogram([1, 2, 3, 4, 5], 5)',
      'let { histogram } = import("vec");\nhistogram([1, 2, 3, 4, 5], 10)',
      'let { histogram } = import("vec");\nhistogram([1, 2, 3, 4, 5], 1)',
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
      'let { ecdf } = import("vec");\necdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)',
      'let { ecdf } = import("vec");\necdf([1, 2, 3, 4, 5], 3)',
      'let { ecdf } = import("vec");\necdf([1, 2, 3, 4, 5], 0)',
      'let { ecdf } = import("vec");\necdf([1, 2, 3, 4, 5], 10)',
      'let { ecdf } = import("vec");\necdf([1, 2, 3, 4, 5], 2)',
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
      'let { outliers? } = import("vec");\noutliers?([1, 2, 3])',
      'let { outliers? } = import("vec");\noutliers?([1, 2, -3])',
      'let { outliers? } = import("vec");\noutliers?([1, 2, 3, 2, 4, 120])',
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
      'let { outliers } = import("vec");\noutliers([1, 2, 3])',
      'let { outliers } = import("vec");\noutliers([1, 2, -3])',
      'let { outliers } = import("vec");\noutliers([1, 2, 3, 2, 4, 120])',
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
      'let { bincount } = import("vec");\nbincount([1, 2, 3])',
      'let { bincount } = import("vec");\nbincount([1, 2, 2, 3, 3])',
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
      'let { winsorize } = import("vec");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25)',
      'let { winsorize } = import("vec");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.75)',
      'let { winsorize } = import("vec");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.5)',
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
      'let { mse } = import("vec");\nmse([1, 2, 3], [1, 2, 3])',
      'let { mse } = import("vec");\nmse([1, 2, 3], [4, 5, 6])',
      'let { mse } = import("vec");\nmse([1, 2, 3], [2, 2, 2])',
      'let { mse } = import("vec");\nmse([1, 2], [3, 3])',
      'let { mse } = import("vec");\nmse([1], [3])',
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
      'let { rmse } = import("vec");\nrmse([1, 2, 3], [1, 2, 3])',
      'let { rmse } = import("vec");\nrmse([1, 2, 3], [4, 5, 6])',
      'let { rmse } = import("vec");\nrmse([1, 2, 3], [2, 2, 2])',
      'let { rmse } = import("vec");\nrmse([1, 2], [3, 3])',
      'let { rmse } = import("vec");\nrmse([1], [3])',
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
      'let { mae } = import("vec");\nmae([1, 2, 3], [1, 2, 3])',
      'let { mae } = import("vec");\nmae([1, 2, 3], [4, 5, 6])',
      'let { mae } = import("vec");\nmae([1, 2, 3], [2, 2, 2])',
      'let { mae } = import("vec");\nmae([1, 2], [3, 3])',
      'let { mae } = import("vec");\nmae([1], [3])',
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
      'let { smape } = import("vec");\nsmape([1, 2, 3], [1, 2, 3])',
      'let { smape } = import("vec");\nsmape([1, 2, 3], [4, 5, 6])',
      'let { smape } = import("vec");\nsmape([1, 2, 3], [2, 2, 2])',
      'let { smape } = import("vec");\nsmape([1, 2], [3, 3])',
      'let { smape } = import("vec");\nsmape([1], [3])',
    ],
  },
}
