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

type VectorReductionKey<T extends string> = `Vector.${T}`
type VectorMovingWindowKey<T extends string> = `Vector.moving-${T}`
type VectorCenteredMovingWindowKey<T extends string> = `Vector.centered-moving-${T}`
type VectorRunningKey<T extends string> = `Vector.running-${T}`
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
  'Vector.monotonic?': {
    title: 'Vector.monotonic?',
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
      'let { monotonic? } = import("Vector");\nmonotonic?([1, 2, 3])',
      'let { monotonic? } = import("Vector");\nmonotonic?([1, 2, 2, 3])',
      'let { monotonic? } = import("Vector");\nmonotonic?([3, 2, 1])',
      'let { monotonic? } = import("Vector");\nmonotonic?([3, 2, 1, 1])',
      'let { monotonic? } = import("Vector");\nmonotonic?([3, 2, 1, 2])',
      'let { monotonic? } = import("Vector");\nmonotonic?([1])',
      'let { monotonic? } = import("Vector");\nmonotonic?([])',
    ],
  },
  'Vector.strictly-monotonic?': {
    title: 'Vector.strictly-monotonic?',
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
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([1, 2, 3])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([1, 2, 2, 3])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([3, 2, 1])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([3, 2, 1, 1])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([3, 2, 1, 2])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([1])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([])',
    ],
  },
  'Vector.increasing?': {
    title: 'Vector.increasing?',
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
      'let { increasing? } = import("Vector");\nincreasing?([1, 2, 3])',
      'let { increasing? } = import("Vector");\nincreasing?([1, 2, 2, 3])',
      'let { increasing? } = import("Vector");\nincreasing?([3, 2, 1])',
      'let { increasing? } = import("Vector");\nincreasing?([3, 2, 1, 1])',
      'let { increasing? } = import("Vector");\nincreasing?([3, 2, 1, 2])',
      'let { increasing? } = import("Vector");\nincreasing?([1])',
      'let { increasing? } = import("Vector");\nincreasing?([])',
    ],
  },
  'Vector.decreasing?': {
    title: 'Vector.decreasing?',
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
      'let { decreasing? } = import("Vector");\ndecreasing?([1, 2, 3])',
      'let { decreasing? } = import("Vector");\ndecreasing?([1, 2, 2, 3])',
      'let { decreasing? } = import("Vector");\ndecreasing?([3, 2, 1])',
      'let { decreasing? } = import("Vector");\ndecreasing?([3, 2, 1, 1])',
      'let { decreasing? } = import("Vector");\ndecreasing?([3, 2, 1, 2])',
      'let { decreasing? } = import("Vector");\ndecreasing?([1])',
      'let { decreasing? } = import("Vector");\ndecreasing?([])',
    ],
  },
  'Vector.strictly-increasing?': {
    title: 'Vector.strictly-increasing?',
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
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([1, 2, 3])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([1, 2, 2, 3])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([3, 2, 1])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([3, 2, 1, 1])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([3, 2, 1, 2])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([1])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([])',
    ],
  },
  'Vector.strictly-decreasing?': {
    title: 'Vector.strictly-decreasing?',
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
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([1, 2, 3])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([1, 2, 2, 3])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([3, 2, 1])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([3, 2, 1, 1])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([3, 2, 1, 2])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([1])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([])',
    ],
  },
  'Vector.sum': {
    title: 'Vector.sum',
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
      'let { sum } = import("Vector");\nsum([1, 2, 3])',
      'let { sum } = import("Vector");\nsum([1, 2, -3])',
    ],
  },
  'Vector.median': {
    title: 'Vector.median',
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
      'let { median } = import("Vector");\nmedian([1, 2, 3])',
      'let { median } = import("Vector");\nmedian([1, 2, -3])',
      'let { median } = import("Vector");\nmedian([1, 2, 3, 4])',
      'let { median } = import("Vector");\nmedian([1, 2, -3, 4])',
    ],
  },
  'Vector.mode': {
    title: 'Vector.mode',
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
      'let { mode } = import("Vector");\nmode([1, 2, 3])',
      'let { mode } = import("Vector");\nmode([1, 2, -3, 1])',
      'let { mode } = import("Vector");\nmode([2, 2, 3, 3, 4])',
      'let { mode } = import("Vector");\nmode([2, 2, 3, 3])',
      'let { mode } = import("Vector");\nmode([1, 2, 3, 2, 1, 2])',
    ],
  },
  'Vector.sample-variance': {
    title: 'Vector.sample-variance',
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
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, -3])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3, 4])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, -3, 4])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3, 40, 50])',
    ],
  },
  'Vector.stdev': {
    title: 'Vector.stdev',
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
      'let { stdev } = import("Vector");\nstdev([1, 2, 3])',
      'let { stdev } = import("Vector");\nstdev([1, 2, -3])',
      'let { stdev } = import("Vector");\nstdev([1, 2, 3, 4])',
      'let { stdev } = import("Vector");\nstdev([1, 2, -3, 4])',
      'let { stdev } = import("Vector");\nstdev([1, 2, 3, 40, 50])',
    ],
  },
  'Vector.sample-stdev': {
    title: 'Vector.sample-stdev',
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
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, 3])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, -3])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, 3, 4])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, -3, 4])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, 3, 40, 50])',
    ],
  },
  'Vector.min': {
    title: 'Vector.min',
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
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, 3])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 1, 2, 3, 3])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, -3])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, 3, 4])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, -3, 4])',
    ],
  },
  'Vector.max': {
    title: 'Vector.max',
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
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, 3])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 1, 2, 3, 3])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, -3])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, 3, 4])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, -3, 4])',
    ],
  },
  'Vector.min-index': {
    title: 'Vector.min-index',
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
      'let { min-index } = import("Vector");\nmin-index([1, 2, 3])',
      'let { min-index } = import("Vector");\nmin-index([1, 1, 2, 3, 3])',
      'let { min-index } = import("Vector");\nmin-index([1, 2, -3])',
      'let { min-index } = import("Vector");\nmin-index([1, 2, 3, 4])',
      'let { min-index } = import("Vector");\nmin-index([1, 2, -3, 4])',
    ],
  },
  'Vector.max-index': {
    title: 'Vector.max-index',
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
      'let { max-index } = import("Vector");\nmax-index([1, 2, 3])',
      'let { max-index } = import("Vector");\nmax-index([1, 1, 2, 3, 3])',
      'let { max-index } = import("Vector");\nmax-index([1, 2, -3])',
      'let { max-index } = import("Vector");\nmax-index([1, 2, 3, 4])',
      'let { max-index } = import("Vector");\nmax-index([1, 2, -3, 4])',
    ],
  },
  'Vector.sort-indices': {
    title: 'Vector.sort-indices',
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
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, 3])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 1, 2, 3, 3])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, -3])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, 3, 4])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, -3, 4])',
    ],
  },
  'Vector.count-values': {
    title: 'Vector.count-values',
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
      'let { count-values } = import("Vector");\ncount-values([1, 2, 3])',
      'let { count-values } = import("Vector");\ncount-values([1, 1, 2, 3, 3])',
      'let { count-values } = import("Vector");\ncount-values([1, 2, -3])',
      'let { count-values } = import("Vector");\ncount-values([1, 2, 2, 1, 3, 2, 4, 2, 1, 2, 2, 1, 3, 2, 4])',
    ],
  },
  'Vector.linspace': {
    title: 'Vector.linspace',
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
      'let { linspace } = import("Vector");\nlinspace(0, 10, 6)',
      'let { linspace } = import("Vector");\nlinspace(10, 20, 25)',
    ],
  },
  'Vector.ones': {
    title: 'Vector.ones',
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
      'let { ones } = import("Vector");\nones(5)',
      'let { ones } = import("Vector");\nones(10)',
      'let { ones } = import("Vector");\nones(0)',
    ],
  },
  'Vector.zeros': {
    title: 'Vector.zeros',
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
      'let { zeros } = import("Vector");\nzeros(5)',
      'let { zeros } = import("Vector");\nzeros(10)',
      'let { zeros } = import("Vector");\nzeros(0)',
    ],
  },
  'Vector.fill': {
    title: 'Vector.fill',
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
      'let { fill } = import("Vector");\nfill(5, PI)',
      'let { fill } = import("Vector");\nfill(10, -1)',
    ],
  },
  'Vector.generate': {
    title: 'Vector.generate',
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
      'let { generate } = import("Vector");\ngenerate(5, -> $ * 2)',
      'let { generate } = import("Vector");\ngenerate(10, -> $ + 1)',
      'let { generate } = import("Vector");\ngenerate(0, -> $ + 1)',
    ],
  },
  'Vector.cumsum': {
    title: 'Vector.cumsum',
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
      'let { cumsum } = import("Vector");\ncumsum([1, 2, 3])',
      'let { cumsum } = import("Vector");\ncumsum([1, 2, -3])',
      'let { cumsum } = import("Vector");\ncumsum([])',
    ],
  },
  'Vector.cumprod': {
    title: 'Vector.cumprod',
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
      'let { cumprod } = import("Vector");\ncumprod([1, 2, 3])',
      'let { cumprod } = import("Vector");\ncumprod([1, 2, -3, 0, 10])',
      'let { cumprod } = import("Vector");\ncumprod([])',
    ],
  },
  'Vector.quartiles': {
    title: 'Vector.quartiles',
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
      'let { quartiles } = import("Vector");\nquartiles([1, 2, 3, 4])',
      'let { quartiles } = import("Vector");\nquartiles([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { quartiles } = import("Vector");\nquartiles(range(1, 1000))',
      'let { quartiles, generate } = import("Vector");\nquartiles(generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let { quartiles, generate } = import("Vector");\nquartiles(generate(1000, -> ln($ + 1)))',
    ],
  },
  'Vector.percentile': {
    title: 'Vector.percentile',
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
      'let { percentile } = import("Vector");\npercentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 0)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 10)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 20)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 30)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 40)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 50)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 60)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 70)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 80)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 90)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 100)',
    ],
  },
  'Vector.quantile': {
    title: 'Vector.quantile',
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
      'let { quantile } = import("Vector");\nquantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.1)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.2)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.3)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.4)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.5)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.6)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.7)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.8)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.9)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 1)',
    ],
  },
  'Vector.span': {
    title: 'Vector.span',
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
      'let { span } = import("Vector");\nspan([1, 2, 3])',
      'let { span } = import("Vector");\nspan([1, 1, 2, 3, 3])',
      'let { span } = import("Vector");\nspan([1, 2, -3])',
    ],
  },
  'Vector.histogram': {
    title: 'Vector.histogram',
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
      'let { histogram } = import("Vector");\nhistogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'let { histogram } = import("Vector");\nhistogram([1, 2, 3, 4, 5], 5)',
      'let { histogram } = import("Vector");\nhistogram([1, 2, 3, 4, 5], 10)',
      'let { histogram } = import("Vector");\nhistogram([1, 2, 3, 4, 5], 1)',
    ],
  },
  'Vector.ecdf': {
    title: 'Vector.ecdf',
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
      'let { ecdf } = import("Vector");\necdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 3)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 0)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 10)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 2)',
    ],
  },
  'Vector.outliers?': {
    title: 'Vector.outliers?',
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
      'let { outliers? } = import("Vector");\noutliers?([1, 2, 3])',
      'let { outliers? } = import("Vector");\noutliers?([1, 2, -3])',
      'let { outliers? } = import("Vector");\noutliers?([1, 2, 3, 2, 4, 120])',
    ],
  },
  'Vector.outliers': {
    title: 'Vector.outliers',
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
      'let { outliers } = import("Vector");\noutliers([1, 2, 3])',
      'let { outliers } = import("Vector");\noutliers([1, 2, -3])',
      'let { outliers } = import("Vector");\noutliers([1, 2, 3, 2, 4, 120])',
    ],
  },
  'Vector.bincount': {
    title: 'Vector.bincount',
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
      'let { bincount } = import("Vector");\nbincount([1, 2, 3])',
      'let { bincount } = import("Vector");\nbincount([1, 2, 2, 3, 3])',
    ],
    noOperatorDocumentation: true,
  },
  'Vector.winsorize': {
    title: 'Vector.winsorize',
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
      'let { winsorize } = import("Vector");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25)',
      'let { winsorize } = import("Vector");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.75)',
      'let { winsorize } = import("Vector");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.5)',
    ],
    noOperatorDocumentation: true,
  },
  'Vector.mse': {
    title: 'Vector.mse',
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
      'let { mse } = import("Vector");\nmse([1, 2, 3], [1, 2, 3])',
      'let { mse } = import("Vector");\nmse([1, 2, 3], [4, 5, 6])',
      'let { mse } = import("Vector");\nmse([1, 2, 3], [2, 2, 2])',
      'let { mse } = import("Vector");\nmse([1, 2], [3, 3])',
      'let { mse } = import("Vector");\nmse([1], [3])',
    ],
  },
  'Vector.rmse': {
    title: 'Vector.rmse',
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
      'let { rmse } = import("Vector");\nrmse([1, 2, 3], [1, 2, 3])',
      'let { rmse } = import("Vector");\nrmse([1, 2, 3], [4, 5, 6])',
      'let { rmse } = import("Vector");\nrmse([1, 2, 3], [2, 2, 2])',
      'let { rmse } = import("Vector");\nrmse([1, 2], [3, 3])',
      'let { rmse } = import("Vector");\nrmse([1], [3])',
    ],
  },
  'Vector.mae': {
    title: 'Vector.mae',
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
      'let { mae } = import("Vector");\nmae([1, 2, 3], [1, 2, 3])',
      'let { mae } = import("Vector");\nmae([1, 2, 3], [4, 5, 6])',
      'let { mae } = import("Vector");\nmae([1, 2, 3], [2, 2, 2])',
      'let { mae } = import("Vector");\nmae([1, 2], [3, 3])',
      'let { mae } = import("Vector");\nmae([1], [3])',
    ],
  },
  'Vector.smape': {
    title: 'Vector.smape',
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
      'let { smape } = import("Vector");\nsmape([1, 2, 3], [1, 2, 3])',
      'let { smape } = import("Vector");\nsmape([1, 2, 3], [4, 5, 6])',
      'let { smape } = import("Vector");\nsmape([1, 2, 3], [2, 2, 2])',
      'let { smape } = import("Vector");\nsmape([1, 2], [3, 3])',
      'let { smape } = import("Vector");\nsmape([1], [3])',
    ],
  },
}
