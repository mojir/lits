import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const interquartileRangeReference: VectorReductionReference<'iqr'> = {
  'Vector.iqr': {
    title: 'Vector.iqr',
    category: 'Vector',
    description: 'Calculates the **interquartile range** of a `vector`. Returns the difference between the third and first quartiles.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **interquartile range** of. Minimum length is 4.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { iqr } = import("Vector");\niqr([1, 2, 3, 4])',
      'let { iqr } = import("Vector");\niqr([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { iqr } = import("Vector");\niqr(range(1, 1000))',
      'let { iqr, generate } = import("Vector");\niqr(generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let { iqr, generate } = import("Vector");\niqr(generate(1000, -> ln($ + 1)))',
    ],
  },

  'Vector.moving-iqr': {
    title: 'Vector.moving-iqr',
    category: 'Vector',
    description: 'Calculates the **moving interquartile range** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving interquartile range** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
    ],
    examples: [
      'let { moving-iqr } = import("Vector");\nmoving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-iqr } = import("Vector");\nmoving-iqr([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-iqr } = import("Vector");\nmoving-iqr([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'Vector.centered-moving-iqr': {
    title: 'Vector.centered-moving-iqr',
    category: 'Vector',
    description: 'Calculates the **centered moving interquartile range** of a `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving interquartile range** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
      { argumentNames: ['vector', 'windowSize', 'leftPadding'] },
      { argumentNames: ['vector', 'windowSize', 'leftPadding', 'rightPadding'] },
    ],
    examples: [
      'let { centered-moving-iqr } = import("Vector");\ncentered-moving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-iqr } = import("Vector");\ncentered-moving-iqr([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'Vector.running-iqr': {
    title: 'Vector.running-iqr',
    category: 'Vector',
    description: 'Calculates the **running interquartile range** of a `vector`. First three element in result is `null` since **running interquartile range** is not defined for less than four elements.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running interquartile range** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-iqr } = import("Vector");\nrunning-iqr([1, 2, 3, 4, 5, 6])',
      'let { running-iqr } = import("Vector");\nrunning-iqr([-1, -2, -3, 1, 2, 3])',
    ],
  },
}
