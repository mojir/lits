import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const interquartileRangeReference: VectorReductionReference<'iqr'> = {
  'vec:iqr': {
    title: 'vec:iqr',
    category: 'Vector',
    description: 'Calculates the **interquartile range** of a `vector`. Returns the difference between the third and first quartiles.',
    linkName: 'vec-colon-iqr',
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
      'vec:iqr([1, 2, 3, 4])',
      'vec:iqr([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'vec:iqr(range(1, 1000))',
      'vec:iqr(vec:generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'vec:iqr(vec:generate(1000, -> ln($ + 1)))',
    ],
  },

  'vec:moving-iqr': {
    title: 'vec:moving-iqr',
    category: 'Vector',
    description: 'Calculates the **moving interquartile range** of a `vector` with a given window size.',
    linkName: 'vec-colon-moving-iqr',
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
      'vec:moving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'vec:moving-iqr([1, 2, 4, 7, 11, 16], 5)',
      'vec:moving-iqr([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'vec:centered-moving-iqr': {
    title: 'vec:centered-moving-iqr',
    category: 'Vector',
    description: 'Calculates the **centered moving interquartile range** of a `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-iqr',
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
      'vec:centered-moving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'vec:centered-moving-iqr([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'vec:running-iqr': {
    title: 'vec:running-iqr',
    category: 'Vector',
    description: 'Calculates the **running interquartile range** of a `vector`. First three element in result is `null` since **running interquartile range** is not defined for less than four elements.',
    linkName: 'vec-colon-running-iqr',
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
      'vec:running-iqr([1, 2, 3, 4, 5, 6])',
      'vec:running-iqr([-1, -2, -3, 1, 2, 3])',
    ],
  },
}
