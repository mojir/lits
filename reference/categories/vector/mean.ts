import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const meanReference: VectorReductionReference<'mean'> = {
  'vec:mean': {
    title: 'vec:mean',
    category: 'Vector',
    description: 'Returns the mean of all elements in the vector.',
    linkName: 'vec-colon-mean',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the mean of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:mean([1, 2, 3])',
      'vec:mean([1, 2, -3])',
    ],
  },
  'vec:moving-mean': {
    title: 'vec:moving-mean',
    category: 'Vector',
    description: 'Returns the moving mean of the vector with a given window size.',
    linkName: 'vec-colon-moving-mean',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the moving mean of.',
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
      'vec:moving-mean([1, 2, 3, 4, 5], 3)',
      'vec:moving-mean([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-mean': {
    title: 'vec:centered-moving-mean',
    category: 'Vector',
    description: 'Returns the centered moving mean of the vector with a given window size.',
    linkName: 'vec-colon-centered-moving-mean',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the centered moving mean of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      paddingValue: {
        type: 'number',
        description: 'Optional value to use for padding. Default is 0.',
      },
      rightPaddingValue: {
        type: 'number',
        description: 'The value to use for right padding. Default is the same as paddingValue.',
      },
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
      { argumentNames: ['vector', 'windowSize', 'paddingValue'] },
      { argumentNames: ['vector', 'windowSize', 'paddingValue', 'rightPaddingValue'] },
    ],
    examples: [
      'vec:centered-moving-mean([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-mean([1, 2, 3, 4, 5], 3, 10)',
      'vec:centered-moving-mean([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-mean': {
    title: 'vec:running-mean',
    category: 'Vector',
    description: 'Returns the running mean of the vector.',
    linkName: 'vec-colon-running-mean',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the running mean of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-mean([1, 2, 3, 4, 5])',
    ],
  },
}
