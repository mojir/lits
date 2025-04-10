import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const maxReference: VectorReductionReference<'max'> = {
  'vec:max': {
    title: 'vec:max',
    category: 'Vector',
    description: 'Returns the maximum of all elements in the vector.',
    linkName: 'vec-colon-max',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the maximum of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:max([1, 2, 3])',
      'vec:max([1, 2, -3])',
    ],
  },
  'vec:moving-max': {
    title: 'vec:moving-max',
    category: 'Vector',
    description: 'Returns the moving maximum of the vector with a given window size.',
    linkName: 'vec-colon-moving-max',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the moving maximum of.',
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
      'vec:moving-max([1, 2, 3, 4, 5], 3)',
      'vec:moving-max([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-max': {
    title: 'vec:centered-moving-max',
    category: 'Vector',
    description: 'Returns the centered moving maximum of the vector with a given window size.',
    linkName: 'vec-colon-centered-moving-max',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the centered moving maximum of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      paddingValue: {
        type: 'number',
        description: 'The value to use for padding. Default is 0.',
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
      'vec:centered-moving-max([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-max([1, 2, 3, 4, 5], 3, 0, 100)',
      'vec:centered-moving-max([1, 2, 3, 4, 5], 3, 0)',
      'vec:centered-moving-max([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-max': {
    title: 'vec:running-max',
    category: 'Vector',
    description: 'Returns the running maximum of the vector.',
    linkName: 'vec-colon-running-max',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the running maximum of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-max([1, 2, 3])',
      'vec:running-max([1, -2, -3])',
    ],
  },
}
