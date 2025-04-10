import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const prodReference: VectorReductionReference<'prod'> = {
  'vec:prod': {
    title: 'vec:prod',
    category: 'Vector',
    description: 'Returns the product of all elements in the vector.',
    linkName: 'vec-colon-prod',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the product of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:prod([1, 2, 3])',
      'vec:prod([1, 2, -3])',
    ],
  },
  'vec:moving-prod': {
    title: 'vec:moving-prod',
    category: 'Vector',
    description: 'Returns the moving product of the vector with a given window size.',
    linkName: 'vec-colon-moving-prod',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the moving product of.',
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
      'vec:moving-prod([1, 2, 3, 4, 5], 3)',
      'vec:moving-prod([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-prod': {
    title: 'vec:centered-moving-prod',
    category: 'Vector',
    description: 'Returns the centered moving product of the vector with a given window size.',
    linkName: 'vec-colon-centered-moving-prod',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the centered moving product of.',
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
      'vec:centered-moving-prod([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-prod([1, 2, 3, 4, 5], 3, 0)',
      'vec:centered-moving-prod([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-prod': {
    title: 'vec:running-prod',
    category: 'Vector',
    description: 'Returns the running product of the vector.',
    linkName: 'vec-colon-running-prod',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the running product of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-prod([1, 2, 3, 4, 5])',
      'vec:running-prod([1, -2, -3])',
    ],
  },
}
