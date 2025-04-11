import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const varianceReference: VectorReductionReference<'variance'> = {
  'vec:variance': {
    title: 'vec:variance',
    category: 'Vector',
    description: 'Returns the `variance` of all elements in the `vector`.',
    linkName: 'vec-colon-variance',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the variance of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:variance([1, 2, 3])',
      'vec:variance([1, 2, -3])',
    ],
  },
  'vec:moving-variance': {
    title: 'vec:moving-variance',
    category: 'Vector',
    description: 'Returns the `moving variance` of the `vector` with a given window size.',
    linkName: 'vec-colon-moving-variance',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving variance` of.',
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
      'vec:moving-variance([1, 2, 3, 4, 5], 3)',
      'vec:moving-variance([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-variance': {
    title: 'vec:centered-moving-variance',
    category: 'Vector',
    description: 'Returns the `centered moving variance` of the `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-variance',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving variance` of.',
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
      '[1, 2, 3, 4, 5] vec:centered-moving-variance 3',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3, 1)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3, 0, 6)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-variance': {
    title: 'vec:running-variance',
    category: 'Vector',
    description: 'Returns the `running variance` of the `vector`.',
    linkName: 'vec-colon-running-variance',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running variance` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-variance([1, 2, 3, 4, 5])',
    ],
  },
}
