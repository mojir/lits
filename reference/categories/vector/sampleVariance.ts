import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const sampleVarianceReference: VectorReductionReference<'sample-variance'> = {
  'vec:sample-variance': {
    title: 'vec:sample-variance',
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
      'vec:sample-variance([1, 2, 3])',
      'vec:sample-variance([1, 2, -3])',
    ],
  },
  'vec:moving-sample-variance': {
    title: 'vec:moving-sample-variance',
    category: 'Vector',
    description: 'Returns the `moving sample variance` of the `vector` with a given window size.',
    linkName: 'vec-colon-moving-sample-variance',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving sample variance` of.',
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
      'vec:moving-sample-variance([1, 2, 3, 4, 5], 3)',
      'vec:moving-sample-variance([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-sample-variance': {
    title: 'vec:centered-moving-sample-variance',
    category: 'Vector',
    description: 'Returns the `centered moving sample variance` of the `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-sample-variance',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving sample variance` of.',
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
      '[1, 2, 3, 4, 5] vec:centered-moving-sample-variance 3',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3, 0, 6)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-sample-variance': {
    title: 'vec:running-sample-variance',
    category: 'Vector',
    description: 'Returns the `running sample variance` of the `vector`.',
    linkName: 'vec-colon-running-sample-variance',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running sample variance` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-sample-variance([1, 2, 3, 4, 5])',
    ],
  },
}
