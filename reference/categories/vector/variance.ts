import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const varianceReference: VectorReductionReference<'variance'> = {
  'vec:variance': {
    title: 'vec:variance',
    category: 'Vector',
    description: 'Returns the `variance` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `variance` of.',
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
    description: 'Returns the **moving variance` of the `vector** with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving variance** of.',
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
      'vec:moving-variance([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-variance': {
    title: 'vec:centered-moving-variance',
    category: 'Vector',
    description: 'Returns the **centered moving variance` of the `vector** with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving variance** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
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
      '[1, 2, 3, 4, 5] vec:centered-moving-variance 3',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3, 1)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'vec:centered-moving-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'vec:running-variance': {
    title: 'vec:running-variance',
    category: 'Vector',
    description: 'Returns the **running variance` of the `vector**.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running variance** of.',
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

export const sampleVarianceReference: VectorReductionReference<'sample-variance'> = {
  'vec:sample-variance': {
    title: 'vec:sample-variance',
    category: 'Vector',
    description: 'Returns the `variance` of all elements in the `vector`.',
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
    description: 'Returns the **moving sample variance` of the `vector** with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample variance** of.',
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
      'vec:moving-sample-variance([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-sample-variance': {
    title: 'vec:centered-moving-sample-variance',
    category: 'Vector',
    description: 'Returns the **centered moving sample variance` of the `vector** with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample variance** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
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
      '[1, 2, 3, 4, 5] vec:centered-moving-sample-variance 3',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'vec:centered-moving-sample-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'vec:running-sample-variance': {
    title: 'vec:running-sample-variance',
    category: 'Vector',
    description: 'Returns the **running sample variance` of the `vector**.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample variance` of. First element in result is `null` since `sample variance** is not defined for a single element.',
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
