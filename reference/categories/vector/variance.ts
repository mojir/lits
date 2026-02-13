import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const varianceReference: VectorReductionReference<'variance'> = {
  'Vector.variance': {
    title: 'Vector.variance',
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
      'let { variance } = import("Vector");\nvariance([1, 2, 3])',
      'let { variance } = import("Vector");\nvariance([1, 2, -3])',
    ],
  },
  'Vector.moving-variance': {
    title: 'Vector.moving-variance',
    category: 'Vector',
    description: 'Returns the **moving variance** of the `vector` with a given window size.',
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
      'let { moving-variance } = import("Vector");\nmoving-variance([1, 2, 3, 4, 5], 3)',
      'let { moving-variance } = import("Vector");\nmoving-variance([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-variance': {
    title: 'Vector.centered-moving-variance',
    category: 'Vector',
    description: 'Returns the **centered moving variance** of the `vector` with a given window size.',
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
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'Vector.running-variance': {
    title: 'Vector.running-variance',
    category: 'Vector',
    description: 'Returns the **running variance** of the `vector`.',
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
      'let { running-variance } = import("Vector");\nrunning-variance([1, 2, 3, 4, 5])',
    ],
  },
}

export const sampleVarianceReference: VectorReductionReference<'sample-variance'> = {
  'Vector.sample-variance': {
    title: 'Vector.sample-variance',
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
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, -3])',
    ],
  },
  'Vector.moving-sample-variance': {
    title: 'Vector.moving-sample-variance',
    category: 'Vector',
    description: 'Returns the **moving sample variance** of the `vector` with a given window size.',
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
      'let { moving-sample-variance } = import("Vector");\nmoving-sample-variance([1, 2, 3, 4, 5], 3)',
      'let { moving-sample-variance } = import("Vector");\nmoving-sample-variance([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-sample-variance': {
    title: 'Vector.centered-moving-sample-variance',
    category: 'Vector',
    description: 'Returns the **centered moving sample variance** of the `vector` with a given window size.',
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
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'Vector.running-sample-variance': {
    title: 'Vector.running-sample-variance',
    category: 'Vector',
    description: 'Returns the **running sample variance** of the `vector`.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample variance** of. First element in result is `null` since **sample variance** is not defined for a single element.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-sample-variance } = import("Vector");\nrunning-sample-variance([1, 2, 3, 4, 5])',
    ],
  },
}
