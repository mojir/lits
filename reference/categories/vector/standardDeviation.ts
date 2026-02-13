import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const standardDeviationReference: VectorReductionReference<'stdev'> = {
  'Vector.stdev': {
    title: 'Vector.stdev',
    category: 'Vector',
    description: 'Returns the **standard deviation** of the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **standard deviation** of.',
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
  'Vector.moving-stdev': {
    title: 'Vector.moving-stdev',
    category: 'Vector',
    description: 'Returns the **moving standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving standard deviation** of.',
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
      'let { moving-stdev } = import("Vector");\nmoving-stdev([1, 2, 3, 4, 5], 3)',
      'let { moving-stdev } = import("Vector");\nmoving-stdev([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-stdev': {
    title: 'Vector.centered-moving-stdev',
    category: 'Vector',
    description: 'Returns the **centered moving standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving standard deviation** of.',
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
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'Vector.running-stdev': {
    title: 'Vector.running-stdev',
    category: 'Vector',
    description: 'Returns the **running standard deviation** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running standard deviation** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-stdev } = import("Vector");\nrunning-stdev([1, 2, 3, 4, 5])',
    ],
  },
}

export const sampleStandardDeviationReference: VectorReductionReference<'sample-stdev'> = {
  'Vector.sample-stdev': {
    title: 'Vector.sample-stdev',
    category: 'Vector',
    description: 'Returns the **sample standard deviation** of the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample standard deviation** of.',
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
  'Vector.moving-sample-stdev': {
    title: 'Vector.moving-sample-stdev',
    category: 'Vector',
    description: 'Returns the **moving sample standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample standard deviation** of.',
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
      'let { moving-sample-stdev } = import("Vector");\nmoving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'let { moving-sample-stdev } = import("Vector");\nmoving-sample-stdev([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-sample-stdev': {
    title: 'Vector.centered-moving-sample-stdev',
    category: 'Vector',
    description: 'Returns the **centered moving sample standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample standard deviation** of.',
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
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'Vector.running-sample-stdev': {
    title: 'Vector.running-sample-stdev',
    category: 'Vector',
    description: 'Returns the **running sample standard deviation** of the `vector`.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample standard deviation** of. First element in result is `null` since **sample standard deviation** is not defined for a single element.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-sample-stdev } = import("Vector");\nrunning-sample-stdev([1, 2, 3, 4, 5])',
    ],
  },
}
