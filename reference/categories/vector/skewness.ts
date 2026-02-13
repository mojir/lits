import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const skewnessReference: VectorReductionReference<'skewness'> = {
  'Vector.skewness': {
    title: 'Vector.skewness',
    category: 'Vector',
    description: 'Calculates the **skewness** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **skewness** of. Minimum length is 3.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { skewness } = import("Vector");\nskewness([1, 2, 3, 6, 20])',
      'let { skewness } = import("Vector");\nskewness([1, 2, 2, 3])',
    ],
  },
  'Vector.moving-skewness': {
    title: 'Vector.moving-skewness',
    category: 'Vector',
    description: 'Calculates the **moving skewness** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving skewness** of.',
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
      'let { moving-skewness } = import("Vector");\nmoving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-skewness } = import("Vector");\nmoving-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'Vector.centered-moving-skewness': {
    title: 'Vector.centered-moving-skewness',
    category: 'Vector',
    description: 'Calculates the **centered moving skewness** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving skewness** of.',
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
      'let { centered-moving-skewness } = import("Vector");\ncentered-moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-skewness } = import("Vector");\ncentered-moving-skewness([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'Vector.running-skewness': {
    title: 'Vector.running-skewness',
    category: 'Vector',
    description: 'Calculates the **running skewness** of a `vector` with a given window size. First two element in result is `null` since **running skewness** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running skewness** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-skewness } = import("Vector");\nrunning-skewness([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleSkewnessReference: VectorReductionReference<'sample-skewness'> = {
  'Vector.sample-skewness': {
    title: 'Vector.sample-skewness',
    category: 'Vector',
    description: 'Calculates the **sample skewness** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample skewness** of. Minimum length is 3.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { sample-skewness } = import("Vector");\nsample-skewness([1, 2, 3, 6, 20])',
      'let { sample-skewness } = import("Vector");\nsample-skewness([1, 2, 2, 3])',
    ],
  },
  'Vector.moving-sample-skewness': {
    title: 'Vector.moving-sample-skewness',
    category: 'Vector',
    description: 'Calculates the **moving sample skewness** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample skewness** of.',
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
      'let { moving-sample-skewness } = import("Vector");\nmoving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-skewness } = import("Vector");\nmoving-sample-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'Vector.centered-moving-sample-skewness': {
    title: 'Vector.centered-moving-sample-skewness',
    category: 'Vector',
    description: 'Calculates the **centered moving sample skewness** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample skewness** of.',
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
      'let { centered-moving-sample-skewness } = import("Vector");\ncentered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-skewness } = import("Vector");\ncentered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
  },
  'Vector.running-sample-skewness': {
    title: 'Vector.running-sample-skewness',
    category: 'Vector',
    description: 'Calculates the **running sample skewness** of a `vector` with a given window size. First two element in result is `null` since **running sample skewness** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample skewness** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-sample-skewness } = import("Vector");\nrunning-sample-skewness([1, 2, 4, 7, 11])',
    ],
  },
}
