import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const skewnessReference: VectorReductionReference<'skewness'> = {
  'vec.skewness': {
    title: 'vec.skewness',
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
      'let { skewness } = import("vec");\nskewness([1, 2, 3, 6, 20])',
      'let { skewness } = import("vec");\nskewness([1, 2, 2, 3])',
    ],
  },
  'vec.moving-skewness': {
    title: 'vec.moving-skewness',
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
      'let vec = import("vec");\nvec.moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec.centered-moving-skewness': {
    title: 'vec.centered-moving-skewness',
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
      'let vec = import("vec");\nvec.centered-moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-skewness([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'vec.running-skewness': {
    title: 'vec.running-skewness',
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
      'let vec = import("vec");\nvec.running-skewness([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleSkewnessReference: VectorReductionReference<'sample-skewness'> = {
  'vec.sample-skewness': {
    title: 'vec.sample-skewness',
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
      'let vec = import("vec");\nvec.sample-skewness([1, 2, 3, 6, 20])',
      'let vec = import("vec");\nvec.sample-skewness([1, 2, 2, 3])',
    ],
  },
  'vec.moving-sample-skewness': {
    title: 'vec.moving-sample-skewness',
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
      'let vec = import("vec");\nvec.moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-sample-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec.centered-moving-sample-skewness': {
    title: 'vec.centered-moving-sample-skewness',
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
      'let vec = import("vec");\nvec.centered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
  },
  'vec.running-sample-skewness': {
    title: 'vec.running-sample-skewness',
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
      'let vec = import("vec");\nvec.running-sample-skewness([1, 2, 4, 7, 11])',
    ],
  },
}
