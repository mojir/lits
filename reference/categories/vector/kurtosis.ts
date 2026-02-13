import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const kurtosisReference: VectorReductionReference<'kurtosis'> = {
  'Vector.kurtosis': {
    title: 'Vector.kurtosis',
    category: 'Vector',
    description: 'Calculates the **kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { kurtosis } = import("Vector");\nkurtosis([1, 2, 3, 6, 20])',
      'let { kurtosis } = import("Vector");\nkurtosis([1, 2, 2, 3])',
    ],
  },
  'Vector.moving-kurtosis': {
    title: 'Vector.moving-kurtosis',
    category: 'Vector',
    description: 'Calculates the **moving kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving kurtosis** of.',
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
      'let { moving-kurtosis } = import("Vector");\nmoving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-kurtosis } = import("Vector");\nmoving-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'Vector.centered-moving-kurtosis': {
    title: 'Vector.centered-moving-kurtosis',
    category: 'Vector',
    description: 'Calculates the **centered moving kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving kurtosis** of.',
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
      'let { centered-moving-kurtosis } = import("Vector");\ncentered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-kurtosis } = import("Vector");\ncentered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'Vector.running-kurtosis': {
    title: 'Vector.running-kurtosis',
    category: 'Vector',
    description: 'Calculates the **running kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running kurtosis** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-kurtosis } = import("Vector");\nrunning-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleKurtosisReference: VectorReductionReference<'sample-kurtosis'> = {
  'Vector.sample-kurtosis': {
    title: 'Vector.sample-kurtosis',
    category: 'Vector',
    description: 'Calculates the **sample kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { sample-kurtosis } = import("Vector");\nsample-kurtosis([1, 2, 3, 6, 20])',
      'let { sample-kurtosis } = import("Vector");\nsample-kurtosis([1, 2, 2, 3])',
    ],
  },
  'Vector.moving-sample-kurtosis': {
    title: 'Vector.moving-sample-kurtosis',
    category: 'Vector',
    description: 'Calculates the **moving sample kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample kurtosis** of.',
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
      'let { moving-sample-kurtosis } = import("Vector");\nmoving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-kurtosis } = import("Vector");\nmoving-sample-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'Vector.centered-moving-sample-kurtosis': {
    title: 'Vector.centered-moving-sample-kurtosis',
    category: 'Vector',
    description: 'Calculates the **centered moving sample kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample kurtosis** of.',
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
      'let { centered-moving-sample-kurtosis } = import("Vector");\ncentered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-kurtosis } = import("Vector");\ncentered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
  },
  'Vector.running-sample-kurtosis': {
    title: 'Vector.running-sample-kurtosis',
    category: 'Vector',
    description: 'Calculates the **running sample kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running sample kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample kurtosis** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-sample-kurtosis } = import("Vector");\nrunning-sample-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}

export const excessKurtoisReference: VectorReductionReference<'excess-kurtosis'> = {
  'Vector.excess-kurtosis': {
    title: 'Vector.excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **excess kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **excess kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { excess-kurtosis } = import("Vector");\nexcess-kurtosis([1, 2, 3, 6, 20])',
      'let { excess-kurtosis } = import("Vector");\nexcess-kurtosis([1, 2, 2, 3])',
    ],
  },
  'Vector.moving-excess-kurtosis': {
    title: 'Vector.moving-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **moving excess kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving excess kurtosis** of.',
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
      'let { moving-excess-kurtosis } = import("Vector");\nmoving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-excess-kurtosis } = import("Vector");\nmoving-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'Vector.centered-moving-excess-kurtosis': {
    title: 'Vector.centered-moving-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **centered moving excess kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving excess kurtosis** of.',
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
      'let { centered-moving-excess-kurtosis } = import("Vector");\ncentered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-excess-kurtosis } = import("Vector");\ncentered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'Vector.running-excess-kurtosis': {
    title: 'Vector.running-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **running excess kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running excess kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running excess kurtosis** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-excess-kurtosis } = import("Vector");\nrunning-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleExcessKurtosisReference: VectorReductionReference<'sample-excess-kurtosis'> = {
  'Vector.sample-excess-kurtosis': {
    title: 'Vector.sample-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **sample excess kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample excess kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { sample-excess-kurtosis } = import("Vector");\nsample-excess-kurtosis([1, 2, 3, 6, 20])',
      'let { sample-excess-kurtosis } = import("Vector");\nsample-excess-kurtosis([1, 2, 2, 3])',
    ],
  },
  'Vector.moving-sample-excess-kurtosis': {
    title: 'Vector.moving-sample-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **moving sample excess kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample excess kurtosis** of.',
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
      'let { moving-sample-excess-kurtosis } = import("Vector");\nmoving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-excess-kurtosis } = import("Vector");\nmoving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'Vector.centered-moving-sample-excess-kurtosis': {
    title: 'Vector.centered-moving-sample-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **centered moving sample excess kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample excess kurtosis** of.',
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
      'let { centered-moving-sample-excess-kurtosis } = import("Vector");\ncentered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-excess-kurtosis } = import("Vector");\ncentered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
  },
  'Vector.running-sample-excess-kurtosis': {
    title: 'Vector.running-sample-excess-kurtosis',
    category: 'Vector',
    description: 'Calculates the **running sample excess kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running sample excess kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample excess kurtosis** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-sample-excess-kurtosis } = import("Vector");\nrunning-sample-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}
