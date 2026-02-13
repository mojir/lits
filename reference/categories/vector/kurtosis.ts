import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const kurtosisReference: VectorReductionReference<'kurtosis'> = {
  'vec.kurtosis': {
    title: 'vec.kurtosis',
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
      'let { kurtosis } = import("vec");\nkurtosis([1, 2, 3, 6, 20])',
      'let { kurtosis } = import("vec");\nkurtosis([1, 2, 2, 3])',
    ],
  },
  'vec.moving-kurtosis': {
    title: 'vec.moving-kurtosis',
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
      'let vec = import("vec");\nvec.moving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec.centered-moving-kurtosis': {
    title: 'vec.centered-moving-kurtosis',
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
      'let vec = import("vec");\nvec.centered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'vec.running-kurtosis': {
    title: 'vec.running-kurtosis',
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
      'let vec = import("vec");\nvec.running-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleKurtosisReference: VectorReductionReference<'sample-kurtosis'> = {
  'vec.sample-kurtosis': {
    title: 'vec.sample-kurtosis',
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
      'let vec = import("vec");\nvec.sample-kurtosis([1, 2, 3, 6, 20])',
      'let vec = import("vec");\nvec.sample-kurtosis([1, 2, 2, 3])',
    ],
  },
  'vec.moving-sample-kurtosis': {
    title: 'vec.moving-sample-kurtosis',
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
      'let vec = import("vec");\nvec.moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec.centered-moving-sample-kurtosis': {
    title: 'vec.centered-moving-sample-kurtosis',
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
      'let vec = import("vec");\nvec.centered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
  },
  'vec.running-sample-kurtosis': {
    title: 'vec.running-sample-kurtosis',
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
      'let vec = import("vec");\nvec.running-sample-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}

export const excessKurtoisReference: VectorReductionReference<'excess-kurtosis'> = {
  'vec.excess-kurtosis': {
    title: 'vec.excess-kurtosis',
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
      'let vec = import("vec");\nvec.excess-kurtosis([1, 2, 3, 6, 20])',
      'let vec = import("vec");\nvec.excess-kurtosis([1, 2, 2, 3])',
    ],
  },
  'vec.moving-excess-kurtosis': {
    title: 'vec.moving-excess-kurtosis',
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
      'let vec = import("vec");\nvec.moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec.centered-moving-excess-kurtosis': {
    title: 'vec.centered-moving-excess-kurtosis',
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
      'let vec = import("vec");\nvec.centered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'vec.running-excess-kurtosis': {
    title: 'vec.running-excess-kurtosis',
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
      'let vec = import("vec");\nvec.running-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleExcessKurtosisReference: VectorReductionReference<'sample-excess-kurtosis'> = {
  'vec.sample-excess-kurtosis': {
    title: 'vec.sample-excess-kurtosis',
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
      'let vec = import("vec");\nvec.sample-excess-kurtosis([1, 2, 3, 6, 20])',
      'let vec = import("vec");\nvec.sample-excess-kurtosis([1, 2, 2, 3])',
    ],
  },
  'vec.moving-sample-excess-kurtosis': {
    title: 'vec.moving-sample-excess-kurtosis',
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
      'let vec = import("vec");\nvec.moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec.centered-moving-sample-excess-kurtosis': {
    title: 'vec.centered-moving-sample-excess-kurtosis',
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
      'let vec = import("vec");\nvec.centered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
  },
  'vec.running-sample-excess-kurtosis': {
    title: 'vec.running-sample-excess-kurtosis',
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
      'let vec = import("vec");\nvec.running-sample-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
}
