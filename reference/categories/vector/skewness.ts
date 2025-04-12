import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const skewnessReference: VectorReductionReference<'skewness'> = {
  'vec:skewness': {
    title: 'vec:skewness',
    category: 'Vector',
    description: 'Calculates the **skewness** of a `vector`. Returns the third standardized moment.',
    linkName: 'vec-colon-skewness',
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
      'vec:skewness([1, 2, 3, 6, 20])',
      'vec:skewness([1, 2, 2, 3])',
    ],
  },
  'vec:moving-skewness': {
    title: 'vec:moving-skewness',
    category: 'Vector',
    description: 'Calculates the **moving skewness** of a `vector` with a given window size.',
    linkName: 'vec-colon-moving-skewness',
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
      'vec:moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'vec:moving-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec:centered-moving-skewness': {
    title: 'vec:centered-moving-skewness',
    category: 'Vector',
    description: 'Calculates the **centered moving skewness** of a `vector` with a given window size and padding.',
    linkName: 'vec-colon-centered-moving-skewness',
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
      'vec:centered-moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'vec:centered-moving-skewness([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'vec:running-skewness': {
    title: 'vec:running-skewness',
    category: 'Vector',
    description: 'Calculates the **running skewness** of a `vector` with a given window size. First two element in result is `null` since **running skewness** is not defined for less than three elements.',
    linkName: 'vec-colon-running-skewness',
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
      'vec:running-skewness([1, 2, 4, 7, 11])',
    ],
  },
}

export const sampleSkewnessReference: VectorReductionReference<'sample-skewness'> = {
  'vec:sample-skewness': {
    title: 'vec:sample-skewness',
    category: 'Vector',
    description: 'Calculates the **sample skewness** of a `vector`. Returns the third standardized moment.',
    linkName: 'vec-colon-sample-skewness',
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
      'vec:sample-skewness([1, 2, 3, 6, 20])',
      'vec:sample-skewness([1, 2, 2, 3])',
    ],
  },
  'vec:moving-sample-skewness': {
    title: 'vec:moving-sample-skewness',
    category: 'Vector',
    description: 'Calculates the **moving sample skewness** of a `vector` with a given window size.',
    linkName: 'vec-colon-moving-sample-skewness',
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
      'vec:moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'vec:moving-sample-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'vec:centered-moving-sample-skewness': {
    title: 'vec:centered-moving-sample-skewness',
    category: 'Vector',
    description: 'Calculates the **centered moving sample skewness** of a `vector` with a given window size and padding.',
    linkName: 'vec-colon-centered-moving-sample-skewness',
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
      'vec:centered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'vec:centered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
  },
  'vec:running-sample-skewness': {
    title: 'vec:running-sample-skewness',
    category: 'Vector',
    description: 'Calculates the **running sample skewness** of a `vector` with a given window size. First two element in result is `null` since **running sample skewness** is not defined for less than three elements.',
    linkName: 'vec-colon-running-sample-skewness',
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
      'vec:running-sample-skewness([1, 2, 4, 7, 11])',
    ],
  },
}
