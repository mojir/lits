import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const standardDeviationReference: VectorReductionReference<'stdev'> = {
  'vec.stdev': {
    title: 'vec.stdev',
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
      'vec.variance([1, 2, 3])',
      'vec.variance([1, 2, -3])',
    ],
  },
  'vec.moving-stdev': {
    title: 'vec.moving-stdev',
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
      'vec.moving-stdev([1, 2, 3, 4, 5], 3)',
      'vec.moving-stdev([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-stdev': {
    title: 'vec.centered-moving-stdev',
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
      'vec.centered-moving-stdev([1, 2, 3, 4, 5], 3)',
      'vec.centered-moving-stdev([1, 2, 3, 4, 5], 3, 1)',
      'vec.centered-moving-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'vec.centered-moving-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'vec.running-stdev': {
    title: 'vec.running-stdev',
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
      'vec.running-stdev([1, 2, 3, 4, 5])',
    ],
  },
}

export const sampleStandardDeviationReference: VectorReductionReference<'sample-stdev'> = {
  'vec.sample-stdev': {
    title: 'vec.sample-stdev',
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
      'vec.variance([1, 2, 3])',
      'vec.variance([1, 2, -3])',
    ],
  },
  'vec.moving-sample-stdev': {
    title: 'vec.moving-sample-stdev',
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
      'vec.moving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'vec.moving-sample-stdev([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-sample-stdev': {
    title: 'vec.centered-moving-sample-stdev',
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
      'vec.centered-moving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'vec.centered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1)',
      'vec.centered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'vec.centered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'vec.running-sample-stdev': {
    title: 'vec.running-sample-stdev',
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
      'vec.running-sample-stdev([1, 2, 3, 4, 5])',
    ],
  },
}
