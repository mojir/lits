import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const maxReference: VectorReductionReference<'TEMP-max', 'max'> = {
  'vec.TEMP-max': {
    title: 'vec.TEMP-max',
    category: 'Vector',
    description: 'Returns the `maximum` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `maximum` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 2, 3])',
      'let { TEMP-max } = import("vec");\nTEMP-max([1, 2, -3])',
    ],
  },
  'vec.moving-max': {
    title: 'vec.moving-max',
    category: 'Vector',
    description: 'Returns the **moving maximum** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving maximum** of.',
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
      'let { moving-max } = import("vec");\nmoving-max([1, 2, 3, 4, 5], 3)',
      'let { moving-max } = import("vec");\nmoving-max([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-max': {
    title: 'vec.centered-moving-max',
    category: 'Vector',
    description: 'Returns the **centered moving maximum** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving maximum** of.',
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
      'let { centered-moving-max } = import("vec");\ncentered-moving-max([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-max } = import("vec");\ncentered-moving-max([1, 2, 3, 4, 5], 3, 0, 100)',
      'let { centered-moving-max } = import("vec");\ncentered-moving-max([1, 2, 3, 4, 5], 3, 0)',
    ],
  },
  'vec.running-max': {
    title: 'vec.running-max',
    category: 'Vector',
    description: 'Returns the **running maximum** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running maximum** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-max } = import("vec");\nrunning-max([1, 2, 3])',
      'let { running-max } = import("vec");\nrunning-max([1, -2, -3])',
    ],
  },
}
