import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const minReference: VectorReductionReference<'TEMP-min', 'min'> = {
  'vec.TEMP-min': {
    title: 'vec.TEMP-min',
    category: 'Vector',
    description: 'Returns the `minimum` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `minimum` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 2, 3])',
      'let { TEMP-min } = import("vec");\nTEMP-min([1, 2, -3])',
    ],
  },
  'vec.moving-min': {
    title: 'vec.moving-min',
    category: 'Vector',
    description: 'Returns the **moving minimum** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving minimum** of.',
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
      'let { moving-min } = import("vec");\nmoving-min([1, 2, 3, 4, 5], 3)',
      'let { moving-min } = import("vec");\nmoving-min([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-min': {
    title: 'vec.centered-moving-min',
    category: 'Vector',
    description: 'Returns the **centered moving minimum** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving minimum** of.',
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
      'let { centered-moving-min } = import("vec");\ncentered-moving-min([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-min } = import("vec");\ncentered-moving-min([1, 2, 3, 4, 5], 3, 0, 100)',
      'let { centered-moving-min } = import("vec");\ncentered-moving-min([1, 2, 3, 4, 5], 3, 0)',
    ],
  },
  'vec.running-min': {
    title: 'vec.running-min',
    category: 'Vector',
    description: 'Returns the **running minimum** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running minimum** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-min } = import("vec");\nrunning-min([1, 2, 3])',
      'let { running-min } = import("vec");\nrunning-min([1, -2, -3])',
    ],
  },
}
