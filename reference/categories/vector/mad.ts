import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const meanAbsoluteDeviationReference: VectorReductionReference<'mad'> = {
  'vec.mad': {
    title: 'vec.mad',
    category: 'Vector',
    description: 'Returns the `mean absolute deviation` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `mean absolute deviation` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let vec = import("vec");\nvec.mad([1, 2, 3])',
      'let vec = import("vec");\nvec.mad([1, 2, -3])',
    ],
  },
  'vec.moving-mad': {
    title: 'vec.moving-mad',
    category: 'Vector',
    description: 'Returns the `moving mean absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving mean absolute deviation` of.',
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
      'let vec = import("vec");\nvec.moving-mad([1, 2, 3, 4, 5], 3)',
      'let vec = import("vec");\nvec.moving-mad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-mad': {
    title: 'vec.centered-moving-mad',
    category: 'Vector',
    description: 'Returns the `centered moving mean absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving mean absolute deviation` of.',
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
      'let vec = import("vec");\nvec.centered-moving-mad([1, 2, 3, 4, 5], 3)',
      'let vec = import("vec");\nvec.centered-moving-mad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.running-mad': {
    title: 'vec.running-mad',
    category: 'Vector',
    description: 'Returns the `running mean absolute deviation` of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running mean absolute deviation` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let vec = import("vec");\nvec.running-mad([1, 2, 3])',
      'let vec = import("vec");\nvec.running-mad([1, 2, -3])',
    ],
  },
}
