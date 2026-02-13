import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const meanAbsoluteDeviationReference: VectorReductionReference<'mad'> = {
  'Vector.mad': {
    title: 'Vector.mad',
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
      'let { mad } = import("Vector");\nmad([1, 2, 3])',
      'let { mad } = import("Vector");\nmad([1, 2, -3])',
    ],
  },
  'Vector.moving-mad': {
    title: 'Vector.moving-mad',
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
      'let { moving-mad } = import("Vector");\nmoving-mad([1, 2, 3, 4, 5], 3)',
      'let { moving-mad } = import("Vector");\nmoving-mad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-mad': {
    title: 'Vector.centered-moving-mad',
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
      'let { centered-moving-mad } = import("Vector");\ncentered-moving-mad([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-mad } = import("Vector");\ncentered-moving-mad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.running-mad': {
    title: 'Vector.running-mad',
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
      'let { running-mad } = import("Vector");\nrunning-mad([1, 2, 3])',
      'let { running-mad } = import("Vector");\nrunning-mad([1, 2, -3])',
    ],
  },
}
