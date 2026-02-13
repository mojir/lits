import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const sumReference: VectorReductionReference<'sum'> = {
  'Vector.sum': {
    title: 'Vector.sum',
    category: 'Vector',
    description: 'Returns the `sum` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `sum` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { sum } = import("Vector");\nsum([1, 2, 3])',
      'let { sum } = import("Vector");\nsum([1, 2, -3])',
    ],
  },
  'Vector.moving-sum': {
    title: 'Vector.moving-sum',
    category: 'Vector',
    description: 'Returns the **moving sum** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sum** of.',
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
      'let { moving-sum } = import("Vector");\nmoving-sum([1, 2, 3, 4, 5], 3)',
      'let { moving-sum } = import("Vector");\nmoving-sum([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-sum': {
    title: 'Vector.centered-moving-sum',
    category: 'Vector',
    description: 'Returns the **centered moving sum** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sum** of.',
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
      'let { centered-moving-sum } = import("Vector");\ncentered-moving-sum([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sum } = import("Vector");\ncentered-moving-sum([1, 2, 3, 4, 5], 3, 0, 0)',
      'let { centered-moving-sum } = import("Vector");\ncentered-moving-sum([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'Vector.running-sum': {
    title: 'Vector.running-sum',
    category: 'Vector',
    description: 'Returns the **running sum** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sum** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-sum } = import("Vector");\nrunning-sum([1, 2, 3])',
      'let { running-sum } = import("Vector");\nrunning-sum([1, -2, -3])',
    ],
  },
}
