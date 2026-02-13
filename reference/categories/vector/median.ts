import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const medianReference: VectorReductionReference<'median'> = {
  'Vector.median': {
    title: 'Vector.median',
    category: 'Vector',
    description: 'Returns the `median` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the `median` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { median } = import("Vector");\nmedian([1, 2, 3])',
      'let { median } = import("Vector");\nmedian([1, 2, -3])',
    ],
  },
  'Vector.moving-median': {
    title: 'Vector.moving-median',
    category: 'Vector',
    description: 'Returns the **moving median** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving median** of.',
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
      'let { moving-median } = import("Vector");\nmoving-median([1, 2, 3, 4, 5], 3)',
      'let { moving-median } = import("Vector");\nmoving-median([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-median': {
    title: 'Vector.centered-moving-median',
    category: 'Vector',
    description: 'Returns the **centered moving median** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving median** of.',
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
      'let { centered-moving-median } = import("Vector");\ncentered-moving-median([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-median } = import("Vector");\ncentered-moving-median([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-median } = import("Vector");\ncentered-moving-median([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'Vector.running-median': {
    title: 'Vector.running-median',
    category: 'Vector',
    description: 'Returns the **running median** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running median** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-median } = import("Vector");\nrunning-median([1, 2, 3, 4, 5])',
    ],
  },
}
