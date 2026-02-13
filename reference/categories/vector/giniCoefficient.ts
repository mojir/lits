import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const giniCoefficientReference: VectorReductionReference<'gini-coefficient'> = {
  'Vector.gini-coefficient': {
    title: 'Vector.gini-coefficient',
    category: 'Vector',
    description: 'Returns the **gini coefficient** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **gini coefficient** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { gini-coefficient } = import("Vector");\ngini-coefficient([1, 2, 3])',
      'let { gini-coefficient } = import("Vector");\ngini-coefficient([1, 1, 3])',
    ],
  },
  'Vector.moving-gini-coefficient': {
    title: 'Vector.moving-gini-coefficient',
    category: 'Vector',
    description: 'Returns the **moving gini coefficient** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving gini coefficient** of.',
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
      'let { moving-gini-coefficient } = import("Vector");\nmoving-gini-coefficient([1, 2, 3], 2)',
      'let { moving-gini-coefficient } = import("Vector");\nmoving-gini-coefficient([1, 1, 3], 2)',
    ],
  },
  'Vector.centered-moving-gini-coefficient': {
    title: 'Vector.centered-moving-gini-coefficient',
    category: 'Vector',
    description: 'Returns the **centered moving gini coefficient** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving gini coefficient** of.',
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
      'let { centered-moving-gini-coefficient } = import("Vector");\ncentered-moving-gini-coefficient([1, 2, 3], 2)',
      'let { centered-moving-gini-coefficient } = import("Vector");\ncentered-moving-gini-coefficient([1, 1, 3], 2)',
    ],
  },
  'Vector.running-gini-coefficient': {
    title: 'Vector.running-gini-coefficient',
    category: 'Vector',
    description: 'Returns the **running gini coefficient** of the `vector`.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running gini coefficient** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-gini-coefficient } = import("Vector");\nrunning-gini-coefficient([1, 2, 3])',
      'let { running-gini-coefficient } = import("Vector");\nrunning-gini-coefficient([1, 1, 3])',
    ],
  },
}
