import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const meanReference: VectorReductionReference<'mean'> = {
  'Vector.mean': {
    title: 'Vector.mean',
    category: 'Vector',
    description: 'Returns the `mean` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `mean` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { mean } = import("Vector");\nmean([1, 2, 3])',
      'let { mean } = import("Vector");\nmean([1, 2, -3])',
    ],
  },
  'Vector.moving-mean': {
    title: 'Vector.moving-mean',
    category: 'Vector',
    description: 'Returns the **moving mean** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving mean** of.',
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
      'let { moving-mean } = import("Vector");\nmoving-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-mean } = import("Vector");\nmoving-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-mean': {
    title: 'Vector.centered-moving-mean',
    category: 'Vector',
    description: 'Returns the **centered moving mean** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving mean** of.',
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
      'let { centered-moving-mean } = import("Vector");\ncentered-moving-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-mean } = import("Vector");\ncentered-moving-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-mean } = import("Vector");\ncentered-moving-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'Vector.running-mean': {
    title: 'Vector.running-mean',
    category: 'Vector',
    description: 'Returns the **running mean** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running mean** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-mean } = import("Vector");\nrunning-mean([1, 2, 3, 4, 5])',
    ],
  },
}

export const geometricMeanReference: VectorReductionReference<'geometric-mean'> = {
  'Vector.geometric-mean': {
    title: 'Vector.geometric-mean',
    category: 'Vector',
    description: 'Returns the `geometric mean` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `geometric mean` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { geometric-mean } = import("Vector");\ngeometric-mean([1, 2, 3])',
      'let { geometric-mean } = import("Vector");\ngeometric-mean([1, 2, 9])',
    ],
  },
  'Vector.moving-geometric-mean': {
    title: 'Vector.moving-geometric-mean',
    category: 'Vector',
    description: 'Returns the **moving geometric mean** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving geometric mean** of.',
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
      'let { moving-geometric-mean } = import("Vector");\nmoving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-geometric-mean } = import("Vector");\nmoving-geometric-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-geometric-mean': {
    title: 'Vector.centered-moving-geometric-mean',
    category: 'Vector',
    description: 'Returns the **centered moving geometric mean** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving geometric mean** of.',
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
      'let { centered-moving-geometric-mean } = import("Vector");\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-geometric-mean } = import("Vector");\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-geometric-mean } = import("Vector");\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'Vector.running-geometric-mean': {
    title: 'Vector.running-geometric-mean',
    category: 'Vector',
    description: 'Returns the **running geometric mean** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running geometric mean** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-geometric-mean } = import("Vector");\nrunning-geometric-mean([1, 2, 3, 4, 5])',
    ],
  },
}

export const harmonicMeanReference: VectorReductionReference<'harmonic-mean'> = {
  'Vector.harmonic-mean': {
    title: 'Vector.harmonic-mean',
    category: 'Vector',
    description: 'Returns the `harmonic mean` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `harmonic mean` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { harmonic-mean } = import("Vector");\nharmonic-mean([1, 2, 3])',
      'let { harmonic-mean } = import("Vector");\nharmonic-mean([1, 2, 9])',
    ],
  },
  'Vector.moving-harmonic-mean': {
    title: 'Vector.moving-harmonic-mean',
    category: 'Vector',
    description: 'Returns the **moving harmonic mean** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving harmonic mean** of.',
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
      'let { moving-harmonic-mean } = import("Vector");\nmoving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-harmonic-mean } = import("Vector");\nmoving-harmonic-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-harmonic-mean': {
    title: 'Vector.centered-moving-harmonic-mean',
    category: 'Vector',
    description: 'Returns the **centered moving harmonic mean** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving harmonic mean** of.',
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
      'let { centered-moving-harmonic-mean } = import("Vector");\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-harmonic-mean } = import("Vector");\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-harmonic-mean } = import("Vector");\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'Vector.running-harmonic-mean': {
    title: 'Vector.running-harmonic-mean',
    category: 'Vector',
    description: 'Returns the **running harmonic mean** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running harmonic mean** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-harmonic-mean } = import("Vector");\nrunning-harmonic-mean([1, 2, 3, 4, 5])',
    ],
  },
}
