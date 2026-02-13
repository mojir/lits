import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const entropyReference: VectorReductionReference<'entropy'> = {
  'Vector.entropy': {
    title: 'Vector.entropy',
    category: 'Vector',
    description: 'Calculates the **entropy** of a `vector`. The entropy is a measure of the uncertainty associated with a random variable.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **entropy** of. Minimum length is 1.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { entropy } = import("Vector");\nentropy([1, 1, 2, 3, 3, 3])',
      'let { entropy } = import("Vector");\nentropy([1, 2, 3])',
      'let { entropy } = import("Vector");\nentropy([1, 2, 2, 3])',
      'let { entropy } = import("Vector");\nentropy([0])',
      'let { entropy } = import("Vector");\nentropy([1])',
      'let { entropy } = import("Vector");\nentropy([1, 2])',
    ],
  },
  'Vector.moving-entropy': {
    title: 'Vector.moving-entropy',
    category: 'Vector',
    description: 'Calculates the **moving entropy** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving entropy** of.',
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
      'let { moving-entropy } = import("Vector");\nmoving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'let { moving-entropy } = import("Vector");\nmoving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'let { moving-entropy } = import("Vector");\nmoving-entropy([1, 2], 2)',
    ],
  },
  'Vector.centered-moving-entropy': {
    title: 'Vector.centered-moving-entropy',
    category: 'Vector',
    description: 'Calculates the **centered moving entropy** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving entropy** of.',
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
      'let { centered-moving-entropy } = import("Vector");\ncentered-moving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'let { centered-moving-entropy } = import("Vector");\ncentered-moving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'let { centered-moving-entropy } = import("Vector");\ncentered-moving-entropy([1, 2], 2)',
    ],
  },
  'Vector.running-entropy': {
    title: 'Vector.running-entropy',
    category: 'Vector',
    description: 'Calculates the **running entropy** of a `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running entropy** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-entropy } = import("Vector");\nrunning-entropy([1, 1, 2, 3, 3, 3])',
      'let { running-entropy } = import("Vector");\nrunning-entropy([1, 2])',
    ],
  },
}
