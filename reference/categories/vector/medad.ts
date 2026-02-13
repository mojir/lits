import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const medianAbsoluteDeviationReference: VectorReductionReference<'medad'> = {
  'Vector.medad': {
    title: 'Vector.medad',
    category: 'Vector',
    description: 'Returns the `median absolute deviation` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `median absolute deviation` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { medad } = import("Vector");\nmedad([1, 2, 3])',
      'let { medad } = import("Vector");\nmedad([1, 2, -3])',
    ],
  },
  'Vector.moving-medad': {
    title: 'Vector.moving-medad',
    category: 'Vector',
    description: 'Returns the `moving median absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving median absolute deviation` of.',
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
      'let { moving-medad } = import("Vector");\nmoving-medad([1, 2, 3, 4, 5], 3)',
      'let { moving-medad } = import("Vector");\nmoving-medad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.centered-moving-medad': {
    title: 'Vector.centered-moving-medad',
    category: 'Vector',
    description: 'Returns the `centered moving median absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving median absolute deviation` of.',
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
      'let { centered-moving-medad } = import("Vector");\ncentered-moving-medad([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-medad } = import("Vector");\ncentered-moving-medad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'Vector.running-medad': {
    title: 'Vector.running-medad',
    category: 'Vector',
    description: 'Returns the `running median absolute deviation` of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running median absolute deviation` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let { running-medad } = import("Vector");\nrunning-medad([1, 2, 3])',
      'let { running-medad } = import("Vector");\nrunning-medad([1, 2, -3])',
    ],
  },
}
