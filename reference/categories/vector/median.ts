import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const medianReference: VectorReductionReference<'median'> = {
  'vec.median': {
    title: 'vec.median',
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
      'let { median } = import("vec");\nmedian([1, 2, 3])',
      'let { median } = import("vec");\nmedian([1, 2, -3])',
    ],
  },
  'vec.moving-median': {
    title: 'vec.moving-median',
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
      'let vec = import("vec");\nvec.moving-median([1, 2, 3, 4, 5], 3)',
      'let vec = import("vec");\nvec.moving-median([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-median': {
    title: 'vec.centered-moving-median',
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
      'let vec = import("vec");\nvec.centered-moving-median([1, 2, 3, 4, 5], 3)',
      'let vec = import("vec");\nvec.centered-moving-median([1, 2, 3, 4, 5], 3, 0, 10)',
      'let vec = import("vec");\nvec.centered-moving-median([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'vec.running-median': {
    title: 'vec.running-median',
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
      'let vec = import("vec");\nvec.running-median([1, 2, 3, 4, 5])',
    ],
  },
}
