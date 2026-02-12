import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const prodReference: VectorReductionReference<'prod'> = {
  'vec.prod': {
    title: 'vec.prod',
    category: 'Vector',
    description: 'Returns the `product` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `product` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec.prod([1, 2, 3])',
      'vec.prod([1, 2, -3])',
    ],
  },
  'vec.moving-prod': {
    title: 'vec.moving-prod',
    category: 'Vector',
    description: 'Returns the **moving product** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving product** of.',
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
      'vec.moving-prod([1, 2, 3, 4, 5], 3)',
      'vec.moving-prod([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec.centered-moving-prod': {
    title: 'vec.centered-moving-prod',
    category: 'Vector',
    description: 'Returns the **centered moving product** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving product** of.',
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
      'vec.centered-moving-prod([1, 2, 3, 4, 5], 3)',
      'vec.centered-moving-prod([1, 2, 3, 4, 5], 3, 0, 0)',
    ],
  },
  'vec.running-prod': {
    title: 'vec.running-prod',
    category: 'Vector',
    description: 'Returns the **running product** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running product** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec.running-prod([1, 2, 3, 4, 5])',
      'vec.running-prod([1, -2, -3])',
    ],
  },
}
