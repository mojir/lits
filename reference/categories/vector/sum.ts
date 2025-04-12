import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const sumReference: VectorReductionReference<'sum'> = {
  'vec:sum': {
    title: 'vec:sum',
    category: 'Vector',
    description: 'Returns the `sum` of all elements in the `vector`.',
    linkName: 'vec-colon-sum',
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
      'vec:sum([1, 2, 3])',
      'vec:sum([1, 2, -3])',
    ],
  },
  'vec:moving-sum': {
    title: 'vec:moving-sum',
    category: 'Vector',
    description: 'Returns the **moving sum` of the `vector** with a given window size.',
    linkName: 'vec-colon-moving-sum',
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
      'vec:moving-sum([1, 2, 3, 4, 5], 3)',
      'vec:moving-sum([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-sum': {
    title: 'vec:centered-moving-sum',
    category: 'Vector',
    description: 'Returns the **centered moving sum` of the `vector** with a given window size.',
    linkName: 'vec-colon-centered-moving-sum',
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
      'vec:centered-moving-sum([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-sum([1, 2, 3, 4, 5], 3, 0, 0)',
      'vec:centered-moving-sum([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'vec:running-sum': {
    title: 'vec:running-sum',
    category: 'Vector',
    description: 'Returns the **running sum` of the `vector**.',
    linkName: 'vec-colon-running-sum',
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
      'vec:running-sum([1, 2, 3])',
      'vec:running-sum([1, -2, -3])',
    ],
  },
}
