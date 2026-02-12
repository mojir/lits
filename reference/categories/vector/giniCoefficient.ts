import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const giniCoefficientReference: VectorReductionReference<'gini-coefficient'> = {
  'vec.gini-coefficient': {
    title: 'vec.gini-coefficient',
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
      'let vec = import("vec");\nvec.gini-coefficient([1, 2, 3])',
      'let vec = import("vec");\nvec.gini-coefficient([1, 1, 3])',
    ],
  },
  'vec.moving-gini-coefficient': {
    title: 'vec.moving-gini-coefficient',
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
      'let vec = import("vec");\nvec.moving-gini-coefficient([1, 2, 3], 2)',
      'let vec = import("vec");\nvec.moving-gini-coefficient([1, 1, 3], 2)',
    ],
  },
  'vec.centered-moving-gini-coefficient': {
    title: 'vec.centered-moving-gini-coefficient',
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
      'let vec = import("vec");\nvec.centered-moving-gini-coefficient([1, 2, 3], 2)',
      'let vec = import("vec");\nvec.centered-moving-gini-coefficient([1, 1, 3], 2)',
    ],
  },
  'vec.running-gini-coefficient': {
    title: 'vec.running-gini-coefficient',
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
      'let vec = import("vec");\nvec.running-gini-coefficient([1, 2, 3])',
      'let vec = import("vec");\nvec.running-gini-coefficient([1, 1, 3])',
    ],
  },
}
