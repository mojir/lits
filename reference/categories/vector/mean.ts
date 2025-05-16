import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const meanReference: VectorReductionReference<'mean'> = {
  'vec:mean': {
    title: 'vec:mean',
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
      'vec:mean([1, 2, 3])',
      'vec:mean([1, 2, -3])',
    ],
  },
  'vec:moving-mean': {
    title: 'vec:moving-mean',
    category: 'Vector',
    description: 'Returns the **moving mean` of the `vector** with a given window size.',
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
      'vec:moving-mean([1, 2, 3, 4, 5], 3)',
      'vec:moving-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-mean': {
    title: 'vec:centered-moving-mean',
    category: 'Vector',
    description: 'Returns the **centered moving mean` of the `vector** with a given window size.',
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
      'vec:centered-moving-mean([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'vec:centered-moving-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'vec:running-mean': {
    title: 'vec:running-mean',
    category: 'Vector',
    description: 'Returns the **running mean` of the `vector**.',
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
      'vec:running-mean([1, 2, 3, 4, 5])',
    ],
  },
}

export const geometricMeanReference: VectorReductionReference<'geometric-mean'> = {
  'vec:geometric-mean': {
    title: 'vec:geometric-mean',
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
      'vec:geometric-mean([1, 2, 3])',
      'vec:geometric-mean([1, 2, 9])',
    ],
  },
  'vec:moving-geometric-mean': {
    title: 'vec:moving-geometric-mean',
    category: 'Vector',
    description: 'Returns the **moving geometric mean` of the `vector** with a given window size.',
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
      'vec:moving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'vec:moving-geometric-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-geometric-mean': {
    title: 'vec:centered-moving-geometric-mean',
    category: 'Vector',
    description: 'Returns the **centered moving geometric mean` of the `vector** with a given window size.',
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
      'vec:centered-moving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'vec:centered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'vec:running-geometric-mean': {
    title: 'vec:running-geometric-mean',
    category: 'Vector',
    description: 'Returns the **running geometric mean` of the `vector**.',
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
      'vec:running-geometric-mean([1, 2, 3, 4, 5])',
    ],
  },
}

export const harmonicMeanReference: VectorReductionReference<'harmonic-mean'> = {
  'vec:harmonic-mean': {
    title: 'vec:harmonic-mean',
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
      'vec:harmonic-mean([1, 2, 3])',
      'vec:harmonic-mean([1, 2, 9])',
    ],
  },
  'vec:moving-harmonic-mean': {
    title: 'vec:moving-harmonic-mean',
    category: 'Vector',
    description: 'Returns the **moving harmonic mean` of the `vector** with a given window size.',
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
      'vec:moving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'vec:moving-harmonic-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-harmonic-mean': {
    title: 'vec:centered-moving-harmonic-mean',
    category: 'Vector',
    description: 'Returns the **centered moving harmonic mean` of the `vector** with a given window size.',
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
      'vec:centered-moving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'vec:centered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'vec:running-harmonic-mean': {
    title: 'vec:running-harmonic-mean',
    category: 'Vector',
    description: 'Returns the **running harmonic mean` of the `vector**.',
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
      'vec:running-harmonic-mean([1, 2, 3, 4, 5])',
    ],
  },
}
