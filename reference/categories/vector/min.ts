import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const minReference: VectorReductionReference<'min'> = {
  'vec:min': {
    title: 'vec:min',
    category: 'Vector',
    description: 'Returns the `minimum` of all elements in the `vector`.',
    linkName: 'vec-colon-min',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `minimum` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:min([1, 2, 3])',
      'vec:min([1, 2, -3])',
    ],
  },
  'vec:moving-min': {
    title: 'vec:moving-min',
    category: 'Vector',
    description: 'Returns the `moving minimum` of the `vector` with a given window size.',
    linkName: 'vec-colon-moving-min',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving minimum` of.',
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
      'vec:moving-min([1, 2, 3, 4, 5], 3)',
      'vec:moving-min([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-min': {
    title: 'vec:centered-moving-min',
    category: 'Vector',
    description: 'Returns the `centered moving minimum` of the `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-min',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving minimum` of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      paddingValue: {
        type: 'number',
        description: 'The value to use for padding. Default is 0.',
      },
      rightPaddingValue: {
        type: 'number',
        description: 'The value to use for right padding. Default is the same as paddingValue.',
      },
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
      { argumentNames: ['vector', 'windowSize', 'paddingValue'] },
      { argumentNames: ['vector', 'windowSize', 'paddingValue', 'rightPaddingValue'] },
    ],
    examples: [
      'vec:centered-moving-min([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-min([1, 2, 3, 4, 5], 3, 0, 100)',
      'vec:centered-moving-min([1, 2, 3, 4, 5], 3, 0)',
      'vec:centered-moving-min([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-min': {
    title: 'vec:running-min',
    category: 'Vector',
    description: 'Returns the `running minimum` of the `vector`.',
    linkName: 'vec-colon-running-min',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running minimum` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-min([1, 2, 3])',
      'vec:running-min([1, -2, -3])',
    ],
  },
}
