import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const medianReference: VectorReductionReference<'median'> = {
  'vec:median': {
    title: 'vec:median',
    category: 'Vector',
    description: 'Returns the `median` of all elements in the `vector`.',
    linkName: 'vec-colon-median',
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
      'vec:median([1, 2, 3])',
      'vec:median([1, 2, -3])',
    ],
  },
  'vec:moving-median': {
    title: 'vec:moving-median',
    category: 'Vector',
    description: 'Returns the `moving median` of the `vector` with a given window size.',
    linkName: 'vec-colon-moving-median',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving median` of.',
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
      'vec:moving-median([1, 2, 3, 4, 5], 3)',
      'vec:moving-median([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:centered-moving-median': {
    title: 'vec:centered-moving-median',
    category: 'Vector',
    description: 'Returns the `centered moving median` of the `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-median',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving median` of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      paddingValue: {
        type: 'number',
        description: 'Optional value to use for padding. Default is 0.',
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
      'vec:centered-moving-median([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-median([1, 2, 3, 4, 5], 3, 10)',
      'vec:centered-moving-median([1, 2, 3, 4, 5], 10)',
    ],
  },
  'vec:running-median': {
    title: 'vec:running-median',
    category: 'Vector',
    description: 'Returns the `running median` of the `vector`.',
    linkName: 'vec-colon-running-median',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running median` of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-median([1, 2, 3, 4, 5])',
    ],
  },
}
