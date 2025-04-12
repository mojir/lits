import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const medianAbsoluteDeviationReference: VectorReductionReference<'medad'> = {
  'vec:medad': {
    title: 'vec:medad',
    category: 'Vector',
    description: 'Returns the `median absolute deviation` of all elements in the `vector`.',
    linkName: 'vec-colon-medad',
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
      'vec:medad([1, 2, 3])',
      'vec:medad([1, 2, -3])',
    ],
  },
  'vec:moving-medad': {
    title: 'vec:moving-medad',
    category: 'Vector',
    description: 'Returns the `moving median absolute deviation` of the `vector` with a given window size.',
    linkName: 'vec-colon-moving-medad',
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
      'vec:moving-medad([1, 2, 3, 4, 5], 3)',
      'vec:moving-medad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:centered-moving-medad': {
    title: 'vec:centered-moving-medad',
    category: 'Vector',
    description: 'Returns the `centered moving median absolute deviation` of the `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-medad',
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
      'vec:centered-moving-medad([1, 2, 3, 4, 5], 3)',
      'vec:centered-moving-medad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'vec:running-medad': {
    title: 'vec:running-medad',
    category: 'Vector',
    description: 'Returns the `running median absolute deviation` of the `vector`.',
    linkName: 'vec-colon-running-medad',
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
      'vec:running-medad([1, 2, 3])',
      'vec:running-medad([1, 2, -3])',
    ],
  },
}
