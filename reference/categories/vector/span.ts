import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const spanReference: VectorReductionReference<'span'> = {
  'vec:span': {
    title: 'vec:span',
    category: 'Vector',
    description: 'Calculates the **span** of a `vector`. Returns the difference between the maximum and minimum values.',
    linkName: 'vec-colon-span',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **span** of. Minimum length is 0.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:span([1, 2, 3, 4])',
      'vec:span([1, 2, 3, 4, 5])',
      'vec:span([1])',
      'vec:span([])',
    ],
  },

  'vec:moving-span': {
    title: 'vec:moving-span',
    category: 'Vector',
    description: 'Calculates the **moving span** of a `vector` with a given window size.',
    linkName: 'vec-colon-moving-span',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving span** of.',
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
      'vec:moving-span([1, 2, 4, 7, 11, 16], 4)',
      'vec:moving-span([1, 2, 4, 7, 11, 16], 5)',
      'vec:moving-span([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'vec:centered-moving-span': {
    title: 'vec:centered-moving-span',
    category: 'Vector',
    description: 'Calculates the **centered moving span** of a `vector` with a given window size. The result is padded with `leftPadding` on the left and right.',
    linkName: 'vec-colon-centered-moving-span',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving span** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'The value to pad the result with on the left.',
      },
      rightPadding: {
        type: 'number',
        description: 'The value to pad the result with on the right.',
      },
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
      { argumentNames: ['vector', 'windowSize', 'leftPadding'] },
      { argumentNames: ['vector', 'windowSize', 'leftPadding', 'rightPadding'] },
    ],
    examples: [
      'vec:centered-moving-span([1, 2, 4, 7, 11, 16], 4)',
      'vec:centered-moving-span([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
  },
  'vec:running-span': {
    title: 'vec:running-span',
    category: 'Vector',
    description: 'Calculates the **running span** of a `vector` with a given window size.',
    linkName: 'vec-colon-running-span',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running span** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-span([1, 2, 4])',
    ],
  },
}
