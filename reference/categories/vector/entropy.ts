import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const entropyReference: VectorReductionReference<'entropy'> = {
  'vec:entropy': {
    title: 'vec:entropy',
    category: 'Vector',
    description: 'Calculates the **entropy** of a `vector`. The entropy is a measure of the uncertainty associated with a random variable.',
    linkName: 'vec-colon-entropy',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **entropy** of. Minimum length is 1.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:entropy([1, 1, 2, 3, 3, 3])',
      'vec:entropy([1, 2, 3])',
      'vec:entropy([1, 2, 2, 3])',
      'vec:entropy([0])',
      'vec:entropy([1])',
      'vec:entropy([1, 2])',
    ],
  },
  'vec:moving-entropy': {
    title: 'vec:moving-entropy',
    category: 'Vector',
    description: 'Calculates the **moving entropy** of a `vector` with a given window size.',
    linkName: 'vec-colon-moving-entropy',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving entropy** of.',
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
      'vec:moving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'vec:moving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'vec:moving-entropy([1, 2], 2)',
    ],
  },
  'vec:centered-moving-entropy': {
    title: 'vec:centered-moving-entropy',
    category: 'Vector',
    description: 'Calculates the **centered moving entropy** of a `vector` with a given window size.',
    linkName: 'vec-colon-centered-moving-entropy',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving entropy** of.',
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
      'vec:centered-moving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'vec:centered-moving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'vec:centered-moving-entropy([1, 2], 2)',
    ],
  },
  'vec:running-entropy': {
    title: 'vec:running-entropy',
    category: 'Vector',
    description: 'Calculates the **running entropy** of a `vector`.',
    linkName: 'vec-colon-running-entropy',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running entropy** of.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:running-entropy([1, 1, 2, 3, 3, 3])',
      'vec:running-entropy([1, 2])',
    ],
  },
}
