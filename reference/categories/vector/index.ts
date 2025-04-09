import type { FunctionReference } from '../..'
import type { VectorApiName } from '../../api'

export const vectorReference: Record<VectorApiName, FunctionReference<'Vector'>> = {
  'vec:vector?': {
    title: 'vec:vector?',
    category: 'Vector',
    description: 'Checks if a value is a vector.',
    linkName: 'c-colon-vector-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      value: {
        type: 'any',
        description: 'The value to check.',
      },
    },
    variants: [
      { argumentNames: ['value'] },
    ],
    examples: [
      'vec:vector?(1)',
      'vec:vector?([1, 2, 3])',
      'vec:vector?([1, 2, "3"])',
    ],
  },
  'vec:sorted?': {
    title: 'vec:sorted?',
    category: 'Vector',
    description: 'Checks if a vector is sorted.',
    linkName: 'c-colon-sorted-question-mark',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'vec:sorted?([1, 2, 3])',
      'vec:sorted?([3, 2, 1])',
      'vec:sorted?([1, 2, 3, 2])',
      'vec:sorted?([1, 2, 3, 4])',
    ],
  },
}
