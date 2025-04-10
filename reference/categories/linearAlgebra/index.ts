import type { FunctionReference } from '../..'
import type { LinAlgApiName } from '../../api'

export const linAlgReference: Record<LinAlgApiName, FunctionReference<'Linear Algebra'>> = {
  'lin:dot': {
    title: 'lin:dot',
    category: 'Linear Algebra',
    description: 'Calculates the dot product of two vectors. The result is a scalar.',
    linkName: 'vec-colon-dot',
    returns: {
      type: 'number',
    },
    args: {
      vector1: {
        type: 'vector',
        description: 'First vector.',
      },
      vector2: {
        type: 'vector',
        description: 'Second vector.',
      },
    },
    variants: [
      { argumentNames: ['vector1', 'vector2'] },
    ],
    examples: [
      'lin:dot([1, 2], [3, 4])',
      'lin:dot([1, 2, 3], [4, 5, 6])',
    ],
  },
  'lin:cross': {
    title: 'lin:cross',
    category: 'Linear Algebra',
    description: 'Calculates the cross product of two 3D vectors. The result is a vector perpendicular to both input vectors.',
    linkName: 'vec-colon-cross',
    returns: {
      type: 'vector',
    },
    args: {
      vector1: {
        type: 'vector',
        description: 'First vector (3D).',
      },
      vector2: {
        type: 'vector',
        description: 'Second vector (3D).',
      },
    },
    variants: [
      { argumentNames: ['vector1', 'vector2'] },
    ],
    examples: [
      'lin:cross([1, 2, 3], [4, 5, 6])',
      'lin:cross([1, 0, 0], [0, 1, 0])',
      'lin:cross([0, 0, 1], [1, 0, 0])',
      'lin:cross([1, 2, 3], [0, 0, 0])',
      'lin:cross([0, 0, 0], [1, 2, 3])',
    ],
  },
  'lin:normalize-minmax': {
    title: 'lin:normalize-minmax',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using min-max normalization. The result is a vector with values between 0 and 1.',
    linkName: 'vec-colon-normalize-minmax',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:normalize-minmax([1, 2, 3])',
      'lin:normalize-minmax([1, 2, -3])',
      'lin:normalize-minmax([1, 2, 3, 4])',
      'lin:normalize-minmax([1, 2, -3, 4])',
      'lin:normalize-minmax([1, 2, 3, 40, 50])',
    ],
  },
  'lin:normalize-zscore': {
    title: 'lin:normalize-zscore',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using z-score normalization. The result is a vector with mean 0 and standard deviation 1.',
    linkName: 'vec-colon-normalize-zscore',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:normalize-zscore([1, 2, 3])',
      'lin:normalize-zscore([1, 2, -3])',
      'lin:normalize-zscore([1, 2, 3, 4])',
      'lin:normalize-zscore([1, 2, -3, 4])',
      'lin:normalize-zscore([1, 2, 3, 40, 50])',
    ],
  },
  'lin:normalize-robust': {
    title: 'lin:normalize-robust',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using robust normalization. The result is a vector with median 0 and median absolute deviation 1.',
    linkName: 'vec-colon-normalize-robust',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:normalize-robust([1, 2, 3])',
      'lin:normalize-robust([1, 2, -3])',
      'lin:normalize-robust([1, 2, 3, 4])',
      'lin:normalize-robust([1, 2, -3, 4])',
      'lin:normalize-robust([1, 2, 3, 40, 50])',
    ],
  },
  'lin:normalize-l1': {
    title: 'lin:normalize-l1',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using L1 normalization. The result is a vector with L1 norm equal to 1.',
    linkName: 'vec-colon-normalize-l1',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:normalize-l1([1, 2, 3])',
      'lin:normalize-l1([1, 2, -3])',
      'lin:normalize-l1([1, 2, 3, 4])',
      'lin:normalize-l1([1, 2, -3, 4])',
      'lin:normalize-l1([1, 2, 3, 40, 50])',
    ],
  },
  'lin:normalize-l2': {
    title: 'lin:normalize-l2',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using L2 normalization. The result is a vector with L2 norm equal to 1.',
    linkName: 'vec-colon-normalize-l2',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:normalize-l2([1, 2, 3])',
      'lin:normalize-l2([1, 2, -3])',
      'lin:normalize-l2([1, 2, 3, 4])',
      'lin:normalize-l2([1, 2, -3, 4])',
      'lin:normalize-l2([1, 2, 3, 40, 50])',
    ],
  },
  'lin:normalize-log': {
    title: 'lin:normalize-log',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using natural log normalization. The result is a vector with log-transformed values.',
    linkName: 'vec-colon-normalize-log',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:normalize-log([1, 2, 3])',
      'lin:normalize-log([1, 2, -3])',
      'lin:normalize-log([1, 2, 3, 4])',
      'lin:normalize-log([1, 2, -3, 4])',
      'lin:normalize-log([1, 2, 3, 40, 50])',
    ],
  },
}
