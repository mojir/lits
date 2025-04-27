import type { FunctionReference } from '../..'
import { type LinAlgApiName, getOperatorArgs } from '../../api'

export const linAlgReference: Record<LinAlgApiName, FunctionReference<'Linear Algebra'>> = {
  'lin:reflect': {
    title: 'lin:reflect',
    category: 'Linear Algebra',
    description: 'Reflects a vector across a given axis.',
    linkName: 'lin-colon-reflect',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
        description: 'Vector to reflect.',
      },
      b: {
        type: 'vector',
        description: 'Axis of reflection.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:reflect([1, 2], [0, 1])',
      'lin:reflect([1, 2, 3], [0, 0, 1])',
    ],
  },
  'lin:refract': {
    title: 'lin:refract',
    category: 'Linear Algebra',
    description: 'Refracts a vector across a given axis.',
    linkName: 'lin-colon-refract',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to refract.',
      },
      axis: {
        type: 'vector',
        description: 'Axis of refraction.',
      },
      eta: {
        type: 'number',
        description: 'Refraction index.',
      },
    },
    variants: [
      { argumentNames: ['vector', 'axis', 'eta'] },
    ],
    examples: [
      'lin:refract([1, 2], [0, 1], 1.5)',
      'lin:refract([1, 2, 3], [0, 0, 1], 1.5)',
    ],
  },
  'lin:lerp': {
    title: 'lin:lerp',
    category: 'Linear Algebra',
    description: 'Performs linear interpolation between two vectors.',
    linkName: 'lin-colon-lerp',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
        description: 'Start vector.',
      },
      b: {
        type: 'vector',
        description: 'End vector.',
      },
      t: {
        type: 'number',
        description: 'Interpolation factor (0 to 1).',
      },
    },
    variants: [
      { argumentNames: ['a', 'b', 't'] },
    ],
    examples: [
      'lin:lerp([1, 2], [3, 4], 0.5)',
      'lin:lerp([1, 2], [3, 4], 2)',
      'lin:lerp([1, 2], [3, 4], -1)',
      'lin:lerp([1, 2, 3], [4, 5, 6], 0.25)',
    ],
  },
  'lin:rotate2d': {
    title: 'lin:rotate2d',
    category: 'Linear Algebra',
    description: 'Rotates a 2D vector by a given angle in radians.',
    linkName: 'lin-colon-rotate2d',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
        description: 'Vector to rotate.',
      },
      b: {
        type: 'number',
        description: 'Angle in b.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:rotate2d([1, 0], PI / 2)',
      'lin:rotate2d([0, 1], PI)',
    ],
  },
  'lin:rotate3d': {
    title: 'lin:rotate3d',
    category: 'Linear Algebra',
    description: 'Rotates a 3D vector around a given axis by a given angle in radians.',
    linkName: 'lin-colon-rotate3d',
    returns: {
      type: 'vector',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to rotate.',
      },
      axis: {
        type: 'vector',
        description: 'Axis of rotation.',
      },
      radians: {
        type: 'number',
        description: 'Angle in radians.',
      },
    },
    variants: [
      { argumentNames: ['v', 'axis', 'radians'] },
    ],
    examples: [
      'lin:rotate3d([1, 0, 0], [0, 1, 0], PI / 2)',
      'lin:rotate3d([0, 1, 0], [1, 0, 0], PI)',
    ],
  },
  'lin:dot': {
    title: 'lin:dot',
    category: 'Linear Algebra',
    description: 'Calculates the dot product of two vectors. The result is a scalar.',
    linkName: 'lin-colon-dot',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'First vector.',
      },
      b: {
        type: 'vector',
        description: 'Second vector.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
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
    linkName: 'lin-colon-cross',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
        description: 'First vector (3D).',
      },
      b: {
        type: 'vector',
        description: 'Second vector (3D).',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
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
    linkName: 'lin-colon-normalize-minmax',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
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
    linkName: 'lin-colon-normalize-zscore',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
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
    linkName: 'lin-colon-normalize-robust',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
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
    linkName: 'lin-colon-normalize-l1',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
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
    linkName: 'lin-colon-normalize-l2',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
    ],
    examples: [
      'lin:normalize-l2([1, 2, 3])',
      'lin:unit([1, 2, 3])',
      'lin:normalize-l2([1, 2, -3])',
      'lin:normalize-l2([1, 2, 3, 4])',
      'lin:normalize-l2([1, 2, -3, 4])',
      'lin:normalize-l2([1, 2, 3, 40, 50])',
    ],
    aliases: [
      'lin:unit',
      'lin:normalize',
    ],
  },
  'lin:normalize-log': {
    title: 'lin:normalize-log',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using natural log normalization. The result is a vector with log-transformed values.',
    linkName: 'lin-colon-normalize-log',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to normalize.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
    ],
    examples: [
      'lin:normalize-log([1, 2, 3])',
      'lin:normalize-log([1, 2, 3, 4])',
      'lin:normalize-log([1, 2, 3, 40, 50])',
    ],
  },
  'lin:angle': {
    title: 'lin:angle',
    category: 'Linear Algebra',
    description: 'Calculates the `angle` between two vectors in radians.',
    linkName: 'lin-colon-angle',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:angle([1, 0], [0, 1])',
      'lin:angle([1, 0, 1], [0, 1, 0])',
    ],
  },
  'lin:projection': {
    title: 'lin:projection',
    category: 'Linear Algebra',
    description: 'Calculates the **projection** of vector `a` onto vector `b`.',
    linkName: 'lin-colon-projection',
    returns: {
      type: 'vector',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:projection([1, 2], [3, 4])',
      'lin:projection([1, 2, 3], [4, 5, 6])',
    ],
  },
  'lin:collinear?': {
    title: 'lin:collinear?',
    category: 'Linear Algebra',
    description: 'Checks if two vectors are **collinear**.',
    linkName: 'lin-colon-collinear-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:collinear?([1, 2], [2, 4])',
      'lin:collinear?([1, 2], [-2, -4])',
      'lin:collinear?([1, 2, 3], [2, 4, 6])',
    ],
  },
  'lin:parallel?': {
    title: 'lin:parallel?',
    category: 'Linear Algebra',
    description: 'Checks if two vectors are **parallel**.',
    linkName: 'lin-colon-parallel-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:parallel?([1, 2], [2, 4])',
      'lin:parallel?([1, 2], [-2, -4])',
      'lin:parallel?([1, 2, 3], [2, 4, 6])',
      'lin:parallel?([1, 2], [3, 4])',
    ],
  },
  'lin:orthogonal?': {
    title: 'lin:orthogonal?',
    category: 'Linear Algebra',
    linkName: 'lin-colon-orthogonal-question',
    returns: {
      type: 'boolean',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Checks if two vectors are **orthogonal**.',
    examples: [
      'lin:orthogonal?([1, 0], [0, 1])',
      'lin:orthogonal?([1, 0, 1], [0, 1, 0])',
      'lin:orthogonal?([1, 2], [2, -1])',
    ],
  },
  'lin:cosine-similarity': {
    title: 'lin:cosine-similarity',
    category: 'Linear Algebra',
    description: 'Calculates the **cosine similarity** between two vectors. The result is a value between -1 and 1.',
    linkName: 'lin-colon-cosine-similarity',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:cosine-similarity([1, 2], [3, 4])',
      'lin:cosine-similarity([1, 2, 3], [4, 5, 6])',
      'lin:cosine-similarity([1, 0], [0, 1])',
    ],
  },
  'lin:euclidean-distance': {
    title: 'lin:euclidean-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Euclidean distance** between two vectors. The result is a non-negative number.',
    linkName: 'lin-colon-euclidean-distance',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:euclidean-distance([1, 2], [3, 4])',
      'lin:euclidean-distance([1, 2, 3], [4, 5, 6])',
      'lin:euclidean-distance([1, 0], [0, 1])',
    ],
    aliases: [
      'lin:distance',
      'lin:l2-distance',
    ],
  },
  'lin:euclidean-norm': {
    title: 'lin:euclidean-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Euclidean norm** (L2 norm) of a vector. The result is a non-negative number.',
    linkName: 'lin-colon-euclidean-norm',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to calculate the norm for.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
    ],
    examples: [
      'lin:euclidean-norm([1, 2])',
      'lin:euclidean-norm([3, 4])',
      'lin:euclidean-norm([1, 2, 3])',
    ],
    aliases: [
      'lin:l2-norm',
      'lin:length',
    ],
  },
  'lin:manhattan-distance': {
    title: 'lin:manhattan-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Manhattan distance** between two vectors. The result is a non-negative number.',
    linkName: 'lin-colon-manhattan-distance',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:manhattan-distance([1, 2], [3, 4])',
      'lin:manhattan-distance([1, 2, 3], [4, 5, 6])',
      'lin:manhattan-distance([1, 0], [0, 1])',
    ],
    aliases: [
      'lin:l1-distance',
      'lin:cityblock-distance',
    ],
  },
  'lin:manhattan-norm': {
    title: 'lin:manhattan-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Manhattan norm** (L1 norm) of a vector. The result is a non-negative number.',
    linkName: 'lin-colon-manhattan-norm',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to calculate the norm for.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
    ],
    examples: [
      'lin:manhattan-norm([1, 2])',
      'lin:manhattan-norm([3, 4])',
      'lin:manhattan-norm([1, 2, 3])',
    ],
    aliases: [
      'lin:l1-norm',
      'lin:cityblock-norm',
    ],
  },
  'lin:hamming-distance': {
    title: 'lin:hamming-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Hamming distance** between two vectors. The result is a non-negative integer.',
    linkName: 'lin-colon-hamming-distance',
    returns: {
      type: 'integer',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:hamming-distance([1, 2], [3, 4])',
      'lin:hamming-distance([1, 2, 3], [4, 5, 6])',
      'lin:hamming-distance([1, 0], [0, 1])',
    ],
  },
  'lin:hamming-norm': {
    title: 'lin:hamming-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Hamming norm** of a vector. The result is a non-negative integer.',
    linkName: 'lin-colon-hamming-norm',
    returns: {
      type: 'integer',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to calculate the norm for.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
    ],
    examples: [
      'lin:hamming-norm([1, 2])',
      'lin:hamming-norm([3, 4])',
      'lin:hamming-norm([1, 2, 3])',
    ],
  },
  'lin:chebyshev-distance': {
    title: 'lin:chebyshev-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Chebyshev distance** between two vectors. The result is a non-negative number.',
    linkName: 'lin-colon-chebyshev-distance',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:chebyshev-distance([1, 2], [3, 4])',
      'lin:chebyshev-distance([1, 2, 3], [4, 5, 6])',
      'lin:chebyshev-distance([1, 0], [0, 1])',
    ],
  },
  'lin:chebyshev-norm': {
    title: 'lin:chebyshev-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Chebyshev norm** of a vector. The result is a non-negative number.',
    linkName: 'lin-colon-chebyshev-norm',
    returns: {
      type: 'number',
    },
    args: {
      v: {
        type: 'vector',
        description: 'Vector to calculate the norm for.',
      },
    },
    variants: [
      { argumentNames: ['v'] },
    ],
    examples: [
      'lin:chebyshev-norm([1, 2])',
      'lin:chebyshev-norm([3, 4])',
      'lin:chebyshev-norm([1, 2, 3])',
    ],
  },
  'lin:minkowski-distance': {
    title: 'lin:minkowski-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Minkowski distance** between two vectors. The result is a non-negative number.',
    linkName: 'lin-colon-minkowski-distance',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
      p: {
        type: 'number',
        description: 'Order of the norm (p).',
      },
    },
    variants: [
      { argumentNames: ['a', 'b', 'p'] },
    ],
    examples: [
      'lin:minkowski-distance([1, 2], [3, 4], 2)',
      'lin:minkowski-distance([1, 2, 3], [4, 5, 6], 3)',
      'lin:minkowski-distance([1, 0], [0, 1], 1)',
    ],
  },
  'lin:minkowski-norm': {
    title: 'lin:minkowski-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Minkowski norm** of a vector. The result is a non-negative number.',
    linkName: 'lin-colon-minkowski-norm',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'Vector to calculate the norm for.',
      },
      b: {
        type: 'number',
        description: 'Order of the norm (p).',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:minkowski-norm([1, 2], 2)',
      'lin:minkowski-norm([3, 4], 3)',
      'lin:minkowski-norm([1, 2, 3], 4)',
    ],
  },
  'lin:cov': {
    title: 'lin:cov',
    category: 'Linear Algebra',
    description: 'Calculates the **covariance** between two vectors. The result is a number.',
    linkName: 'lin-colon-cov',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:cov([1, 2], [3, 4])',
      'lin:cov([1, 2, 3], [4, 5, 6])',
      'lin:cov([1, 0], [0, 1])',
    ],
  },
  'lin:corr': {
    title: 'lin:corr',
    category: 'Linear Algebra',
    description: 'Calculates the **correlation** between two vectors. The result is a number between -1 and 1.',
    linkName: 'lin-colon-corr',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:corr([1, 2], [3, 4])',
      'lin:corr([1, 2, 3], [4, 5, 6])',
      'lin:corr([1, 0], [0, 1])',
    ],
  },
  'lin:spearman-corr': {
    title: 'lin:spearman-corr',
    category: 'Linear Algebra',
    description: 'Calculates the **Spearman rank correlation** between two vectors. The result is a number between -1 and 1.',
    linkName: 'lin-colon-spearman-corr',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:spearman-corr([1, 2], [3, 4])',
      'lin:spearman-corr([1, 2, 3], [4, 5, 6])',
      'lin:spearman-corr([1, 0], [0, 1])',
    ],
    aliases: ['lin:spearman-rho'],
  },
  'lin:pearson-corr': {
    title: 'lin:pearson-corr',
    category: 'Linear Algebra',
    description: 'Calculates the **Pearson correlation** between two vectors. The result is a number between -1 and 1.',
    linkName: 'lin-colon-pearson-corr',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:pearson-corr([1, 2], [3, 4])',
      'lin:pearson-corr([1, 2, 3], [4, 5, 6])',
      'lin:pearson-corr([1, 0], [0, 1])',
    ],
  },
  'lin:kendall-tau': {
    title: 'lin:kendall-tau',
    category: 'Linear Algebra',
    description: 'Calculates the **Kendall Tau** rank correlation coefficient between two vectors. The result is a number between -1 and 1.',
    linkName: 'lin-colon-kendall-tau',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:kendall-tau([1, 2], [3, 4])',
      'lin:kendall-tau([1, 2, 3], [4, 5, 6])',
      'lin:kendall-tau([1, 0], [0, 1])',
    ],
  },
  'lin:autocorrelation': {
    title: 'lin:autocorrelation',
    category: 'Linear Algebra',
    description: 'Calculates the **autocorrelation** of a vector. The result is a vector of autocorrelation coefficients.',
    linkName: 'lin-colon-autocorrelation',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
        description: 'Vector to calculate the autocorrelation for.',
      },
      b: {
        type: 'integer',
        description: 'Lag value for the autocorrelation.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      'lin:autocorrelation([1, 2, 3], -2)',
      'lin:autocorrelation([1, 2, 3], -1)',
      'lin:autocorrelation([1, 2, 3], 0)',
      'lin:autocorrelation([1, 2, 3], 1)',
      'lin:autocorrelation([1, 2, 3], 2)',
    ],
    aliases: ['lin:acf'],
  },
  'lin:cross-correlation': {
    title: 'lin:cross-correlation',
    category: 'Linear Algebra',
    description: 'Calculates the **cross-correlation** between two vectors. The result is a vector of cross-correlation coefficients.',
    linkName: 'lin-colon-cross-correlation',
    returns: {
      type: 'vector',
    },
    args: {
      ...getOperatorArgs('vector', 'vector'),
      lag: {
        type: 'integer',
        description: 'Lag value for the cross-correlation.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b', 'lag'] },
    ],
    examples: [
      'lin:cross-correlation([1, 2, 3], [4, 5, 6], -2)',
      'lin:cross-correlation([1, 2, 3], [4, 5, 6], -1)',
      'lin:cross-correlation([1, 2, 3], [4, 5, 6], 0)',
      'lin:cross-correlation([1, 2, 3], [4, 5, 6], 1)',
      'lin:cross-correlation([1, 2, 3], [4, 5, 6], 2)',
    ],
    aliases: ['lin:ccf'],
  },
  'lin:rref': {
    title: 'lin:rref',
    category: 'Linear Algebra',
    description: 'Calculates the **Reduced Row Echelon Form** (RREF) of a matrix.',
    linkName: 'lin-colon-rref',
    returns: {
      type: 'matrix',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'Matrix to calculate the RREF for.',
      },
    },
    variants: [
      { argumentNames: ['m'] },
    ],
    examples: [
      'lin:rref([[1, 2], [3, 4]])',
      'lin:rref([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'lin:rref([[1, 2, 3], [7, 8, 9], [4, 5, 7]])',
    ],
  },
  'lin:solve': {
    title: 'lin:solve',
    category: 'Linear Algebra',
    description: 'Solves a system of linear equations represented by a matrix and a vector.',
    linkName: 'lin-colon-solve',
    returns: {
      type: 'vector',
    },
    args: {
      ...getOperatorArgs('matrix', 'vector'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    examples: [
      `lin:solve([
  [2, 1, -1, 1], 
  [4, 5, -3, 2], 
  [6, -2, 5, -3], 
  [8, 3, 2, 4]
], [5, 10, 2, 17])`,
      'lin:solve([[2, 0, 0], [3, 1, 0], [4, 5, 6]], [4, 5, 38])',
      'lin:solve([[2, 3], [1, -1]], [8, 2])',
    ],
  },
  'lin:to-polar': {
    title: 'lin:to-polar',
    category: 'Linear Algebra',
    description: 'Converts a 2D vector to polar coordinates.',
    linkName: 'lin-colon-to-polar',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: '2D Vector to convert.',
      },
    },
    variants: [
      { argumentNames: ['vector'] },
    ],
    examples: [
      'lin:to-polar([1, 2])',
      'lin:to-polar([3, 4])',
    ],
  },
  'lin:from-polar': {
    title: 'lin:from-polar',
    category: 'Linear Algebra',
    description: 'Converts polar coordinates to a 2D vector.',
    linkName: 'lin-colon-from-polar',
    returns: {
      type: 'vector',
    },
    args: {
      polar: {
        type: 'vector',
        description: 'Polar coordinates to convert.',
      },
    },
    variants: [
      { argumentNames: ['polar'] },
    ],
    examples: [
      'lin:from-polar([1, PI / 4])',
      'lin:from-polar([1, 0])',
      'lin:from-polar([1, -PI / 2])',
    ],
  },
}
