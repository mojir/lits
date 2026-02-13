import type { FunctionReference } from '../..'
import { type LinAlgApiName, getOperatorArgs } from '../../api'

export const linAlgReference: Record<LinAlgApiName, FunctionReference<'Linear Algebra'>> = {
  'Linear-Algebra.reflect': {
    title: 'Linear-Algebra.reflect',
    category: 'Linear Algebra',
    description: 'Reflects a vector across a given axis.',
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
      'let { reflect } = import("Linear-Algebra");\nreflect([1, 2], [0, 1])',
      'let { reflect } = import("Linear-Algebra");\nreflect([1, 2, 3], [0, 0, 1])',
    ],
  },
  'Linear-Algebra.refract': {
    title: 'Linear-Algebra.refract',
    category: 'Linear Algebra',
    description: 'Refracts a vector across a given axis.',
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
      'let { refract } = import("Linear-Algebra");\nrefract([1, 2], [0, 1], 1.5)',
      'let { refract } = import("Linear-Algebra");\nrefract([1, 2, 3], [0, 0, 1], 1.5)',
    ],
  },
  'Linear-Algebra.lerp': {
    title: 'Linear-Algebra.lerp',
    category: 'Linear Algebra',
    description: 'Performs linear interpolation between two vectors.',
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
      'let { lerp } = import("Linear-Algebra");\nlerp([1, 2], [3, 4], 0.5)',
      'let { lerp } = import("Linear-Algebra");\nlerp([1, 2], [3, 4], 2)',
      'let { lerp } = import("Linear-Algebra");\nlerp([1, 2], [3, 4], -1)',
      'let { lerp } = import("Linear-Algebra");\nlerp([1, 2, 3], [4, 5, 6], 0.25)',
    ],
  },
  'Linear-Algebra.rotate2d': {
    title: 'Linear-Algebra.rotate2d',
    category: 'Linear Algebra',
    description: 'Rotates a 2D vector by a given angle in radians.',
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
      'let { rotate2d } = import("Linear-Algebra");\nrotate2d([1, 0], PI / 2)',
      'let { rotate2d } = import("Linear-Algebra");\nrotate2d([0, 1], PI)',
    ],
  },
  'Linear-Algebra.rotate3d': {
    title: 'Linear-Algebra.rotate3d',
    category: 'Linear Algebra',
    description: 'Rotates a 3D vector around a given axis by a given angle in radians.',
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
      'let { rotate3d } = import("Linear-Algebra");\nrotate3d([1, 0, 0], [0, 1, 0], PI / 2)',
      'let { rotate3d } = import("Linear-Algebra");\nrotate3d([0, 1, 0], [1, 0, 0], PI)',
    ],
  },
  'Linear-Algebra.dot': {
    title: 'Linear-Algebra.dot',
    category: 'Linear Algebra',
    description: 'Calculates the dot product of two vectors. The result is a scalar.',
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
      'let { dot } = import("Linear-Algebra");\ndot([1, 2], [3, 4])',
      'let { dot } = import("Linear-Algebra");\ndot([1, 2, 3], [4, 5, 6])',
    ],
  },
  'Linear-Algebra.cross': {
    title: 'Linear-Algebra.cross',
    category: 'Linear Algebra',
    description: 'Calculates the cross product of two 3D vectors. The result is a vector perpendicular to both input vectors.',
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
      'let { cross } = import("Linear-Algebra");\ncross([1, 2, 3], [4, 5, 6])',
      'let { cross } = import("Linear-Algebra");\ncross([1, 0, 0], [0, 1, 0])',
      'let { cross } = import("Linear-Algebra");\ncross([0, 0, 1], [1, 0, 0])',
      'let { cross } = import("Linear-Algebra");\ncross([1, 2, 3], [0, 0, 0])',
      'let { cross } = import("Linear-Algebra");\ncross([0, 0, 0], [1, 2, 3])',
    ],
  },
  'Linear-Algebra.normalize-minmax': {
    title: 'Linear-Algebra.normalize-minmax',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using min-max normalization. The result is a vector with values between 0 and 1.',
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
      'let { normalize-minmax } = import("Linear-Algebra");\nnormalize-minmax([1, 2, 3])',
      'let { normalize-minmax } = import("Linear-Algebra");\nnormalize-minmax([1, 2, -3])',
      'let { normalize-minmax } = import("Linear-Algebra");\nnormalize-minmax([1, 2, 3, 4])',
      'let { normalize-minmax } = import("Linear-Algebra");\nnormalize-minmax([1, 2, -3, 4])',
      'let { normalize-minmax } = import("Linear-Algebra");\nnormalize-minmax([1, 2, 3, 40, 50])',
    ],
  },
  'Linear-Algebra.normalize-zscore': {
    title: 'Linear-Algebra.normalize-zscore',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using z-score normalization. The result is a vector with mean 0 and standard deviation 1.',
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
      'let { normalize-zscore } = import("Linear-Algebra");\nnormalize-zscore([1, 2, 3])',
      'let { normalize-zscore } = import("Linear-Algebra");\nnormalize-zscore([1, 2, -3])',
      'let { normalize-zscore } = import("Linear-Algebra");\nnormalize-zscore([1, 2, 3, 4])',
      'let { normalize-zscore } = import("Linear-Algebra");\nnormalize-zscore([1, 2, -3, 4])',
      'let { normalize-zscore } = import("Linear-Algebra");\nnormalize-zscore([1, 2, 3, 40, 50])',
    ],
  },
  'Linear-Algebra.normalize-robust': {
    title: 'Linear-Algebra.normalize-robust',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using robust normalization. The result is a vector with median 0 and median absolute deviation 1.',
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
      'let { normalize-robust } = import("Linear-Algebra");\nnormalize-robust([1, 2, 3])',
      'let { normalize-robust } = import("Linear-Algebra");\nnormalize-robust([1, 2, -3])',
      'let { normalize-robust } = import("Linear-Algebra");\nnormalize-robust([1, 2, 3, 4])',
      'let { normalize-robust } = import("Linear-Algebra");\nnormalize-robust([1, 2, -3, 4])',
      'let { normalize-robust } = import("Linear-Algebra");\nnormalize-robust([1, 2, 3, 40, 50])',
    ],
  },
  'Linear-Algebra.normalize-l1': {
    title: 'Linear-Algebra.normalize-l1',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using L1 normalization. The result is a vector with L1 norm equal to 1.',
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
      'let { normalize-l1 } = import("Linear-Algebra");\nnormalize-l1([1, 2, 3])',
      'let { normalize-l1 } = import("Linear-Algebra");\nnormalize-l1([1, 2, -3])',
      'let { normalize-l1 } = import("Linear-Algebra");\nnormalize-l1([1, 2, 3, 4])',
      'let { normalize-l1 } = import("Linear-Algebra");\nnormalize-l1([1, 2, -3, 4])',
      'let { normalize-l1 } = import("Linear-Algebra");\nnormalize-l1([1, 2, 3, 40, 50])',
    ],
  },
  'Linear-Algebra.normalize-l2': {
    title: 'Linear-Algebra.normalize-l2',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using L2 normalization. The result is a vector with L2 norm equal to 1.',
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
      'let { normalize-l2 } = import("Linear-Algebra");\nnormalize-l2([1, 2, 3])',
      'let { normalize-l2 } = import("Linear-Algebra");\nnormalize-l2([1, 2, 3])',
      'let { normalize-l2 } = import("Linear-Algebra");\nnormalize-l2([1, 2, -3])',
      'let { normalize-l2 } = import("Linear-Algebra");\nnormalize-l2([1, 2, 3, 4])',
      'let { normalize-l2 } = import("Linear-Algebra");\nnormalize-l2([1, 2, -3, 4])',
      'let { normalize-l2 } = import("Linear-Algebra");\nnormalize-l2([1, 2, 3, 40, 50])',
    ],
  },
  'Linear-Algebra.normalize-log': {
    title: 'Linear-Algebra.normalize-log',
    category: 'Linear Algebra',
    description: 'Normalizes the vector using natural log normalization. The result is a vector with log-transformed values.',
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
      'let { normalize-log } = import("Linear-Algebra");\nnormalize-log([1, 2, 3])',
      'let { normalize-log } = import("Linear-Algebra");\nnormalize-log([1, 2, 3, 4])',
      'let { normalize-log } = import("Linear-Algebra");\nnormalize-log([1, 2, 3, 40, 50])',
    ],
  },
  'Linear-Algebra.angle': {
    title: 'Linear-Algebra.angle',
    category: 'Linear Algebra',
    description: 'Calculates the `angle` between two vectors in radians.',
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
      'let { angle } = import("Linear-Algebra");\nangle([1, 0], [0, 1])',
      'let { angle } = import("Linear-Algebra");\nangle([1, 0, 1], [0, 1, 0])',
    ],
  },
  'Linear-Algebra.projection': {
    title: 'Linear-Algebra.projection',
    category: 'Linear Algebra',
    description: 'Calculates the **projection** of vector `a` onto vector `b`.',
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
      'let { projection } = import("Linear-Algebra");\nprojection([1, 2], [3, 4])',
      'let { projection } = import("Linear-Algebra");\nprojection([1, 2, 3], [4, 5, 6])',
    ],
  },
  'Linear-Algebra.collinear?': {
    title: 'Linear-Algebra.collinear?',
    category: 'Linear Algebra',
    description: 'Checks if two vectors are **collinear**.',
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
      'let { collinear? } = import("Linear-Algebra");\ncollinear?([1, 2], [2, 4])',
      'let { collinear? } = import("Linear-Algebra");\ncollinear?([1, 2], [-2, -4])',
      'let { collinear? } = import("Linear-Algebra");\ncollinear?([1, 2, 3], [2, 4, 6])',
    ],
  },
  'Linear-Algebra.parallel?': {
    title: 'Linear-Algebra.parallel?',
    category: 'Linear Algebra',
    description: 'Checks if two vectors are **parallel**.',
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
      'let { parallel? } = import("Linear-Algebra");\nparallel?([1, 2], [2, 4])',
      'let { parallel? } = import("Linear-Algebra");\nparallel?([1, 2], [-2, -4])',
      'let { parallel? } = import("Linear-Algebra");\nparallel?([1, 2, 3], [2, 4, 6])',
      'let { parallel? } = import("Linear-Algebra");\nparallel?([1, 2], [3, 4])',
    ],
  },
  'Linear-Algebra.orthogonal?': {
    title: 'Linear-Algebra.orthogonal?',
    category: 'Linear Algebra',
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
      'let { orthogonal? } = import("Linear-Algebra");\northogonal?([1, 0], [0, 1])',
      'let { orthogonal? } = import("Linear-Algebra");\northogonal?([1, 0, 1], [0, 1, 0])',
      'let { orthogonal? } = import("Linear-Algebra");\northogonal?([1, 2], [2, -1])',
    ],
  },
  'Linear-Algebra.cosine-similarity': {
    title: 'Linear-Algebra.cosine-similarity',
    category: 'Linear Algebra',
    description: 'Calculates the **cosine similarity** between two vectors. The result is a value between -1 and 1.',
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
      'let { cosine-similarity } = import("Linear-Algebra");\ncosine-similarity([1, 2], [3, 4])',
      'let { cosine-similarity } = import("Linear-Algebra");\ncosine-similarity([1, 2, 3], [4, 5, 6])',
      'let { cosine-similarity } = import("Linear-Algebra");\ncosine-similarity([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.euclidean-distance': {
    title: 'Linear-Algebra.euclidean-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Euclidean distance** between two vectors. The result is a non-negative number.',
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
      'let { euclidean-distance } = import("Linear-Algebra");\neuclidean-distance([1, 2], [3, 4])',
      'let { euclidean-distance } = import("Linear-Algebra");\neuclidean-distance([1, 2, 3], [4, 5, 6])',
      'let { euclidean-distance } = import("Linear-Algebra");\neuclidean-distance([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.euclidean-norm': {
    title: 'Linear-Algebra.euclidean-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Euclidean norm** (L2 norm) of a vector. The result is a non-negative number.',
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
      'let { euclidean-norm } = import("Linear-Algebra");\neuclidean-norm([1, 2])',
      'let { euclidean-norm } = import("Linear-Algebra");\neuclidean-norm([3, 4])',
      'let { euclidean-norm } = import("Linear-Algebra");\neuclidean-norm([1, 2, 3])',
    ],
  },
  'Linear-Algebra.manhattan-distance': {
    title: 'Linear-Algebra.manhattan-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Manhattan distance** between two vectors. The result is a non-negative number.',
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
      'let { manhattan-distance } = import("Linear-Algebra");\nmanhattan-distance([1, 2], [3, 4])',
      'let { manhattan-distance } = import("Linear-Algebra");\nmanhattan-distance([1, 2, 3], [4, 5, 6])',
      'let { manhattan-distance } = import("Linear-Algebra");\nmanhattan-distance([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.manhattan-norm': {
    title: 'Linear-Algebra.manhattan-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Manhattan norm** (L1 norm) of a vector. The result is a non-negative number.',
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
      'let { manhattan-norm } = import("Linear-Algebra");\nmanhattan-norm([1, 2])',
      'let { manhattan-norm } = import("Linear-Algebra");\nmanhattan-norm([3, 4])',
      'let { manhattan-norm } = import("Linear-Algebra");\nmanhattan-norm([1, 2, 3])',
    ],
  },
  'Linear-Algebra.hamming-distance': {
    title: 'Linear-Algebra.hamming-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Hamming distance** between two vectors. The result is a non-negative integer.',
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
      'let { hamming-distance } = import("Linear-Algebra");\nhamming-distance([1, 2], [3, 4])',
      'let { hamming-distance } = import("Linear-Algebra");\nhamming-distance([1, 2, 3], [4, 5, 6])',
      'let { hamming-distance } = import("Linear-Algebra");\nhamming-distance([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.hamming-norm': {
    title: 'Linear-Algebra.hamming-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Hamming norm** of a vector. The result is a non-negative integer.',
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
      'let { hamming-norm } = import("Linear-Algebra");\nhamming-norm([1, 2])',
      'let { hamming-norm } = import("Linear-Algebra");\nhamming-norm([3, 4])',
      'let { hamming-norm } = import("Linear-Algebra");\nhamming-norm([1, 2, 3])',
    ],
  },
  'Linear-Algebra.chebyshev-distance': {
    title: 'Linear-Algebra.chebyshev-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Chebyshev distance** between two vectors. The result is a non-negative number.',
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
      'let { chebyshev-distance } = import("Linear-Algebra");\nchebyshev-distance([1, 2], [3, 4])',
      'let { chebyshev-distance } = import("Linear-Algebra");\nchebyshev-distance([1, 2, 3], [4, 5, 6])',
      'let { chebyshev-distance } = import("Linear-Algebra");\nchebyshev-distance([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.chebyshev-norm': {
    title: 'Linear-Algebra.chebyshev-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Chebyshev norm** of a vector. The result is a non-negative number.',
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
      'let { chebyshev-norm } = import("Linear-Algebra");\nchebyshev-norm([1, 2])',
      'let { chebyshev-norm } = import("Linear-Algebra");\nchebyshev-norm([3, 4])',
      'let { chebyshev-norm } = import("Linear-Algebra");\nchebyshev-norm([1, 2, 3])',
    ],
  },
  'Linear-Algebra.minkowski-distance': {
    title: 'Linear-Algebra.minkowski-distance',
    category: 'Linear Algebra',
    description: 'Calculates the **Minkowski distance** between two vectors. The result is a non-negative number.',
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
      'let { minkowski-distance } = import("Linear-Algebra");\nminkowski-distance([1, 2], [3, 4], 2)',
      'let { minkowski-distance } = import("Linear-Algebra");\nminkowski-distance([1, 2, 3], [4, 5, 6], 3)',
      'let { minkowski-distance } = import("Linear-Algebra");\nminkowski-distance([1, 0], [0, 1], 1)',
    ],
  },
  'Linear-Algebra.minkowski-norm': {
    title: 'Linear-Algebra.minkowski-norm',
    category: 'Linear Algebra',
    description: 'Calculates the **Minkowski norm** of a vector. The result is a non-negative number.',
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
      'let { minkowski-norm } = import("Linear-Algebra");\nminkowski-norm([1, 2], 2)',
      'let { minkowski-norm } = import("Linear-Algebra");\nminkowski-norm([3, 4], 3)',
      'let { minkowski-norm } = import("Linear-Algebra");\nminkowski-norm([1, 2, 3], 4)',
    ],
  },
  'Linear-Algebra.cov': {
    title: 'Linear-Algebra.cov',
    category: 'Linear Algebra',
    description: 'Calculates the **covariance** between two vectors. The result is a number.',
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
      'let { cov } = import("Linear-Algebra");\ncov([1, 2], [3, 4])',
      'let { cov } = import("Linear-Algebra");\ncov([1, 2, 3], [4, 5, 6])',
      'let { cov } = import("Linear-Algebra");\ncov([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.corr': {
    title: 'Linear-Algebra.corr',
    category: 'Linear Algebra',
    description: 'Calculates the **correlation** between two vectors. The result is a number between -1 and 1.',
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
      'let { corr } = import("Linear-Algebra");\ncorr([1, 2], [3, 4])',
      'let { corr } = import("Linear-Algebra");\ncorr([1, 2, 3], [4, 5, 6])',
      'let { corr } = import("Linear-Algebra");\ncorr([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.spearman-corr': {
    title: 'Linear-Algebra.spearman-corr',
    category: 'Linear Algebra',
    description: 'Calculates the **Spearman rank correlation** between two vectors. The result is a number between -1 and 1.',
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
      'let { spearman-corr } = import("Linear-Algebra");\nspearman-corr([1, 2], [3, 4])',
      'let { spearman-corr } = import("Linear-Algebra");\nspearman-corr([1, 2, 3], [4, 5, 6])',
      'let { spearman-corr } = import("Linear-Algebra");\nspearman-corr([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.pearson-corr': {
    title: 'Linear-Algebra.pearson-corr',
    category: 'Linear Algebra',
    description: 'Calculates the **Pearson correlation** between two vectors. The result is a number between -1 and 1.',
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
      'let { pearson-corr } = import("Linear-Algebra");\npearson-corr([1, 2], [3, 4])',
      'let { pearson-corr } = import("Linear-Algebra");\npearson-corr([1, 2, 3], [4, 5, 6])',
      'let { pearson-corr } = import("Linear-Algebra");\npearson-corr([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.kendall-tau': {
    title: 'Linear-Algebra.kendall-tau',
    category: 'Linear Algebra',
    description: 'Calculates the **Kendall Tau** rank correlation coefficient between two vectors. The result is a number between -1 and 1.',
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
      'let { kendall-tau } = import("Linear-Algebra");\nkendall-tau([1, 2], [3, 4])',
      'let { kendall-tau } = import("Linear-Algebra");\nkendall-tau([1, 2, 3], [4, 5, 6])',
      'let { kendall-tau } = import("Linear-Algebra");\nkendall-tau([1, 0], [0, 1])',
    ],
  },
  'Linear-Algebra.autocorrelation': {
    title: 'Linear-Algebra.autocorrelation',
    category: 'Linear Algebra',
    description: 'Calculates the **autocorrelation** of a vector. The result is a vector of autocorrelation coefficients.',
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
      'let { autocorrelation } = import("Linear-Algebra");\nautocorrelation([1, 2, 3], -2)',
      'let { autocorrelation } = import("Linear-Algebra");\nautocorrelation([1, 2, 3], -1)',
      'let { autocorrelation } = import("Linear-Algebra");\nautocorrelation([1, 2, 3], 0)',
      'let { autocorrelation } = import("Linear-Algebra");\nautocorrelation([1, 2, 3], 1)',
      'let { autocorrelation } = import("Linear-Algebra");\nautocorrelation([1, 2, 3], 2)',
    ],
  },
  'Linear-Algebra.cross-correlation': {
    title: 'Linear-Algebra.cross-correlation',
    category: 'Linear Algebra',
    description: 'Calculates the **cross-correlation** between two vectors. The result is a vector of cross-correlation coefficients.',
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
      'let { cross-correlation } = import("Linear-Algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], -2)',
      'let { cross-correlation } = import("Linear-Algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], -1)',
      'let { cross-correlation } = import("Linear-Algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], 0)',
      'let { cross-correlation } = import("Linear-Algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], 1)',
      'let { cross-correlation } = import("Linear-Algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], 2)',
    ],
  },
  'Linear-Algebra.rref': {
    title: 'Linear-Algebra.rref',
    category: 'Linear Algebra',
    description: 'Calculates the **Reduced Row Echelon Form** (RREF) of a matrix.',
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
      'let { rref } = import("Linear-Algebra");\nrref([[1, 2], [3, 4]])',
      'let { rref } = import("Linear-Algebra");\nrref([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { rref } = import("Linear-Algebra");\nrref([[1, 2, 3], [7, 8, 9], [4, 5, 7]])',
    ],
  },
  'Linear-Algebra.solve': {
    title: 'Linear-Algebra.solve',
    category: 'Linear Algebra',
    description: 'Solves a system of linear equations represented by a matrix and a vector.',
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
      `let { solve } = import("Linear-Algebra");
solve([
  [2, 1, -1, 1], 
  [4, 5, -3, 2], 
  [6, -2, 5, -3], 
  [8, 3, 2, 4]
], [5, 10, 2, 17])`,
      'let { solve } = import("Linear-Algebra"); solve([[2, 0, 0], [3, 1, 0], [4, 5, 6]], [4, 5, 38])',
      'let { solve } = import("Linear-Algebra"); solve([[2, 3], [1, -1]], [8, 2])',
    ],
  },
  'Linear-Algebra.to-polar': {
    title: 'Linear-Algebra.to-polar',
    category: 'Linear Algebra',
    description: 'Converts a 2D vector to polar coordinates.',
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
      'let { to-polar } = import("Linear-Algebra");\nto-polar([1, 2])',
      'let { to-polar } = import("Linear-Algebra");\nto-polar([3, 4])',
    ],
  },
  'Linear-Algebra.from-polar': {
    title: 'Linear-Algebra.from-polar',
    category: 'Linear Algebra',
    description: 'Converts polar coordinates to a 2D vector.',
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
      'let { from-polar } = import("Linear-Algebra");\nfrom-polar([1, PI / 4])',
      'let { from-polar } = import("Linear-Algebra");\nfrom-polar([1, 0])',
      'let { from-polar } = import("Linear-Algebra");\nfrom-polar([1, -PI / 2])',
    ],
  },
}
