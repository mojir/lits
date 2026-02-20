import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'reflect': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { reflect } = import("linear-algebra");\nreflect([1, 2], [0, 1])',
      'let { reflect } = import("linear-algebra");\nreflect([1, 2, 3], [0, 0, 1])',
    ],
    seeAlso: ['linear-algebra.refract', 'linear-algebra.projection'],
  },
  'refract': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'vector',
          'axis',
          'eta',
        ],
      },
    ],
    examples: [
      'let { refract } = import("linear-algebra");\nrefract([1, 2], [0, 1], 1.5)',
      'let { refract } = import("linear-algebra");\nrefract([1, 2, 3], [0, 0, 1], 1.5)',
    ],
    seeAlso: ['linear-algebra.reflect'],
  },
  'lerp': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
          't',
        ],
      },
    ],
    examples: [
      'let { lerp } = import("linear-algebra");\nlerp([1, 2], [3, 4], 0.5)',
      'let { lerp } = import("linear-algebra");\nlerp([1, 2], [3, 4], 2)',
      'let { lerp } = import("linear-algebra");\nlerp([1, 2], [3, 4], -1)',
      'let { lerp } = import("linear-algebra");\nlerp([1, 2, 3], [4, 5, 6], 0.25)',
    ],
    seeAlso: ['linear-algebra.projection'],
  },
  'rotate2d': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { rotate2d } = import("linear-algebra");\nrotate2d([1, 0], PI / 2)',
      'let { rotate2d } = import("linear-algebra");\nrotate2d([0, 1], PI)',
    ],
    seeAlso: ['linear-algebra.rotate3d', 'linear-algebra.angle'],
  },
  'rotate3d': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
          'axis',
          'radians',
        ],
      },
    ],
    examples: [
      'let { rotate3d } = import("linear-algebra");\nrotate3d([1, 0, 0], [0, 1, 0], PI / 2)',
      'let { rotate3d } = import("linear-algebra");\nrotate3d([0, 1, 0], [1, 0, 0], PI)',
    ],
    seeAlso: ['linear-algebra.rotate2d', 'linear-algebra.angle'],
  },
  'dot': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { dot } = import("linear-algebra");\ndot([1, 2], [3, 4])',
      'let { dot } = import("linear-algebra");\ndot([1, 2, 3], [4, 5, 6])',
    ],
    seeAlso: ['linear-algebra.cross', 'linear-algebra.cosine-similarity', 'linear-algebra.angle', 'linear-algebra.projection', 'linear-algebra.orthogonal?'],
  },
  'cross': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { cross } = import("linear-algebra");\ncross([1, 2, 3], [4, 5, 6])',
      'let { cross } = import("linear-algebra");\ncross([1, 0, 0], [0, 1, 0])',
      'let { cross } = import("linear-algebra");\ncross([0, 0, 1], [1, 0, 0])',
      'let { cross } = import("linear-algebra");\ncross([1, 2, 3], [0, 0, 0])',
      'let { cross } = import("linear-algebra");\ncross([0, 0, 0], [1, 2, 3])',
    ],
    seeAlso: ['linear-algebra.dot'],
  },
  'normalize-minmax': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { normalize-minmax } = import("linear-algebra");\nnormalize-minmax([1, 2, 3])',
      'let { normalize-minmax } = import("linear-algebra");\nnormalize-minmax([1, 2, -3])',
      'let { normalize-minmax } = import("linear-algebra");\nnormalize-minmax([1, 2, 3, 4])',
      'let { normalize-minmax } = import("linear-algebra");\nnormalize-minmax([1, 2, -3, 4])',
      'let { normalize-minmax } = import("linear-algebra");\nnormalize-minmax([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['linear-algebra.normalize-zscore', 'linear-algebra.normalize-robust', 'linear-algebra.normalize-l1', 'linear-algebra.normalize-l2', 'linear-algebra.normalize-log'],
  },
  'normalize-zscore': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { normalize-zscore } = import("linear-algebra");\nnormalize-zscore([1, 2, 3])',
      'let { normalize-zscore } = import("linear-algebra");\nnormalize-zscore([1, 2, -3])',
      'let { normalize-zscore } = import("linear-algebra");\nnormalize-zscore([1, 2, 3, 4])',
      'let { normalize-zscore } = import("linear-algebra");\nnormalize-zscore([1, 2, -3, 4])',
      'let { normalize-zscore } = import("linear-algebra");\nnormalize-zscore([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['linear-algebra.normalize-minmax', 'linear-algebra.normalize-robust', 'linear-algebra.normalize-l1', 'linear-algebra.normalize-l2', 'linear-algebra.normalize-log'],
  },
  'normalize-robust': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { normalize-robust } = import("linear-algebra");\nnormalize-robust([1, 2, 3])',
      'let { normalize-robust } = import("linear-algebra");\nnormalize-robust([1, 2, -3])',
      'let { normalize-robust } = import("linear-algebra");\nnormalize-robust([1, 2, 3, 4])',
      'let { normalize-robust } = import("linear-algebra");\nnormalize-robust([1, 2, -3, 4])',
      'let { normalize-robust } = import("linear-algebra");\nnormalize-robust([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['linear-algebra.normalize-minmax', 'linear-algebra.normalize-zscore'],
  },
  'normalize-l1': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { normalize-l1 } = import("linear-algebra");\nnormalize-l1([1, 2, 3])',
      'let { normalize-l1 } = import("linear-algebra");\nnormalize-l1([1, 2, -3])',
      'let { normalize-l1 } = import("linear-algebra");\nnormalize-l1([1, 2, 3, 4])',
      'let { normalize-l1 } = import("linear-algebra");\nnormalize-l1([1, 2, -3, 4])',
      'let { normalize-l1 } = import("linear-algebra");\nnormalize-l1([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['linear-algebra.normalize-l2', 'linear-algebra.normalize-minmax', 'linear-algebra.manhattan-norm', 'linear-algebra.normalize-zscore'],
  },
  'normalize-l2': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { normalize-l2 } = import("linear-algebra");\nnormalize-l2([1, 2, 3])',
      'let { normalize-l2 } = import("linear-algebra");\nnormalize-l2([1, 2, 3])',
      'let { normalize-l2 } = import("linear-algebra");\nnormalize-l2([1, 2, -3])',
      'let { normalize-l2 } = import("linear-algebra");\nnormalize-l2([1, 2, 3, 4])',
      'let { normalize-l2 } = import("linear-algebra");\nnormalize-l2([1, 2, -3, 4])',
      'let { normalize-l2 } = import("linear-algebra");\nnormalize-l2([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['linear-algebra.normalize-l1', 'linear-algebra.normalize-minmax', 'linear-algebra.euclidean-norm', 'linear-algebra.normalize-zscore'],
  },
  'normalize-log': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { normalize-log } = import("linear-algebra");\nnormalize-log([1, 2, 3])',
      'let { normalize-log } = import("linear-algebra");\nnormalize-log([1, 2, 3, 4])',
      'let { normalize-log } = import("linear-algebra");\nnormalize-log([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['linear-algebra.normalize-minmax', 'linear-algebra.normalize-zscore'],
  },
  'angle': {
    category: 'linear-algebra',
    description: 'Calculates the **angle** between two vectors in radians.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { angle } = import("linear-algebra");\nangle([1, 0], [0, 1])',
      'let { angle } = import("linear-algebra");\nangle([1, 0, 1], [0, 1, 0])',
    ],
    seeAlso: ['linear-algebra.dot', 'linear-algebra.collinear?', 'linear-algebra.orthogonal?', 'linear-algebra.rotate2d', 'linear-algebra.rotate3d', 'linear-algebra.parallel?', 'linear-algebra.cosine-similarity', 'linear-algebra.to-polar'],
  },
  'projection': {
    category: 'linear-algebra',
    description: 'Calculates the **projection** of vector `a` onto vector `b`.',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { projection } = import("linear-algebra");\nprojection([1, 2], [3, 4])',
      'let { projection } = import("linear-algebra");\nprojection([1, 2, 3], [4, 5, 6])',
    ],
    seeAlso: ['linear-algebra.dot', 'linear-algebra.reflect', 'linear-algebra.lerp'],
  },
  'collinear?': {
    category: 'linear-algebra',
    description: 'Checks if two vectors are **collinear**.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { collinear? } = import("linear-algebra");\ncollinear?([1, 2], [2, 4])',
      'let { collinear? } = import("linear-algebra");\ncollinear?([1, 2], [-2, -4])',
      'let { collinear? } = import("linear-algebra");\ncollinear?([1, 2, 3], [2, 4, 6])',
    ],
    seeAlso: ['linear-algebra.parallel?', 'linear-algebra.orthogonal?', 'linear-algebra.angle'],
  },
  'parallel?': {
    category: 'linear-algebra',
    description: 'Checks if two vectors are **parallel**.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { parallel? } = import("linear-algebra");\nparallel?([1, 2], [2, 4])',
      'let { parallel? } = import("linear-algebra");\nparallel?([1, 2], [-2, -4])',
      'let { parallel? } = import("linear-algebra");\nparallel?([1, 2, 3], [2, 4, 6])',
      'let { parallel? } = import("linear-algebra");\nparallel?([1, 2], [3, 4])',
    ],
    seeAlso: ['linear-algebra.collinear?', 'linear-algebra.orthogonal?', 'linear-algebra.angle'],
  },
  'orthogonal?': {
    category: 'linear-algebra',
    description: 'Checks if two vectors are **orthogonal**.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { orthogonal? } = import("linear-algebra");\northogonal?([1, 0], [0, 1])',
      'let { orthogonal? } = import("linear-algebra");\northogonal?([1, 0, 1], [0, 1, 0])',
      'let { orthogonal? } = import("linear-algebra");\northogonal?([1, 2], [2, -1])',
    ],
    seeAlso: ['linear-algebra.collinear?', 'linear-algebra.parallel?', 'linear-algebra.dot', 'matrix.orthogonal?', 'linear-algebra.angle'],
  },
  'cosine-similarity': {
    category: 'linear-algebra',
    description: 'Calculates the **cosine similarity** between two vectors. The result is a value between -1 and 1.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { cosine-similarity } = import("linear-algebra");\ncosine-similarity([1, 2], [3, 4])',
      'let { cosine-similarity } = import("linear-algebra");\ncosine-similarity([1, 2, 3], [4, 5, 6])',
      'let { cosine-similarity } = import("linear-algebra");\ncosine-similarity([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.dot', 'linear-algebra.angle', 'linear-algebra.euclidean-distance'],
  },
  'euclidean-distance': {
    category: 'linear-algebra',
    description: 'Calculates the **Euclidean distance** between two vectors. The result is a non-negative number.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { euclidean-distance } = import("linear-algebra");\neuclidean-distance([1, 2], [3, 4])',
      'let { euclidean-distance } = import("linear-algebra");\neuclidean-distance([1, 2, 3], [4, 5, 6])',
      'let { euclidean-distance } = import("linear-algebra");\neuclidean-distance([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.manhattan-distance', 'linear-algebra.chebyshev-distance', 'linear-algebra.minkowski-distance', 'linear-algebra.euclidean-norm', 'linear-algebra.cosine-similarity', 'linear-algebra.hamming-distance'],
  },
  'euclidean-norm': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { euclidean-norm } = import("linear-algebra");\neuclidean-norm([1, 2])',
      'let { euclidean-norm } = import("linear-algebra");\neuclidean-norm([3, 4])',
      'let { euclidean-norm } = import("linear-algebra");\neuclidean-norm([1, 2, 3])',
    ],
    seeAlso: ['linear-algebra.manhattan-norm', 'linear-algebra.chebyshev-norm', 'linear-algebra.minkowski-norm', 'linear-algebra.euclidean-distance', 'linear-algebra.normalize-l2', 'linear-algebra.hamming-norm'],
  },
  'manhattan-distance': {
    category: 'linear-algebra',
    description: 'Calculates the **Manhattan distance** between two vectors. The result is a non-negative number.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { manhattan-distance } = import("linear-algebra");\nmanhattan-distance([1, 2], [3, 4])',
      'let { manhattan-distance } = import("linear-algebra");\nmanhattan-distance([1, 2, 3], [4, 5, 6])',
      'let { manhattan-distance } = import("linear-algebra");\nmanhattan-distance([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.euclidean-distance', 'linear-algebra.chebyshev-distance', 'linear-algebra.minkowski-distance', 'linear-algebra.manhattan-norm', 'linear-algebra.hamming-distance'],
  },
  'manhattan-norm': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { manhattan-norm } = import("linear-algebra");\nmanhattan-norm([1, 2])',
      'let { manhattan-norm } = import("linear-algebra");\nmanhattan-norm([3, 4])',
      'let { manhattan-norm } = import("linear-algebra");\nmanhattan-norm([1, 2, 3])',
    ],
    seeAlso: ['linear-algebra.euclidean-norm', 'linear-algebra.chebyshev-norm', 'linear-algebra.minkowski-norm', 'linear-algebra.manhattan-distance', 'linear-algebra.normalize-l1', 'linear-algebra.hamming-norm'],
  },
  'hamming-distance': {
    category: 'linear-algebra',
    description: 'Calculates the **Hamming distance** between two vectors. The result is a non-negative integer.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { hamming-distance } = import("linear-algebra");\nhamming-distance([1, 2], [3, 4])',
      'let { hamming-distance } = import("linear-algebra");\nhamming-distance([1, 2, 3], [4, 5, 6])',
      'let { hamming-distance } = import("linear-algebra");\nhamming-distance([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.euclidean-distance', 'linear-algebra.manhattan-distance', 'linear-algebra.hamming-norm'],
  },
  'hamming-norm': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { hamming-norm } = import("linear-algebra");\nhamming-norm([1, 2])',
      'let { hamming-norm } = import("linear-algebra");\nhamming-norm([3, 4])',
      'let { hamming-norm } = import("linear-algebra");\nhamming-norm([1, 2, 3])',
    ],
    seeAlso: ['linear-algebra.euclidean-norm', 'linear-algebra.manhattan-norm', 'linear-algebra.hamming-distance'],
  },
  'chebyshev-distance': {
    category: 'linear-algebra',
    description: 'Calculates the **Chebyshev distance** between two vectors. The result is a non-negative number.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { chebyshev-distance } = import("linear-algebra");\nchebyshev-distance([1, 2], [3, 4])',
      'let { chebyshev-distance } = import("linear-algebra");\nchebyshev-distance([1, 2, 3], [4, 5, 6])',
      'let { chebyshev-distance } = import("linear-algebra");\nchebyshev-distance([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.euclidean-distance', 'linear-algebra.manhattan-distance', 'linear-algebra.minkowski-distance', 'linear-algebra.chebyshev-norm'],
  },
  'chebyshev-norm': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'v',
        ],
      },
    ],
    examples: [
      'let { chebyshev-norm } = import("linear-algebra");\nchebyshev-norm([1, 2])',
      'let { chebyshev-norm } = import("linear-algebra");\nchebyshev-norm([3, 4])',
      'let { chebyshev-norm } = import("linear-algebra");\nchebyshev-norm([1, 2, 3])',
    ],
    seeAlso: ['linear-algebra.euclidean-norm', 'linear-algebra.manhattan-norm', 'linear-algebra.minkowski-norm', 'linear-algebra.chebyshev-distance'],
  },
  'minkowski-distance': {
    category: 'linear-algebra',
    description: 'Calculates the **Minkowski distance** between two vectors. The result is a non-negative number.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
      p: {
        type: 'number',
        description: 'Order of the norm (p).',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
          'p',
        ],
      },
    ],
    examples: [
      'let { minkowski-distance } = import("linear-algebra");\nminkowski-distance([1, 2], [3, 4], 2)',
      'let { minkowski-distance } = import("linear-algebra");\nminkowski-distance([1, 2, 3], [4, 5, 6], 3)',
      'let { minkowski-distance } = import("linear-algebra");\nminkowski-distance([1, 0], [0, 1], 1)',
    ],
    seeAlso: ['linear-algebra.euclidean-distance', 'linear-algebra.manhattan-distance', 'linear-algebra.chebyshev-distance', 'linear-algebra.minkowski-norm'],
  },
  'minkowski-norm': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { minkowski-norm } = import("linear-algebra");\nminkowski-norm([1, 2], 2)',
      'let { minkowski-norm } = import("linear-algebra");\nminkowski-norm([3, 4], 3)',
      'let { minkowski-norm } = import("linear-algebra");\nminkowski-norm([1, 2, 3], 4)',
    ],
    seeAlso: ['linear-algebra.euclidean-norm', 'linear-algebra.manhattan-norm', 'linear-algebra.chebyshev-norm', 'linear-algebra.minkowski-distance'],
  },
  'cov': {
    category: 'linear-algebra',
    description: 'Calculates the **covariance** between two vectors. The result is a number.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { cov } = import("linear-algebra");\ncov([1, 2], [3, 4])',
      'let { cov } = import("linear-algebra");\ncov([1, 2, 3], [4, 5, 6])',
      'let { cov } = import("linear-algebra");\ncov([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.corr', 'linear-algebra.pearson-corr', 'vector.variance'],
  },
  'corr': {
    category: 'linear-algebra',
    description: 'Calculates the **correlation** between two vectors. The result is a number between -1 and 1.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { corr } = import("linear-algebra");\ncorr([1, 2], [3, 4])',
      'let { corr } = import("linear-algebra");\ncorr([1, 2, 3], [4, 5, 6])',
      'let { corr } = import("linear-algebra");\ncorr([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.cov', 'linear-algebra.pearson-corr', 'linear-algebra.spearman-corr', 'linear-algebra.kendall-tau', 'linear-algebra.autocorrelation', 'linear-algebra.cross-correlation'],
  },
  'spearman-corr': {
    category: 'linear-algebra',
    description: 'Calculates the **Spearman rank correlation** between two vectors. The result is a number between -1 and 1.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { spearman-corr } = import("linear-algebra");\nspearman-corr([1, 2], [3, 4])',
      'let { spearman-corr } = import("linear-algebra");\nspearman-corr([1, 2, 3], [4, 5, 6])',
      'let { spearman-corr } = import("linear-algebra");\nspearman-corr([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.pearson-corr', 'linear-algebra.kendall-tau', 'linear-algebra.corr'],
  },
  'pearson-corr': {
    category: 'linear-algebra',
    description: 'Calculates the **Pearson correlation** between two vectors. The result is a number between -1 and 1.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { pearson-corr } = import("linear-algebra");\npearson-corr([1, 2], [3, 4])',
      'let { pearson-corr } = import("linear-algebra");\npearson-corr([1, 2, 3], [4, 5, 6])',
      'let { pearson-corr } = import("linear-algebra");\npearson-corr([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.spearman-corr', 'linear-algebra.kendall-tau', 'linear-algebra.corr', 'linear-algebra.cov'],
  },
  'kendall-tau': {
    category: 'linear-algebra',
    description: 'Calculates the **Kendall Tau** rank correlation coefficient between two vectors. The result is a number between -1 and 1.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { kendall-tau } = import("linear-algebra");\nkendall-tau([1, 2], [3, 4])',
      'let { kendall-tau } = import("linear-algebra");\nkendall-tau([1, 2, 3], [4, 5, 6])',
      'let { kendall-tau } = import("linear-algebra");\nkendall-tau([1, 0], [0, 1])',
    ],
    seeAlso: ['linear-algebra.spearman-corr', 'linear-algebra.pearson-corr', 'linear-algebra.corr'],
  },
  'autocorrelation': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { autocorrelation } = import("linear-algebra");\nautocorrelation([1, 2, 3], -2)',
      'let { autocorrelation } = import("linear-algebra");\nautocorrelation([1, 2, 3], -1)',
      'let { autocorrelation } = import("linear-algebra");\nautocorrelation([1, 2, 3], 0)',
      'let { autocorrelation } = import("linear-algebra");\nautocorrelation([1, 2, 3], 1)',
      'let { autocorrelation } = import("linear-algebra");\nautocorrelation([1, 2, 3], 2)',
    ],
    seeAlso: ['linear-algebra.cross-correlation', 'linear-algebra.corr'],
  },
  'cross-correlation': {
    category: 'linear-algebra',
    description: 'Calculates the **cross-correlation** between two vectors. The result is a vector of cross-correlation coefficients.',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'vector',
      },
      b: {
        type: 'vector',
      },
      lag: {
        type: 'integer',
        description: 'Lag value for the cross-correlation.',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
          'lag',
        ],
      },
    ],
    examples: [
      'let { cross-correlation } = import("linear-algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], -2)',
      'let { cross-correlation } = import("linear-algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], -1)',
      'let { cross-correlation } = import("linear-algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], 0)',
      'let { cross-correlation } = import("linear-algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], 1)',
      'let { cross-correlation } = import("linear-algebra");\ncross-correlation([1, 2, 3], [4, 5, 6], 2)',
    ],
    seeAlso: ['linear-algebra.autocorrelation', 'linear-algebra.corr'],
  },
  'rref': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'm',
        ],
      },
    ],
    examples: [
      'let { rref } = import("linear-algebra");\nrref([[1, 2], [3, 4]])',
      'let { rref } = import("linear-algebra");\nrref([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { rref } = import("linear-algebra");\nrref([[1, 2, 3], [7, 8, 9], [4, 5, 7]])',
    ],
    seeAlso: ['linear-algebra.solve', 'matrix.rank'],
  },
  'solve': {
    category: 'linear-algebra',
    description: 'Solves a system of linear equations represented by a matrix and a vector.',
    returns: {
      type: 'vector',
    },
    args: {
      a: {
        type: 'matrix',
      },
      b: {
        type: 'vector',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { solve } = import("linear-algebra");\nsolve([\n  [2, 1, -1, 1], \n  [4, 5, -3, 2], \n  [6, -2, 5, -3], \n  [8, 3, 2, 4]\n], [5, 10, 2, 17])',
      'let { solve } = import("linear-algebra"); solve([[2, 0, 0], [3, 1, 0], [4, 5, 6]], [4, 5, 38])',
      'let { solve } = import("linear-algebra"); solve([[2, 3], [1, -1]], [8, 2])',
    ],
    seeAlso: ['linear-algebra.rref', 'matrix.inv'],
  },
  'to-polar': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { to-polar } = import("linear-algebra");\nto-polar([1, 2])',
      'let { to-polar } = import("linear-algebra");\nto-polar([3, 4])',
    ],
    seeAlso: ['linear-algebra.from-polar', 'linear-algebra.angle'],
  },
  'from-polar': {
    category: 'linear-algebra',
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
      {
        argumentNames: [
          'polar',
        ],
      },
    ],
    examples: [
      'let { from-polar } = import("linear-algebra");\nfrom-polar([1, PI / 4])',
      'let { from-polar } = import("linear-algebra");\nfrom-polar([1, 0])',
      'let { from-polar } = import("linear-algebra");\nfrom-polar([1, -PI / 2])',
    ],
    seeAlso: ['linear-algebra.to-polar'],
  },
}
