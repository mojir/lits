import type { FunctionDocs } from '../../interface'

export const namespaceDocs: Record<string, FunctionDocs> = {
  'mul': {
    category: 'Matrix',
    description: 'Multiplies two `matrices` using standard `matrix` multiplication based on **dot products** of rows and columns.',
    returns: {
      type: 'matrix',
    },
    args: {
      a: {
        type: 'matrix',
      },
      b: {
        type: 'matrix',
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
      'let { mul } = import("Matrix");\nmul([[1, 2], [3, 4]], [[5, 6], [7, 8]])',
      'let { mul } = import("Matrix");\nmul([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]])',
    ],
  },
  'det': {
    category: 'Matrix',
    description: 'Calculates the **determinant** of a square matrix.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the determinant of.',
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
      'let { det } = import("Matrix");\ndet([[1, 2], [3, 4]])',
      'let { det } = import("Matrix");\ndet([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'inv': {
    category: 'Matrix',
    description: 'Calculates the **inverse** of a square matrix.',
    returns: {
      type: 'matrix',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the inverse of.',
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
      'let { inv } = import("Matrix");\ninv([[1, 2], [3, 4]])',
      'let { inv } = import("Matrix");\ninv([[1, 2, 3], [4, 5, 7], [7, 8, 10]])',
    ],
  },
  'adj': {
    category: 'Matrix',
    description: 'Calculates the **adjugate** of a square matrix.',
    returns: {
      type: 'matrix',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the adjugate of.',
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
      'let { adj } = import("Matrix");\nadj([[1, 2], [3, 4]])',
      'let { adj } = import("Matrix");\nadj([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { adj } = import("Matrix");\nadj([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',
    ],
  },
  'cofactor': {
    category: 'Matrix',
    description: 'Calculates the **cofactor** of a square matrix.',
    returns: {
      type: 'matrix',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the cofactor of.',
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
      'let { cofactor } = import("Matrix");\ncofactor([[1, 2], [3, 4]])',
      'let { cofactor } = import("Matrix");\ncofactor([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { cofactor } = import("Matrix");\ncofactor([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',
    ],
  },
  'minor': {
    category: 'Matrix',
    description: 'Calculates the **minor** of a square matrix.',
    returns: {
      type: 'matrix',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the minor of.',
      },
      row: {
        type: 'integer',
        description: 'The row index of the element to calculate the minor for.',
      },
      col: {
        type: 'integer',
        description: 'The column index of the element to calculate the minor for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'm',
          'row',
          'col',
        ],
      },
    ],
    examples: [
      'let { minor } = import("Matrix");\nminor([[1, 2], [3, 4]], 0, 1)',
      'let { minor } = import("Matrix");\nminor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1)',
    ],
  },
  'trace': {
    category: 'Matrix',
    description: 'Calculates the **trace** of a square matrix.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the trace of.',
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
      'let { trace } = import("Matrix");\ntrace([[1, 2], [3, 4]])',
      'let { trace } = import("Matrix");\ntrace([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'symmetric?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **symmetric**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for symmetry.',
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
      'let { symmetric? } = import("Matrix");\nsymmetric?([[1, 2], [2, 1]])',
      'let { symmetric? } = import("Matrix");\nsymmetric?([[1, 2, 3], [2, 1, 4], [3, 4, 1]])',
    ],
  },
  'triangular?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **triangular**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for triangularity.',
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
      'let { triangular? } = import("Matrix");\ntriangular?([[2, 0], [0, 1]])',
      'let { triangular? } = import("Matrix");\ntriangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'upper-triangular?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **upper triangular**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for upper triangularity.',
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
      'let { upper-triangular? } = import("Matrix");\nupper-triangular?([[1, 2], [0, 3]])',
      'let { upper-triangular? } = import("Matrix");\nupper-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'lower-triangular?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **lower triangular**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for lower triangularity.',
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
      'let { lower-triangular? } = import("Matrix");\nlower-triangular?([[1, 0], [2, 3]])',
      'let { lower-triangular? } = import("Matrix");\nlower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])',
    ],
  },
  'diagonal?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **diagonal**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for diagonal property.',
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
      'let { diagonal? } = import("Matrix");\ndiagonal?([[1, 0], [0, 2]])',
      'let { diagonal? } = import("Matrix");\ndiagonal?([[1, 0, 0], [0, 2, 0], [0, 0, 3]])',
      'let { diagonal? } = import("Matrix");\ndiagonal?([[1, 0, 0], [2, 2, 2], [0, 0, 3]])',
    ],
  },
  'square?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **square**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for square property.',
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
      'let { square? } = import("Matrix");\nsquare?([[1, 2], [3, 4]])',
      'let { square? } = import("Matrix");\nsquare?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { square? } = import("Matrix");\nsquare?([[1, 2, 3], [4, 5, 6]])',
    ],
  },
  'orthogonal?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **orthogonal**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for **orthogonality**.',
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
      'let { orthogonal? } = import("Matrix");\northogonal?([[1, 0], [0, 1]])',
      'let { orthogonal? } = import("Matrix");\northogonal?([[1, 0], [0, -1]])',
      'let { orthogonal? } = import("Matrix");\northogonal?([[1, 2], [3, 4]])',
    ],
  },
  'identity?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is an **identity matrix**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for identity property.',
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
      'let { identity? } = import("Matrix");\nidentity?([[1, 0], [0, 1]])',
      'let { identity? } = import("Matrix");\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { identity? } = import("Matrix");\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 0]])',
    ],
  },
  'invertible?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **invertible**.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for invertibility.',
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
      'let { invertible? } = import("Matrix");\ninvertible?([[1, 2], [3, 4]])',
      'let { invertible? } = import("Matrix");\ninvertible?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { invertible? } = import("Matrix");\ninvertible?([[1, 2], [2, 4]])',
    ],
  },
  'hilbert': {
    category: 'Matrix',
    description: 'Generates a **Hilbert matrix** of size `n`.',
    returns: {
      type: 'matrix',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The size of the Hilbert matrix.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { hilbert } = import("Matrix");\nhilbert(3)',
      'let { hilbert } = import("Matrix");\nhilbert(4)',
    ],
  },
  'vandermonde': {
    category: 'Matrix',
    description: 'Generates a **Vandermonde matrix** from a vector.',
    returns: {
      type: 'matrix',
    },
    args: {
      v: {
        type: 'vector',
        description: 'The vector to generate the Vandermonde matrix from.',
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
      'let { vandermonde } = import("Matrix");\nvandermonde([1, 2, 3])',
      'let { vandermonde } = import("Matrix");\nvandermonde([1, 0, 1])',
    ],
  },
  'band': {
    category: 'Matrix',
    description: 'Generates a **banded matrix** of size `n` with lower band index `lband` and upper band index `uband`.',
    returns: {
      type: 'matrix',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The size of the banded matrix.',
      },
      lband: {
        type: 'integer',
        description: 'The lower band index.',
      },
      uband: {
        type: 'integer',
        description: 'The upper band index.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
          'lband',
          'uband',
        ],
      },
    ],
    examples: [
      'let { band } = import("Matrix");\nband(3, 1, 1)',
      'let { band } = import("Matrix");\nband(4, 1, 2)',
    ],
  },
  'banded?': {
    category: 'Matrix',
    description: 'Checks if a `matrix` is **banded** with lower band index `lband` and upper band index `uband`.',
    returns: {
      type: 'boolean',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to check for **banded** property.',
      },
      lband: {
        type: 'integer',
        description: 'The lower band index.',
      },
      uband: {
        type: 'integer',
        description: 'The upper band index.',
      },
    },
    variants: [
      {
        argumentNames: [
          'm',
          'lband',
          'uband',
        ],
      },
    ],
    examples: [
      'let { banded? } = import("Matrix");\nbanded?([\n  [1, 1, 1, 0],\n  [1, 1, 1, 1],\n  [1, 1, 1, 1],\n  [0, 1, 1, 1],\n], 2, 2)',
      'let { banded? } = import("Matrix");\nbanded?([\n  [1, 1, 1, 0],\n  [1, 1, 1, 1],\n  [1, 1, 1, 1],\n  [0, 1, 1, 1],\n], 1, 1)',
    ],
  },
  'rank': {
    category: 'Matrix',
    description: 'Calculates the **rank** of a matrix using **Gaussian elimination**.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the rank of.',
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
      'let { rank } = import("Matrix");\nrank([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { rank } = import("Matrix");\nrank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { rank } = import("Matrix");\nrank([[2, 4, 6], [3, 6, 9], [4, 8, 12]])',
    ],
  },
  'frobenius-norm': {
    category: 'Matrix',
    description: 'Calculates the **Frobenius norm** of a matrix.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the Frobenius norm of.',
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
      'let { frobenius-norm } = import("Matrix");\nfrobenius-norm([[1, 2], [3, 4]])',
      'let { frobenius-norm } = import("Matrix");\nfrobenius-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'one-norm': {
    category: 'Matrix',
    description: 'Calculates the **one-norm** (column norm) of a matrix.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the one-norm of.',
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
      'let { one-norm } = import("Matrix");\none-norm([[1, 2], [3, 4]])',
      'let { one-norm } = import("Matrix");\none-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'inf-norm': {
    category: 'Matrix',
    description: 'Calculates the **infinity norm** of a matrix.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the infinity norm of.',
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
      'let { inf-norm } = import("Matrix");\ninf-norm([[1, 2], [3, 4]])',
      'let { inf-norm } = import("Matrix");\ninf-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'max-norm': {
    category: 'Matrix',
    description: 'Calculates the **max norm** of a matrix.',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the max norm of.',
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
      'let { max-norm } = import("Matrix");\nmax-norm([[1, 2], [3, 4]])',
      'let { max-norm } = import("Matrix");\nmax-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
}
