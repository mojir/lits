import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'mul': {
    category: 'matrix',
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
      'let { mul } = import(matrix);\nmul([[1, 2], [3, 4]], [[5, 6], [7, 8]])',
      'let { mul } = import(matrix);\nmul([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]])',
    ],
    seeAlso: ['matrix.det', 'matrix.inv'],
  },
  'det': {
    category: 'matrix',
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
      'let { det } = import(matrix);\ndet([[1, 2], [3, 4]])',
      'let { det } = import(matrix);\ndet([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    seeAlso: ['matrix.inv', 'matrix.cofactor', 'matrix.adj', 'matrix.trace', 'matrix.rank', 'matrix.invertible?', 'matrix.mul', 'matrix.minor'],
  },
  'inv': {
    category: 'matrix',
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
      'let { inv } = import(matrix);\ninv([[1, 2], [3, 4]])',
      'let { inv } = import(matrix);\ninv([[1, 2, 3], [4, 5, 7], [7, 8, 10]])',
    ],
    seeAlso: ['matrix.det', 'matrix.adj', 'matrix.invertible?', 'linear-algebra.solve', 'matrix.mul', 'matrix.orthogonal?'],
  },
  'adj': {
    category: 'matrix',
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
      'let { adj } = import(matrix);\nadj([[1, 2], [3, 4]])',
      'let { adj } = import(matrix);\nadj([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { adj } = import(matrix);\nadj([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',
    ],
    seeAlso: ['matrix.cofactor', 'matrix.det', 'matrix.inv'],
  },
  'cofactor': {
    category: 'matrix',
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
      'let { cofactor } = import(matrix);\ncofactor([[1, 2], [3, 4]])',
      'let { cofactor } = import(matrix);\ncofactor([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { cofactor } = import(matrix);\ncofactor([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',
    ],
    seeAlso: ['matrix.adj', 'matrix.minor', 'matrix.det'],
  },
  'minor': {
    category: 'matrix',
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
      'let { minor } = import(matrix);\nminor([[1, 2], [3, 4]], 0, 1)',
      'let { minor } = import(matrix);\nminor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1)',
    ],
    seeAlso: ['matrix.cofactor', 'matrix.det'],
  },
  'trace': {
    category: 'matrix',
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
      'let { trace } = import(matrix);\ntrace([[1, 2], [3, 4]])',
      'let { trace } = import(matrix);\ntrace([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    seeAlso: ['matrix.det', 'matrix.diagonal?'],
  },
  'symmetric?': {
    category: 'matrix',
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
      'let { symmetric? } = import(matrix);\nsymmetric?([[1, 2], [2, 1]])',
      'let { symmetric? } = import(matrix);\nsymmetric?([[1, 2, 3], [2, 1, 4], [3, 4, 1]])',
    ],
    seeAlso: ['matrix.orthogonal?', 'matrix.diagonal?', 'matrix.square?', 'matrix.hilbert'],
  },
  'triangular?': {
    category: 'matrix',
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
      'let { triangular? } = import(matrix);\ntriangular?([[2, 0], [0, 1]])',
      'let { triangular? } = import(matrix);\ntriangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
    seeAlso: ['matrix.upper-triangular?', 'matrix.lower-triangular?', 'matrix.diagonal?', 'matrix.banded?'],
  },
  'upper-triangular?': {
    category: 'matrix',
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
      'let { upper-triangular? } = import(matrix);\nupper-triangular?([[1, 2], [0, 3]])',
      'let { upper-triangular? } = import(matrix);\nupper-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
    seeAlso: ['matrix.lower-triangular?', 'matrix.triangular?', 'matrix.diagonal?'],
  },
  'lower-triangular?': {
    category: 'matrix',
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
      'let { lower-triangular? } = import(matrix);\nlower-triangular?([[1, 0], [2, 3]])',
      'let { lower-triangular? } = import(matrix);\nlower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])',
    ],
    seeAlso: ['matrix.upper-triangular?', 'matrix.triangular?', 'matrix.diagonal?'],
  },
  'diagonal?': {
    category: 'matrix',
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
      'let { diagonal? } = import(matrix);\ndiagonal?([[1, 0], [0, 2]])',
      'let { diagonal? } = import(matrix);\ndiagonal?([[1, 0, 0], [0, 2, 0], [0, 0, 3]])',
      'let { diagonal? } = import(matrix);\ndiagonal?([[1, 0, 0], [2, 2, 2], [0, 0, 3]])',
    ],
    seeAlso: ['matrix.identity?', 'matrix.symmetric?', 'matrix.triangular?', 'matrix.trace', 'matrix.upper-triangular?', 'matrix.lower-triangular?', 'matrix.band', 'matrix.banded?'],
  },
  'square?': {
    category: 'matrix',
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
      'let { square? } = import(matrix);\nsquare?([[1, 2], [3, 4]])',
      'let { square? } = import(matrix);\nsquare?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { square? } = import(matrix);\nsquare?([[1, 2, 3], [4, 5, 6]])',
    ],
    seeAlso: ['matrix.symmetric?', 'matrix.identity?', 'matrix.invertible?'],
  },
  'orthogonal?': {
    category: 'matrix',
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
      'let { orthogonal? } = import(matrix);\northogonal?([[1, 0], [0, 1]])',
      'let { orthogonal? } = import(matrix);\northogonal?([[1, 0], [0, -1]])',
      'let { orthogonal? } = import(matrix);\northogonal?([[1, 2], [3, 4]])',
    ],
    seeAlso: ['matrix.symmetric?', 'matrix.inv', 'matrix.identity?', 'linear-algebra.orthogonal?'],
  },
  'identity?': {
    category: 'matrix',
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
      'let { identity? } = import(matrix);\nidentity?([[1, 0], [0, 1]])',
      'let { identity? } = import(matrix);\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { identity? } = import(matrix);\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 0]])',
    ],
    seeAlso: ['matrix.diagonal?', 'matrix.square?', 'matrix.orthogonal?'],
  },
  'invertible?': {
    category: 'matrix',
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
      'let { invertible? } = import(matrix);\ninvertible?([[1, 2], [3, 4]])',
      'let { invertible? } = import(matrix);\ninvertible?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { invertible? } = import(matrix);\ninvertible?([[1, 2], [2, 4]])',
    ],
    seeAlso: ['matrix.det', 'matrix.inv', 'matrix.rank', 'matrix.square?'],
  },
  'hilbert': {
    category: 'matrix',
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
      'let { hilbert } = import(matrix);\nhilbert(3)',
      'let { hilbert } = import(matrix);\nhilbert(4)',
    ],
    seeAlso: ['matrix.vandermonde', 'matrix.symmetric?'],
  },
  'vandermonde': {
    category: 'matrix',
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
      'let { vandermonde } = import(matrix);\nvandermonde([1, 2, 3])',
      'let { vandermonde } = import(matrix);\nvandermonde([1, 0, 1])',
    ],
    seeAlso: ['matrix.hilbert', 'matrix.band'],
  },
  'band': {
    category: 'matrix',
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
      'let { band } = import(matrix);\nband(3, 1, 1)',
      'let { band } = import(matrix);\nband(4, 1, 2)',
    ],
    seeAlso: ['matrix.banded?', 'matrix.diagonal?', 'matrix.vandermonde'],
  },
  'banded?': {
    category: 'matrix',
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
      'let { banded? } = import(matrix);\nbanded?([\n  [1, 1, 1, 0],\n  [1, 1, 1, 1],\n  [1, 1, 1, 1],\n  [0, 1, 1, 1],\n], 2, 2)',
      'let { banded? } = import(matrix);\nbanded?([\n  [1, 1, 1, 0],\n  [1, 1, 1, 1],\n  [1, 1, 1, 1],\n  [0, 1, 1, 1],\n], 1, 1)',
    ],
    seeAlso: ['matrix.band', 'matrix.triangular?', 'matrix.diagonal?'],
  },
  'rank': {
    category: 'matrix',
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
      'let { rank } = import(matrix);\nrank([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { rank } = import(matrix);\nrank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { rank } = import(matrix);\nrank([[2, 4, 6], [3, 6, 9], [4, 8, 12]])',
    ],
    seeAlso: ['matrix.det', 'matrix.invertible?', 'linear-algebra.rref'],
  },
  'frobenius-norm': {
    category: 'matrix',
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
      'let { frobenius-norm } = import(matrix);\nfrobenius-norm([[1, 2], [3, 4]])',
      'let { frobenius-norm } = import(matrix);\nfrobenius-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    seeAlso: ['matrix.one-norm', 'matrix.inf-norm', 'matrix.max-norm'],
  },
  'one-norm': {
    category: 'matrix',
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
      'let { one-norm } = import(matrix);\none-norm([[1, 2], [3, 4]])',
      'let { one-norm } = import(matrix);\none-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    seeAlso: ['matrix.frobenius-norm', 'matrix.inf-norm', 'matrix.max-norm'],
  },
  'inf-norm': {
    category: 'matrix',
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
      'let { inf-norm } = import(matrix);\ninf-norm([[1, 2], [3, 4]])',
      'let { inf-norm } = import(matrix);\ninf-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    seeAlso: ['matrix.frobenius-norm', 'matrix.one-norm', 'matrix.max-norm'],
  },
  'max-norm': {
    category: 'matrix',
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
      'let { max-norm } = import(matrix);\nmax-norm([[1, 2], [3, 4]])',
      'let { max-norm } = import(matrix);\nmax-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    seeAlso: ['matrix.frobenius-norm', 'matrix.one-norm', 'matrix.inf-norm'],
  },
}
