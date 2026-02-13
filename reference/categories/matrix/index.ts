import type { FunctionReference } from '../..'
import { type MatrixApiName, getOperatorArgs } from '../../api'

export const matrixReference: Record<MatrixApiName, FunctionReference<'Matrix'>> = {
  'Matrix.mul': {
    title: 'Matrix.mul',
    category: 'Matrix',
    returns: {
      type: 'matrix',
    },
    args: {
      ...getOperatorArgs('matrix', 'matrix'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Multiplies two `matrices` using standard `matrix` multiplication based on **dot products** of rows and columns.',
    examples: [
      'let { mul } = import("Matrix");\nmul([[1, 2], [3, 4]], [[5, 6], [7, 8]])',
      'let { mul } = import("Matrix");\nmul([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]])',
    ],
  },
  'Matrix.det': {
    title: 'Matrix.det',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **determinant** of a square matrix.',
    examples: [
      'let { det } = import("Matrix");\ndet([[1, 2], [3, 4]])',
      'let { det } = import("Matrix");\ndet([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'Matrix.inv': {
    title: 'Matrix.inv',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **inverse** of a square matrix.',
    examples: [
      'let { inv } = import("Matrix");\ninv([[1, 2], [3, 4]])',
      'let { inv } = import("Matrix");\ninv([[1, 2, 3], [4, 5, 7], [7, 8, 10]])',
    ],
  },
  'Matrix.adj': {
    title: 'Matrix.adj',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **adjugate** of a square matrix.',
    examples: [
      'let { adj } = import("Matrix");\nadj([[1, 2], [3, 4]])',
      'let { adj } = import("Matrix");\nadj([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { adj } = import("Matrix");\nadj([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',

    ],
  },
  'Matrix.cofactor': {
    title: 'Matrix.cofactor',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **cofactor** of a square matrix.',
    examples: [
      'let { cofactor } = import("Matrix");\ncofactor([[1, 2], [3, 4]])',
      'let { cofactor } = import("Matrix");\ncofactor([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { cofactor } = import("Matrix");\ncofactor([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',

    ],
  },
  'Matrix.minor': {
    title: 'Matrix.minor',
    category: 'Matrix',
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
      { argumentNames: ['m', 'row', 'col'] },
    ],
    description: 'Calculates the **minor** of a square matrix.',
    examples: [
      'let { minor } = import("Matrix");\nminor([[1, 2], [3, 4]], 0, 1)',
      'let { minor } = import("Matrix");\nminor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1)',
    ],
  },
  'Matrix.trace': {
    title: 'Matrix.trace',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **trace** of a square matrix.',
    examples: [
      'let { trace } = import("Matrix");\ntrace([[1, 2], [3, 4]])',
      'let { trace } = import("Matrix");\ntrace([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'Matrix.symmetric?': {
    title: 'Matrix.symmetric?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **symmetric**.',
    examples: [
      'let { symmetric? } = import("Matrix");\nsymmetric?([[1, 2], [2, 1]])',
      'let { symmetric? } = import("Matrix");\nsymmetric?([[1, 2, 3], [2, 1, 4], [3, 4, 1]])',
    ],
  },
  'Matrix.triangular?': {
    title: 'Matrix.triangular?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **triangular**.',
    examples: [
      'let { triangular? } = import("Matrix");\ntriangular?([[2, 0], [0, 1]])',
      'let { triangular? } = import("Matrix");\ntriangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'Matrix.upper-triangular?': {
    title: 'Matrix.upper-triangular?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **upper triangular**.',
    examples: [
      'let { upper-triangular? } = import("Matrix");\nupper-triangular?([[1, 2], [0, 3]])',
      'let { upper-triangular? } = import("Matrix");\nupper-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'Matrix.lower-triangular?': {
    title: 'Matrix.lower-triangular?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **lower triangular**.',
    examples: [
      'let { lower-triangular? } = import("Matrix");\nlower-triangular?([[1, 0], [2, 3]])',
      'let { lower-triangular? } = import("Matrix");\nlower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])',
    ],
  },
  'Matrix.diagonal?': {
    title: 'Matrix.diagonal?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **diagonal**.',
    examples: [
      'let { diagonal? } = import("Matrix");\ndiagonal?([[1, 0], [0, 2]])',
      'let { diagonal? } = import("Matrix");\ndiagonal?([[1, 0, 0], [0, 2, 0], [0, 0, 3]])',
      'let { diagonal? } = import("Matrix");\ndiagonal?([[1, 0, 0], [2, 2, 2], [0, 0, 3]])',
    ],
  },
  'Matrix.square?': {
    title: 'Matrix.square?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **square**.',
    examples: [
      'let { square? } = import("Matrix");\nsquare?([[1, 2], [3, 4]])',
      'let { square? } = import("Matrix");\nsquare?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { square? } = import("Matrix");\nsquare?([[1, 2, 3], [4, 5, 6]])',
    ],
  },
  'Matrix.orthogonal?': {
    title: 'Matrix.orthogonal?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **orthogonal**.',
    examples: [
      'let { orthogonal? } = import("Matrix");\northogonal?([[1, 0], [0, 1]])',
      'let { orthogonal? } = import("Matrix");\northogonal?([[1, 0], [0, -1]])',
      'let { orthogonal? } = import("Matrix");\northogonal?([[1, 2], [3, 4]])',
    ],
  },
  'Matrix.identity?': {
    title: 'Matrix.identity?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is an **identity matrix**.',
    examples: [
      'let { identity? } = import("Matrix");\nidentity?([[1, 0], [0, 1]])',
      'let { identity? } = import("Matrix");\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { identity? } = import("Matrix");\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 0]])',
    ],
  },
  'Matrix.invertible?': {
    title: 'Matrix.invertible?',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a `matrix` is **invertible**.',
    examples: [
      'let { invertible? } = import("Matrix");\ninvertible?([[1, 2], [3, 4]])',
      'let { invertible? } = import("Matrix");\ninvertible?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { invertible? } = import("Matrix");\ninvertible?([[1, 2], [2, 4]])',
    ],
  },
  'Matrix.hilbert': {
    title: 'Matrix.hilbert',
    category: 'Matrix',
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
      { argumentNames: ['n'] },
    ],
    description: 'Generates a **Hilbert matrix** of size `n`.',
    examples: [
      'let { hilbert } = import("Matrix");\nhilbert(3)',
      'let { hilbert } = import("Matrix");\nhilbert(4)',
    ],
  },
  'Matrix.vandermonde': {
    title: 'Matrix.vandermonde',
    category: 'Matrix',
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
      { argumentNames: ['v'] },
    ],
    description: 'Generates a **Vandermonde matrix** from a vector.',
    examples: [
      'let { vandermonde } = import("Matrix");\nvandermonde([1, 2, 3])',
      'let { vandermonde } = import("Matrix");\nvandermonde([1, 0, 1])',
    ],
  },
  'Matrix.band': {
    title: 'Matrix.band',
    category: 'Matrix',
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
      { argumentNames: ['n', 'lband', 'uband'] },
    ],
    description: 'Generates a **banded matrix** of size `n` with lower band index `lband` and upper band index `uband`.',
    examples: [
      'let { band } = import("Matrix");\nband(3, 1, 1)',
      'let { band } = import("Matrix");\nband(4, 1, 2)',
    ],
  },
  'Matrix.banded?': {
    title: 'Matrix.banded?',
    category: 'Matrix',
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
      { argumentNames: ['m', 'lband', 'uband'] },
    ],
    description: 'Checks if a `matrix` is **banded** with lower band index `lband` and upper band index `uband`.',
    examples: [
      `let { banded? } = import("Matrix");
banded?([
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 1, 1, 1],
  [0, 1, 1, 1],
], 2, 2)`,
      `let { banded? } = import("Matrix");
banded?([
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 1, 1, 1],
  [0, 1, 1, 1],
], 1, 1)`,
    ],
  },
  'Matrix.rank': {
    title: 'Matrix.rank',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **rank** of a matrix using **Gaussian elimination**.',
    examples: [
      'let { rank } = import("Matrix");\nrank([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { rank } = import("Matrix");\nrank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { rank } = import("Matrix");\nrank([[2, 4, 6], [3, 6, 9], [4, 8, 12]])',
    ],
  },
  'Matrix.frobenius-norm': {
    title: 'Matrix.frobenius-norm',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **Frobenius norm** of a matrix.',
    examples: [
      'let { frobenius-norm } = import("Matrix");\nfrobenius-norm([[1, 2], [3, 4]])',
      'let { frobenius-norm } = import("Matrix");\nfrobenius-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'Matrix.one-norm': {
    title: 'Matrix.one-norm',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **one-norm** (column norm) of a matrix.',
    examples: [
      'let { one-norm } = import("Matrix");\none-norm([[1, 2], [3, 4]])',
      'let { one-norm } = import("Matrix");\none-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'Matrix.inf-norm': {
    title: 'Matrix.inf-norm',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **infinity norm** of a matrix.',
    examples: [
      'let { inf-norm } = import("Matrix");\ninf-norm([[1, 2], [3, 4]])',
      'let { inf-norm } = import("Matrix");\ninf-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    aliases: ['Matrix.row-norm'],
  },
  'Matrix.max-norm': {
    title: 'Matrix.max-norm',
    category: 'Matrix',
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
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **max norm** of a matrix.',
    examples: [
      'let { max-norm } = import("Matrix");\nmax-norm([[1, 2], [3, 4]])',
      'let { max-norm } = import("Matrix");\nmax-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
}
