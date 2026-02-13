import type { FunctionReference } from '../..'
import { type MatrixApiName, getOperatorArgs } from '../../api'

export const matrixReference: Record<MatrixApiName, FunctionReference<'Matrix'>> = {
  'mat.mul': {
    title: 'mat.mul',
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
      'let { mul } = import("mat");\nmul([[1, 2], [3, 4]], [[5, 6], [7, 8]])',
      'let { mul } = import("mat");\nmul([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]])',
    ],
  },
  'mat.det': {
    title: 'mat.det',
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
      'let { det } = import("mat");\ndet([[1, 2], [3, 4]])',
      'let { det } = import("mat");\ndet([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat.inv': {
    title: 'mat.inv',
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
      'let { inv } = import("mat");\ninv([[1, 2], [3, 4]])',
      'let { inv } = import("mat");\ninv([[1, 2, 3], [4, 5, 7], [7, 8, 10]])',
    ],
  },
  'mat.adj': {
    title: 'mat.adj',
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
      'let { adj } = import("mat");\nadj([[1, 2], [3, 4]])',
      'let { adj } = import("mat");\nadj([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { adj } = import("mat");\nadj([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',

    ],
  },
  'mat.cofactor': {
    title: 'mat.cofactor',
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
      'let { cofactor } = import("mat");\ncofactor([[1, 2], [3, 4]])',
      'let { cofactor } = import("mat");\ncofactor([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { cofactor } = import("mat");\ncofactor([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',

    ],
  },
  'mat.minor': {
    title: 'mat.minor',
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
      'let { minor } = import("mat");\nminor([[1, 2], [3, 4]], 0, 1)',
      'let { minor } = import("mat");\nminor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1)',
    ],
  },
  'mat.trace': {
    title: 'mat.trace',
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
      'let { trace } = import("mat");\ntrace([[1, 2], [3, 4]])',
      'let { trace } = import("mat");\ntrace([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat.symmetric?': {
    title: 'mat.symmetric?',
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
      'let { symmetric? } = import("mat");\nsymmetric?([[1, 2], [2, 1]])',
      'let { symmetric? } = import("mat");\nsymmetric?([[1, 2, 3], [2, 1, 4], [3, 4, 1]])',
    ],
  },
  'mat.triangular?': {
    title: 'mat.triangular?',
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
      'let { triangular? } = import("mat");\ntriangular?([[2, 0], [0, 1]])',
      'let { triangular? } = import("mat");\ntriangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'mat.upper-triangular?': {
    title: 'mat.upper-triangular?',
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
      'let { upper-triangular? } = import("mat");\nupper-triangular?([[1, 2], [0, 3]])',
      'let { upper-triangular? } = import("mat");\nupper-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'mat.lower-triangular?': {
    title: 'mat.lower-triangular?',
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
      'let { lower-triangular? } = import("mat");\nlower-triangular?([[1, 0], [2, 3]])',
      'let { lower-triangular? } = import("mat");\nlower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])',
    ],
  },
  'mat.diagonal?': {
    title: 'mat.diagonal?',
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
      'let { diagonal? } = import("mat");\ndiagonal?([[1, 0], [0, 2]])',
      'let { diagonal? } = import("mat");\ndiagonal?([[1, 0, 0], [0, 2, 0], [0, 0, 3]])',
      'let { diagonal? } = import("mat");\ndiagonal?([[1, 0, 0], [2, 2, 2], [0, 0, 3]])',
    ],
  },
  'mat.square?': {
    title: 'mat.square?',
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
      'let { square? } = import("mat");\nsquare?([[1, 2], [3, 4]])',
      'let { square? } = import("mat");\nsquare?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { square? } = import("mat");\nsquare?([[1, 2, 3], [4, 5, 6]])',
    ],
  },
  'mat.orthogonal?': {
    title: 'mat.orthogonal?',
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
      'let { orthogonal? } = import("mat");\northogonal?([[1, 0], [0, 1]])',
      'let { orthogonal? } = import("mat");\northogonal?([[1, 0], [0, -1]])',
      'let { orthogonal? } = import("mat");\northogonal?([[1, 2], [3, 4]])',
    ],
  },
  'mat.identity?': {
    title: 'mat.identity?',
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
      'let { identity? } = import("mat");\nidentity?([[1, 0], [0, 1]])',
      'let { identity? } = import("mat");\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { identity? } = import("mat");\nidentity?([[1, 0, 0], [0, 1, 0], [0, 0, 0]])',
    ],
  },
  'mat.invertible?': {
    title: 'mat.invertible?',
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
      'let { invertible? } = import("mat");\ninvertible?([[1, 2], [3, 4]])',
      'let { invertible? } = import("mat");\ninvertible?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { invertible? } = import("mat");\ninvertible?([[1, 2], [2, 4]])',
    ],
  },
  'mat.hilbert': {
    title: 'mat.hilbert',
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
      'let { hilbert } = import("mat");\nhilbert(3)',
      'let { hilbert } = import("mat");\nhilbert(4)',
    ],
  },
  'mat.vandermonde': {
    title: 'mat.vandermonde',
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
      'let { vandermonde } = import("mat");\nvandermonde([1, 2, 3])',
      'let { vandermonde } = import("mat");\nvandermonde([1, 0, 1])',
    ],
  },
  'mat.band': {
    title: 'mat.band',
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
      'let { band } = import("mat");\nband(3, 1, 1)',
      'let { band } = import("mat");\nband(4, 1, 2)',
    ],
  },
  'mat.banded?': {
    title: 'mat.banded?',
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
      `mat.banded?([
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 1, 1, 1],
  [0, 1, 1, 1],
], 2, 2)`,
      `mat.banded?([
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 1, 1, 1],
  [0, 1, 1, 1],
], 1, 1)`,
    ],
  },
  'mat.rank': {
    title: 'mat.rank',
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
      'let { rank } = import("mat");\nrank([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'let { rank } = import("mat");\nrank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'let { rank } = import("mat");\nrank([[2, 4, 6], [3, 6, 9], [4, 8, 12]])',
    ],
  },
  'mat.frobenius-norm': {
    title: 'mat.frobenius-norm',
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
      'let { frobenius-norm } = import("mat");\nfrobenius-norm([[1, 2], [3, 4]])',
      'let { frobenius-norm } = import("mat");\nfrobenius-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat.one-norm': {
    title: 'mat.one-norm',
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
      'let { one-norm } = import("mat");\none-norm([[1, 2], [3, 4]])',
      'let { one-norm } = import("mat");\none-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat.inf-norm': {
    title: 'mat.inf-norm',
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
      'let { inf-norm } = import("mat");\ninf-norm([[1, 2], [3, 4]])',
      'let { inf-norm } = import("mat");\ninf-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    aliases: ['mat.row-norm'],
  },
  'mat.max-norm': {
    title: 'mat.max-norm',
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
      'let { max-norm } = import("mat");\nmax-norm([[1, 2], [3, 4]])',
      'let { max-norm } = import("mat");\nmax-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
}
