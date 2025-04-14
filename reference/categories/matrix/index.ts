import type { FunctionReference } from '../..'
import { type MatrixApiName, getOperatorArgs } from '../../api'

export const matrixReference: Record<MatrixApiName, FunctionReference<'Matrix'>> = {
  'mat:mul': {
    title: 'mat:mul',
    category: 'Matrix',
    linkName: 'mat-colon-mul',
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
      'mat:mul([[1, 2], [3, 4]], [[5, 6], [7, 8]])',
      'mat:mul([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]])',
    ],
  },
  'mat:det': {
    title: 'mat:det',
    category: 'Matrix',
    linkName: 'mat-colon-det',
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
      'mat:det([[1, 2], [3, 4]])',
      'mat:det([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat:inv': {
    title: 'mat:inv',
    category: 'Matrix',
    linkName: 'mat-colon-inv',
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
      'mat:inv([[1, 2], [3, 4]])',
      'mat:inv([[1, 2, 3], [4, 5, 7], [7, 8, 10]])',
    ],
  },
  'mat:adj': {
    title: 'mat:adj',
    category: 'Matrix',
    linkName: 'mat-colon-adj',
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
      'mat:adj([[1, 2], [3, 4]])',
      'mat:adj([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'mat:adj([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',

    ],
  },
  'mat:cofactor': {
    title: 'mat:cofactor',
    category: 'Matrix',
    linkName: 'mat-colon-cofactor',
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
      'mat:cofactor([[1, 2], [3, 4]])',
      'mat:cofactor([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'mat:cofactor([[1, 2, 3], [7, 8, 9], [4, 5, 6]])',

    ],
  },
  'mat:minor': {
    title: 'mat:minor',
    category: 'Matrix',
    linkName: 'mat-colon-minor',
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
      'mat:minor([[1, 2], [3, 4]], 0, 1)',
      'mat:minor([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1)',
    ],
  },
  'mat:trace': {
    title: 'mat:trace',
    category: 'Matrix',
    linkName: 'mat-colon-trace',
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
      'mat:trace([[1, 2], [3, 4]])',
      'mat:trace([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat:symmetric?': {
    title: 'mat:symmetric?',
    category: 'Matrix',
    linkName: 'mat-colon-symmetric-question',
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
      'mat:symmetric?([[1, 2], [2, 1]])',
      'mat:symmetric?([[1, 2, 3], [2, 1, 4], [3, 4, 1]])',
    ],
  },
  'mat:triangular?': {
    title: 'mat:triangular?',
    category: 'Matrix',
    linkName: 'mat-colon-triangular-question',
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
      'mat:triangular?([[2, 0], [0, 1]])',
      'mat:triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'mat:upper-triangular?': {
    title: 'mat:upper-triangular?',
    category: 'Matrix',
    linkName: 'mat-colon-upper-triangular-question',
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
      'mat:upper-triangular?([[1, 2], [0, 3]])',
      'mat:upper-triangular?([[1, 2, 3], [0, 4, 5], [0, 0, 6]])',
    ],
  },
  'mat:lower-triangular?': {
    title: 'mat:lower-triangular?',
    category: 'Matrix',
    linkName: 'mat-colon-lower-triangular-question',
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
      'mat:lower-triangular?([[1, 0], [2, 3]])',
      'mat:lower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])',
    ],
  },
  'mat:diagonal?': {
    title: 'mat:diagonal?',
    category: 'Matrix',
    linkName: 'mat-colon-diagonal-question',
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
      'mat:diagonal?([[1, 0], [0, 2]])',
      'mat:diagonal?([[1, 0, 0], [0, 2, 0], [0, 0, 3]])',
      'mat:diagonal?([[1, 0, 0], [2, 2, 2], [0, 0, 3]])',
    ],
  },
  'mat:square?': {
    title: 'mat:square?',
    category: 'Matrix',
    linkName: 'mat-colon-square-question',
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
      'mat:square?([[1, 2], [3, 4]])',
      'mat:square?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'mat:square?([[1, 2, 3], [4, 5, 6]])',
    ],
  },
  'mat:orthogonal?': {
    title: 'mat:orthogonal?',
    category: 'Matrix',
    linkName: 'mat-colon-orthogonal-question',
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
      'mat:orthogonal?([[1, 0], [0, 1]])',
      'mat:orthogonal?([[1, 0], [0, -1]])',
      'mat:orthogonal?([[1, 2], [3, 4]])',
    ],
  },
  'mat:identity?': {
    title: 'mat:identity?',
    category: 'Matrix',
    linkName: 'mat-colon-identity-question',
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
      'mat:identity?([[1, 0], [0, 1]])',
      'mat:identity?([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'mat:identity?([[1, 0, 0], [0, 1, 0], [0, 0, 0]])',
    ],
  },
  'mat:invertible?': {
    title: 'mat:invertible?',
    category: 'Matrix',
    linkName: 'mat-colon-invertible-question',
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
      'mat:invertible?([[1, 2], [3, 4]])',
      'mat:invertible?([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'mat:invertible?([[1, 2], [2, 4]])',
    ],
  },
  'mat:hilbert': {
    title: 'mat:hilbert',
    category: 'Matrix',
    linkName: 'mat-colon-hilbert',
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
      'mat:hilbert(3)',
      'mat:hilbert(4)',
    ],
  },
  'mat:vandermonde': {
    title: 'mat:vandermonde',
    category: 'Matrix',
    linkName: 'mat-colon-vandermonde',
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
      'mat:vandermonde([1, 2, 3])',
      'mat:vandermonde([1, 0, 1])',
    ],
  },
  'mat:band': {
    title: 'mat:band',
    category: 'Matrix',
    linkName: 'mat-colon-band',
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
      'mat:band(3, 1, 1)',
      'mat:band(4, 1, 2)',
    ],
  },
  'mat:banded?': {
    title: 'mat:banded?',
    category: 'Matrix',
    linkName: 'mat-colon-banded-question',
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
      `mat:banded?([
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 1, 1, 1],
  [0, 1, 1, 1],
], 2, 2)`,
      `mat:banded?([
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 1, 1, 1],
  [0, 1, 1, 1],
], 1, 1)`,
    ],
  },
  'mat:rank': {
    title: 'mat:rank',
    category: 'Matrix',
    linkName: 'mat-colon-rank',
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
      'mat:rank([[1, 0, 0], [0, 1, 0], [0, 0, 1]])',
      'mat:rank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
      'mat:rank([[2, 4, 6], [3, 6, 9], [4, 8, 12]])',
    ],
  },
  'mat:frobenius-norm': {
    title: 'mat:frobenius-norm',
    category: 'Matrix',
    linkName: 'mat-colon-frobenius-norm',
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
      'mat:frobenius-norm([[1, 2], [3, 4]])',
      'mat:frobenius-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat:1-norm': {
    title: 'mat:1-norm',
    category: 'Matrix',
    linkName: 'mat-colon-1-norm',
    returns: {
      type: 'number',
    },
    args: {
      m: {
        type: 'matrix',
        description: 'The `matrix` to calculate the 1-norm of.',
      },
    },
    variants: [
      { argumentNames: ['m'] },
    ],
    description: 'Calculates the **1-norm** of a matrix.',
    examples: [
      'mat:1-norm([[1, 2], [3, 4]])',
      'mat:1-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
  'mat:inf-norm': {
    title: 'mat:inf-norm',
    category: 'Matrix',
    linkName: 'mat-colon-inf-norm',
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
      'mat:inf-norm([[1, 2], [3, 4]])',
      'mat:inf-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
    aliases: ['mat:row-norm'],
  },
  'mat:max-norm': {
    title: 'mat:max-norm',
    category: 'Matrix',
    linkName: 'mat-colon-max-norm',
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
      'mat:max-norm([[1, 2], [3, 4]])',
      'mat:max-norm([[1, 2, 3], [4, 5, 6], [7, 8, 9]])',
    ],
  },
}
