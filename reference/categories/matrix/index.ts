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
    description: 'Multiplies two `matrices` using standard matrix multiplication based on **dot products** of rows and columns.',
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
        description: 'The matrix to calculate the determinant of.',
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
        description: 'The matrix to calculate the inverse of.',
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
        description: 'The matrix to calculate the adjugate of.',
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
        description: 'The matrix to calculate the cofactor of.',
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
        description: 'The matrix to calculate the minor of.',
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
        description: 'The matrix to calculate the trace of.',
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
        description: 'The matrix to check for symmetry.',
      },
    },
    variants: [
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a matrix is **symmetric**.',
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
        description: 'The matrix to check for triangularity.',
      },
    },
    variants: [
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a matrix is **triangular**.',
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
        description: 'The matrix to check for upper triangularity.',
      },
    },
    variants: [
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a matrix is **upper triangular**.',
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
        description: 'The matrix to check for lower triangularity.',
      },
    },
    variants: [
      { argumentNames: ['m'] },
    ],
    description: 'Checks if a matrix is **lower triangular**.',
    examples: [
      'mat:lower-triangular?([[1, 0], [2, 3]])',
      'mat:lower-triangular?([[1, 0, 0], [2, 3, 0], [4, 5, 6]])',
    ],
  },
}
