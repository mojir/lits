import { LitsError } from '../../errors'
import { assertMatrix, assertSquareMatrix, assertVector, isSquareMatrix } from '../../typeGuards/annotatedArrays'
import { assertNumber } from '../../typeGuards/number'
import { approxZero } from '../../utils'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../../builtin/interface'
import type { LitsNamespace } from '../interface'
import { gaussJordanElimination } from '../linearAlgebra/helpers/gaussJordanElimination'
import { adjugate } from './helpers/adjugate'
import { band } from './helpers/band'
import { cofactor } from './helpers/cofactor'
import { determinant } from './helpers/determinant'
import { inverse } from './helpers/inverse'
import { isBanded } from './helpers/isBanded'
import { isDiagonal } from './helpers/isDiagonal'
import { isIdentity } from './helpers/isIdentity'
import { isOrthogonal } from './helpers/isOrthogonal'
import { isSquare } from './helpers/isSquare'
import { isSymetric } from './helpers/isSymetric'
import { isTriangular, isTriangularLower, isTriangularUpper } from './helpers/isTriangular'
import { matrixMultiply } from './helpers/matrixMultiply'
import { minor } from './helpers/minor'
import { norm1 } from './helpers/norm1'
import { trace } from './helpers/trace'

export const matrixNormalExpression: BuiltinNormalExpressions = {
  'mul': {
    evaluate: ([matrix1, matrix2], sourceCodeInfo): number[][] => {
      assertMatrix(matrix1, sourceCodeInfo)
      assertMatrix(matrix2, sourceCodeInfo)
      try {
        return matrixMultiply(matrix1, matrix2)
      }
      catch (error) {
        throw new LitsError(`The number of columns in the first matrix must be equal to the number of rows in the second matrix, but got ${matrix1[0]!.length} and ${matrix2.length}`, sourceCodeInfo)
      }
    },
    arity: toFixedArity(2),
  },
  'det': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return determinant(matrix)
    },
    arity: toFixedArity(1),
  },
  'inv': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      const result = inverse(matrix)
      if (result === null) {
        throw new LitsError('The matrix must be invertible', sourceCodeInfo)
      }
      return result
    },
    arity: toFixedArity(1),
  },
  'adj': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return adjugate(matrix)
    },
    arity: toFixedArity(1),
  },
  'cofactor': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return cofactor(matrix)
    },
    arity: toFixedArity(1),
  },
  'minor': {
    evaluate: ([matrix, row, col], sourceCodeInfo): number[][] => {
      assertMatrix(matrix, sourceCodeInfo)
      assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lte: matrix.length })
      assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lte: matrix[0]!.length })

      return minor(matrix, row, col)
    },
    arity: toFixedArity(3),
  },
  'trace': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return trace(matrix)
    },
    arity: toFixedArity(1),
  },
  'symmetric?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isSymetric(matrix)
    },
    arity: toFixedArity(1),
  },
  'triangular?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isTriangular(matrix)
    },
    arity: toFixedArity(1),
  },
  'upper-triangular?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isTriangularUpper(matrix)
    },
    arity: toFixedArity(1),
  },
  'lower-triangular?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isTriangularLower(matrix)
    },
    arity: toFixedArity(1),
  },
  'diagonal?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isDiagonal(matrix)
    },
    arity: toFixedArity(1),
  },
  'square?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isSquare(matrix)
    },
    arity: toFixedArity(1),
  },
  'orthogonal?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isOrthogonal(matrix)
    },
    arity: toFixedArity(1),
  },
  'identity?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isIdentity(matrix)
    },
    arity: toFixedArity(1),
  },
  'invertible?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      if (!isSquareMatrix(matrix)) {
        return false
      }
      return !approxZero(determinant(matrix))
    },
    arity: toFixedArity(1),
  },
  'hilbert': {
    evaluate: ([size], sourceCodeInfo): number[][] => {
      assertNumber(size, sourceCodeInfo, { integer: true, positive: true })
      const result: number[][] = []
      for (let i = 0; i < size; i += 1) {
        const row: number[] = []
        for (let j = 0; j < size; j += 1) {
          row.push(1 / (i + j + 1))
        }
        result.push(row)
      }
      return result
    },
    arity: toFixedArity(1),
  },
  'vandermonde': {
    evaluate: ([vector], sourceCodeInfo): number[][] => {
      assertVector(vector, sourceCodeInfo)
      const result: number[][] = []
      for (let i = 0; i < vector.length; i += 1) {
        const row: number[] = []
        for (let j = 0; j < vector.length; j += 1) {
          row.push((vector[i]!) ** j)
        }
        result.push(row)
      }
      return result
    },
    arity: toFixedArity(1),
  },
  'band': {
    evaluate: ([n, lband, uband], sourceCodeInfo): number[][] => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(lband, sourceCodeInfo, { integer: true, nonNegative: true, lt: n })
      assertNumber(uband, sourceCodeInfo, { integer: true, nonNegative: true, lte: n })
      return band(n, lband, uband)
    },
    arity: toFixedArity(3),
  },
  'banded?': {
    evaluate: ([matrix, lband, uband], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      const maxBand = Math.max(matrix.length, matrix[0]!.length)
      assertNumber(lband, sourceCodeInfo, { integer: true, nonNegative: true, lt: maxBand })
      assertNumber(uband, sourceCodeInfo, { integer: true, nonNegative: true, lt: maxBand })
      return isBanded(matrix, lband, uband)
    },
    arity: toFixedArity(3),
  },
  'rank': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      const [, result] = gaussJordanElimination(matrix)
      return result
    },
    arity: toFixedArity(1),
  },
  // Frobenius norm
  'frobenius-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return Math.sqrt(matrix.reduce((sum, row) => sum + row.reduce((rowSum, cell) => rowSum + cell * cell, 0), 0))
    },
    arity: toFixedArity(1),
  },
  // one-norm (column norm)
  'one-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return norm1(matrix)
    },
    arity: toFixedArity(1),
  },
  // Infinity norm
  'inf-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return matrix.reduce((max, row) => Math.max(max, row.reduce((sum, cell) => sum + Math.abs(cell), 0)), 0)
    },
    arity: toFixedArity(1),
    aliases: ['row-norm'],
  },
  // Max norm
  'max-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return matrix.reduce((maxVal, row) => {
        const rowMax = row.reduce((max, val) => Math.max(max, Math.abs(val)), 0)
        return Math.max(maxVal, rowMax)
      }, 0)
    },
    arity: toFixedArity(1),
  },
}

export const matrixNamespace: LitsNamespace = {
  name: 'mat',
  functions: matrixNormalExpression,
}
