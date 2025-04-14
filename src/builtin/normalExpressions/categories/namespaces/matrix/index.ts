import { LitsError } from '../../../../../errors'
import { assertMatrix, assertSquareMatrix, assertVector, isSquareMatrix } from '../../../../../typeGuards/annotatedArrays'
import { assertNumber } from '../../../../../typeGuards/number'
import { approxZero } from '../../../../../utils'
import type { BuiltinNormalExpressions } from '../../../../interface'
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
  'mat:mul': {
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
    paramCount: 2,
  },
  'mat:det': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return determinant(matrix)
    },
    paramCount: 1,
  },
  'mat:inv': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      const result = inverse(matrix)
      if (result === null) {
        throw new LitsError('The matrix must be invertible', sourceCodeInfo)
      }
      return result
    },
    paramCount: 1,
  },
  'mat:adj': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return adjugate(matrix)
    },
    paramCount: 1,
  },
  'mat:cofactor': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return cofactor(matrix)
    },
    paramCount: 1,
  },
  'mat:minor': {
    evaluate: ([matrix, row, col], sourceCodeInfo): number[][] => {
      assertMatrix(matrix, sourceCodeInfo)
      assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lte: matrix.length })
      assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lte: matrix[0]!.length })

      return minor(matrix, row, col)
    },
    paramCount: 3,
  },
  'mat:trace': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      return trace(matrix)
    },
    paramCount: 1,
  },
  'mat:symmetric?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isSymetric(matrix)
    },
    paramCount: 1,
  },
  'mat:triangular?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isTriangular(matrix)
    },
    paramCount: 1,
  },
  'mat:upper-triangular?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isTriangularUpper(matrix)
    },
    paramCount: 1,
  },
  'mat:lower-triangular?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isTriangularLower(matrix)
    },
    paramCount: 1,
  },
  'mat:diagonal?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isDiagonal(matrix)
    },
    paramCount: 1,
  },
  'mat:square?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isSquare(matrix)
    },
    paramCount: 1,
  },
  'mat:orthogonal?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isOrthogonal(matrix)
    },
    paramCount: 1,
  },
  'mat:identity?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      return isIdentity(matrix)
    },
    paramCount: 1,
  },
  'mat:invertible?': {
    evaluate: ([matrix], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      if (!isSquareMatrix(matrix)) {
        return false
      }
      return !approxZero(determinant(matrix))
    },
    paramCount: 1,
  },
  'mat:hilbert': {
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
    paramCount: 1,
  },
  'mat:vandermonde': {
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
    paramCount: 1,
  },
  'mat:band': {
    evaluate: ([n, lband, uband], sourceCodeInfo): number[][] => {
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(lband, sourceCodeInfo, { integer: true, nonNegative: true, lt: n })
      assertNumber(uband, sourceCodeInfo, { integer: true, nonNegative: true, lte: n })
      return band(n, lband, uband)
    },
    paramCount: 3,
  },
  'mat:banded?': {
    evaluate: ([matrix, lband, uband], sourceCodeInfo): boolean => {
      assertMatrix(matrix, sourceCodeInfo)
      const maxBand = Math.max(matrix.length, matrix[0]!.length)
      assertNumber(lband, sourceCodeInfo, { integer: true, nonNegative: true, lt: maxBand })
      assertNumber(uband, sourceCodeInfo, { integer: true, nonNegative: true, lt: maxBand })
      return isBanded(matrix, lband, uband)
    },
    paramCount: 3,
  },
  'mat:rank': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      const [, result] = gaussJordanElimination(matrix)
      return result
    },
    paramCount: 1,
  },
  // Frobenius norm
  'mat:frobenius-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return Math.sqrt(matrix.reduce((sum, row) => sum + row.reduce((rowSum, cell) => rowSum + cell * cell, 0), 0))
    },
    paramCount: 1,
  },
  // 1-norm
  'mat:1-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return norm1(matrix)
    },
    paramCount: 1,
  },
  // Infinity norm
  'mat:inf-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return matrix.reduce((max, row) => Math.max(max, row.reduce((sum, cell) => sum + Math.abs(cell), 0)), 0)
    },
    paramCount: 1,
    aliases: ['mat:row-norm'],
  },
  // Max norm
  'mat:max-norm': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return matrix.reduce((maxVal, row) => {
        const rowMax = row.reduce((max, val) => Math.max(max, Math.abs(val)), 0)
        return Math.max(maxVal, rowMax)
      }, 0)
    },
    paramCount: 1,
  },
}
