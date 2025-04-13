import { LitsError } from '../../../../../errors'
import { assertMatrix, assertSquareMatrix, assertVector } from '../../../../../typeGuards/annotatedArrays'
import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'
import { calcMean } from '../vector/calcMean'
import { calcMedad } from '../vector/calcMedad'
import { calcMedian } from '../vector/calcMedian'
import { calcStdDev } from '../vector/calcStdDev'
import { calcVariance } from '../vector/calcVariance'
import { gaussJordanElimination } from './helpers.ts/gaussJordanElimination'
import { solve } from './helpers.ts/solve'
import { norm1 } from './helpers.ts/norm1'

export const linearAlgebraNormalExpression: BuiltinNormalExpressions = {
  'lin:dot': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
    },
    paramCount: 2,
  },
  'lin:cross': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number[] => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== 3 || vectorB.length !== 3) {
        throw new LitsError('Cross product is only defined for 3D vectors', sourceCodeInfo)
      }

      return [
        vectorA[1]! * vectorB[2]! - vectorA[2]! * vectorB[1]!,
        vectorA[2]! * vectorB[0]! - vectorA[0]! * vectorB[2]!,
        vectorA[0]! * vectorB[1]! - vectorA[1]! * vectorB[0]!,
      ] as number[]
    },
    paramCount: 2,
  },
  'lin:normalize-minmax': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const min = vector.reduce((acc, val) => (val < acc ? val : acc), vector[0]!)
      const max = vector.reduce((acc, val) => (val > acc ? val : acc), vector[0]!)

      if (min === max) {
        return vector.map(() => 0)
      }

      return vector.map(val => (val - min) / (max - min))
    },
    paramCount: 1,
  },
  'lin:normalize-robust': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      if (vector.length === 0) {
        return []
      }

      const median = calcMedian(vector)
      const medad = calcMedad(vector)

      if (medad === 0) {
        return vector.map(val => val - median)
      }
      return vector.map(val => (val - median) / medad)
    },
    paramCount: 1,
  },
  'lin:normalize-zscore': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)

      if (stdDev === 0) {
        return vector.map(() => 0)
      }

      return vector.map(val => (val - mean) / stdDev)
    },
    paramCount: 1,
  },
  'lin:normalize-l1': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length === 0) {
        return []
      }
      const norm = vector.reduce((acc, val) => acc + Math.abs(val), 0)

      if (norm === 0) {
        return vector.map(() => 0)
      }

      return vector.map(val => val / norm)
    },
    paramCount: 1,
  },
  'lin:normalize-l2': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length === 0) {
        return []
      }
      const norm = Math.sqrt(vector.reduce((acc, val) => acc + val ** 2, 0))

      if (norm === 0) {
        return vector.map(() => 0)
      }

      return vector.map(val => val / norm)
    },
    paramCount: 1,
  },
  'lin:normalize-log': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      if (vector.length === 0) {
        return []
      }

      const min = vector.reduce((acc, val) => (val < acc ? val : acc), vector[0]!)

      if (min <= 0) {
        throw new LitsError('Log normalization requires all values to be positive', sourceCodeInfo)
      }

      return vector.map(val => Math.log(val / min))
    },
    paramCount: 1,
  },
  'lin:magnitude': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val * val, 0))
    },
    paramCount: 1,
  },
  'lin:angle': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      const magnitudeA = Math.sqrt(vectorA.reduce((acc, val) => acc + val * val, 0))
      const magnitudeB = Math.sqrt(vectorB.reduce((acc, val) => acc + val * val, 0))

      return Math.acos(dotProduct / (magnitudeA * magnitudeB))
    },
    paramCount: 2,
  },
  'lin:projection': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number[] => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      const magnitudeB = Math.sqrt(vectorB.reduce((acc, val) => acc + val * val, 0))

      return vectorB.map(val => (dotProduct / (magnitudeB ** 2)) * val)
    },
    paramCount: 2,
  },
  'lin:orthogonal?': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): boolean => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      return dotProduct === 0
    },
    paramCount: 2,
  },
  'lin:parallel?': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): boolean => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const crossProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      return crossProduct === 0
    },
    paramCount: 2,
  },
  'lin:collinear?': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): boolean => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const crossProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      return crossProduct === 0
    },
    paramCount: 2,
  },
  'lin:cosine-similarity': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      const magnitudeA = Math.sqrt(vectorA.reduce((acc, val) => acc + val * val, 0))
      const magnitudeB = Math.sqrt(vectorB.reduce((acc, val) => acc + val * val, 0))

      return dotProduct / (magnitudeA * magnitudeB)
    },
    paramCount: 2,
  },
  'lin:distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return Math.sqrt(vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0))
    },
    paramCount: 2,
  },
  'lin:euclidean-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return Math.sqrt(vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0))
    },
    paramCount: 2,
  },
  'lin:manhattan-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!), 0)
    },
    paramCount: 2,
  },
  'lin:hamming-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + (val !== vectorB[i]! ? 1 : 0), 0)
    },
    paramCount: 2,
  },
  'lin:chebyshev-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return Math.max(...vectorA.map((val, i) => Math.abs(val - vectorB[i]!)))
    },
    paramCount: 2,
  },
  'lin:minkowski-distance': {
    evaluate: ([vectorA, vectorB, p], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)
      assertNumber(p, sourceCodeInfo, { finite: true })

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!) ** p, 0) ** (1 / p)
    },
    paramCount: 3,
  },
  'lin:jaccard-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      const intersection = vectorA.filter(val => vectorB.includes(val)).length
      const union = new Set([...vectorA, ...vectorB]).size

      return 1 - intersection / union
    },
    paramCount: 2,
  },
  'lin:dice-coefficient': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      const intersection = vectorA.filter(val => vectorB.includes(val)).length
      return (2 * intersection) / (vectorA.length + vectorB.length)
    },
    paramCount: 2,
  },
  'lin:levenshtein-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      const m = vectorA.length
      const n = vectorB.length
      const d: number[][] = Array.from({ length: m + 1 }, () => Array<number>(n + 1).fill(0))

      for (let i = 0; i <= m; i += 1) {
        d[i]![0] = i
      }
      for (let j = 0; j <= n; j += 1) {
        d[0]![j] = j
      }

      for (let i = 1; i <= m; i += 1) {
        for (let j = 1; j <= n; j += 1) {
          const cost = vectorA[i - 1]! === vectorB[j - 1]! ? 0 : 1
          d[i]![j] = Math.min(
            d[i - 1]![j]! + 1,
            d[i]![j - 1]! + 1,
            d[i - 1]![j - 1]! + cost,
          )
        }
      }

      return d[m]![n]!
    },
    paramCount: 2,
  },
  'lin:l1-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => acc + Math.abs(val), 0)
    },
    paramCount: 1,
  },
  'lin:l2-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val * val, 0))
    },
    paramCount: 1,
  },
  'lin:covariance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const meanA = calcMean(vectorA)
      const meanB = calcMean(vectorB)

      return vectorA.reduce((acc, val, i) => acc + (val - meanA) * (vectorB[i]! - meanB), 0) / vectorA.length
    },
    paramCount: 2,
  },
  'lin:correlation': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const meanA = calcMean(vectorA)
      const meanB = calcMean(vectorB)

      const numerator = vectorA.reduce((acc, val, i) => acc + (val - meanA) * (vectorB[i]! - meanB), 0)
      const denominator = Math.sqrt(
        vectorA.reduce((acc, val) => acc + (val - meanA) ** 2, 0) * vectorB.reduce((acc, val) => acc + (val - meanB) ** 2, 0),
      )

      return numerator / denominator
    },
    paramCount: 2,
  },
  'lin:spearman-correlation': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const rankA = [...vectorA.keys()].sort((a, b) => vectorA[a]! - vectorA[b]!)
      const rankB = [...vectorB.keys()].sort((a, b) => vectorB[a]! - vectorB[b]!)

      return vectorA.reduce((acc, _val, i) => acc + (rankA[i]! - rankB[i]!), 0) / vectorA.length
    },
    paramCount: 2,
  },
  'lin:kendall-tau': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      let concordant = 0
      let discordant = 0

      for (let i = 0; i < vectorA.length; i += 1) {
        for (let j = i + 1; j < vectorA.length; j += 1) {
          if ((vectorA[i]! - vectorA[j]!) * (vectorB[i]! - vectorB[j]!) > 0) {
            concordant += 1
          }
          else {
            discordant += 1
          }
        }
      }

      return (concordant - discordant) / Math.sqrt((concordant + discordant) ** 2)
    },
    paramCount: 2,
  },
  'lin:autocorrelation': {
    evaluate: ([vector, lag], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      const effectiveLag = lag ?? vector.length - 1
      assertNumber(effectiveLag, sourceCodeInfo, { integer: true })
      if (effectiveLag >= vector.length) {
        throw new LitsError('Lag must be less than the length of the vector', sourceCodeInfo)
      }
      const mean = calcMean(vector)
      const variance = calcVariance(vector)
      const autocovariance = vector.reduce((acc, val, i) => acc + (val - mean) * (vector[i + effectiveLag]! - mean), 0) / vector.length
      return autocovariance / variance
    },
    paramCount: { min: 1, max: 2 },
  },
  'lin:cross-correlation': {
    evaluate: ([vectorA, vectorB, lag], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      const effectiveLag = lag ?? vectorA.length - 1
      assertNumber(effectiveLag, sourceCodeInfo, { integer: true, positive: true })
      if (effectiveLag >= vectorA.length || effectiveLag >= vectorB.length) {
        throw new LitsError('Lag must be less than the length of the vectors', sourceCodeInfo)
      }
      const n = vectorA.length
      const meanA = vectorA.reduce((sum, x) => sum + x, 0) / n
      const meanB = vectorB.reduce((sum, x) => sum + x, 0) / n
      const stdA = Math.sqrt(vectorA.reduce((sum, x) => sum + (x - meanA) ** 2, 0) / n)
      const stdB = Math.sqrt(vectorB.reduce((sum, x) => sum + (x - meanB) ** 2, 0) / n)

      if (stdA === 0 || stdB === 0)
        return 0

      const overlapLength = n - Math.abs(effectiveLag)
      let sum = 0

      if (effectiveLag >= 0) {
        for (let i = 0; i < overlapLength; i++) {
          sum += (vectorA[i]! - meanA) * (vectorB[i + effectiveLag]! - meanB)
        }
      }
      else {
        for (let i = 0; i < overlapLength; i++) {
          sum += (vectorA[i - effectiveLag]! - meanA) * (vectorB[i]! - meanB)
        }
      }

      return sum / (overlapLength * stdA * stdB)
    },
    paramCount: 3,
  },
  'lin:rref': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertMatrix(matrix, sourceCodeInfo)

      // Reduced Row Echelon Form (RREF)
      const [rref] = gaussJordanElimination(matrix)
      return rref
    },
    paramCount: 1,
  },
  'lin:rank': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      const [, result] = gaussJordanElimination(matrix)
      return result
    },
    paramCount: 1,
  },
  'lin:solve': {
    evaluate: ([matrix, vector], sourceCodeInfo): number[] | null => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      assertVector(vector, sourceCodeInfo)
      if (matrix.length !== vector.length) {
        throw new LitsError(`The number of rows in the matrix must be equal to the length of the vector, but got ${matrix.length} and ${vector.length}`, sourceCodeInfo)
      }
      return solve(matrix, vector)
    },
    paramCount: 2,
  },
  // Frobenius norm
  'lin:norm-frobenius': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return Math.sqrt(matrix.reduce((sum, row) => sum + row.reduce((rowSum, cell) => rowSum + cell * cell, 0), 0))
    },
    paramCount: 1,
  },
  // 1-norm
  'lin:norm-1': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return norm1(matrix)
    },
    paramCount: 1,
  },
  // Infinity norm
  'lin:norm-infinity': {
    evaluate: ([matrix], sourceCodeInfo): number => {
      assertMatrix(matrix, sourceCodeInfo)
      return matrix.reduce((max, row) => Math.max(max, row.reduce((sum, cell) => sum + Math.abs(cell), 0)), 0)
    },
    paramCount: 1,
  },
  // Max norm
  'lin:norm-max': {
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
