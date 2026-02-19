import { LitsError } from '../../../errors'
import { assert2dVector, assert3dVector, assertMatrix, assertNonEmptyVector, assertSquareMatrix, assertVector } from '../../../typeGuards/annotatedArrays'
import { assertNumber } from '../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../builtin/interface'
import type { LitsModule } from '../interface'
import { calcMean } from '../vector/calcMean'
import { calcMedad } from '../vector/calcMedad'
import { calcMedian } from '../vector/calcMedian'
import { calcStdDev } from '../vector/calcStdDev'
import { toFixedArity } from '../../../utils/arity'
import { moduleDocs } from './docs'
import { gaussJordanElimination } from './helpers/gaussJordanElimination'
import { solve } from './helpers/solve'
import { areVectorsCollinear, areVectorsParallel } from './helpers/collinear'
import { isZeroVector } from './helpers/isZeroVector'
import { pearsonCorr } from './helpers/pearsonCorr'
import { calcFractionalRanks } from './helpers/calcFractionalRanks'
import { kendallTau } from './helpers/kendallTau'
import { calcCovariance } from './helpers/covariance'
import { calcCorrelation, extractOverlappingSegments } from './helpers/corrleation'
import { getUnit } from './helpers/getUnit'
import { dot } from './helpers/dot'
import { subtract } from './helpers/subtract'
import { scale } from './helpers/scale'
import { length } from './helpers/length'

export const linearAlgebraNormalExpression: BuiltinNormalExpressions = {
  'rotate2d': {
    evaluate: ([vector, radians], sourceCodeInfo): number[] => {
      assert2dVector(vector, sourceCodeInfo)
      if (isZeroVector(vector)) {
        return vector
      }
      assertNumber(radians, sourceCodeInfo, { finite: true })
      const cosTheta = Math.cos(radians)
      const sinTheta = Math.sin(radians)
      return [
        vector[0] * cosTheta - vector[1] * sinTheta,
        vector[0] * sinTheta + vector[1] * cosTheta,
      ]
    },
    arity: toFixedArity(2),
  },
  'rotate3d': {
    evaluate: ([vector, axis, radians], sourceCodeInfo): number[] => {
      assert3dVector(vector, sourceCodeInfo)
      if (isZeroVector(vector)) {
        return vector
      }
      assertNumber(radians, sourceCodeInfo, { finite: true })
      assert3dVector(axis, sourceCodeInfo)
      if (isZeroVector(axis)) {
        throw new LitsError('Rotation axis must not be zero', sourceCodeInfo)
      }
      const cosTheta = Math.cos(radians)
      const sinTheta = Math.sin(radians)
      const [u, v, w] = getUnit(axis, sourceCodeInfo)
      const dotProduct = vector[0] * u + vector[1] * v + vector[2] * w
      return [
        dotProduct * u * (1 - cosTheta) + vector[0] * cosTheta + (-w * vector[1] + v * vector[2]) * sinTheta,
        dotProduct * v * (1 - cosTheta) + vector[1] * cosTheta + (w * vector[0] - u * vector[2]) * sinTheta,
        dotProduct * w * (1 - cosTheta) + vector[2] * cosTheta + (-v * vector[0] + u * vector[1]) * sinTheta,
      ]
    },
    arity: toFixedArity(3),
  },
  'reflect': {
    evaluate: ([vector, normal], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertVector(normal, sourceCodeInfo)
      if (vector.length !== normal.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      if (isZeroVector(normal)) {
        throw new LitsError('Reflection normal must not be zero', sourceCodeInfo)
      }
      if (isZeroVector(vector)) {
        return vector
      }
      const unitNormal = getUnit(normal, sourceCodeInfo)
      const doubleDot = 2 * dot(vector, unitNormal)
      return subtract(vector, scale(unitNormal, doubleDot))
    },
    arity: toFixedArity(2),
  },
  'refract': {
    evaluate: ([vector, normal, eta], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertVector(normal, sourceCodeInfo)
      assertNumber(eta, sourceCodeInfo, { finite: true, positive: true })
      if (vector.length !== normal.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      if (isZeroVector(normal)) {
        throw new LitsError('Refraction normal must not be zero', sourceCodeInfo)
      }
      if (isZeroVector(vector)) {
        return vector
      }
      // Make sure vectors are normalized
      const normalizedV = getUnit(vector, sourceCodeInfo)
      const normalizedNormal = getUnit(normal, sourceCodeInfo)

      // Calculate dot product between incident vector and normal
      const dotProduct = dot(normalizedV, normalizedNormal)

      // Calculate discriminant
      const discriminant = 1 - eta * eta * (1 - dotProduct * dotProduct)

      // Check for total internal reflection
      if (discriminant < 0) {
        return vector // Total internal reflection occurs
      }

      // Calculate the refracted vector
      const scaledIncident = scale(normalizedV, eta)
      const scaledNormal = scale(
        normalizedNormal,
        eta * dotProduct + Math.sqrt(discriminant),
      )

      return subtract(scaledIncident, scaledNormal)
    },
    arity: toFixedArity(3),
  },
  'lerp': {
    evaluate: ([vectorA, vectorB, t], sourceCodeInfo): number[] => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)
      assertNumber(t, sourceCodeInfo, { finite: true })
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.map((val, i) => val + (vectorB[i]! - val) * t)
    },
    arity: toFixedArity(3),
  },
  'dot': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return dot(vectorA, vectorB)
    },
    arity: toFixedArity(2),
  },
  'cross': {
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
    arity: toFixedArity(2),
  },
  'normalize-minmax': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      if (vector.length === 0) {
        return []
      }

      const min = vector.reduce((acc, val) => (val < acc ? val : acc), vector[0]!)
      const max = vector.reduce((acc, val) => (val > acc ? val : acc), vector[0]!)

      if (min === max) {
        return vector.map(() => 0)
      }

      return vector.map(val => (val - min) / (max - min))
    },
    arity: toFixedArity(1),
  },
  'normalize-robust': {
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
    arity: toFixedArity(1),
  },
  'normalize-zscore': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)

      if (stdDev === 0) {
        return vector.map(() => 0)
      }

      return vector.map(val => (val - mean) / stdDev)
    },
    arity: toFixedArity(1),
  },
  'normalize-l1': {
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
    arity: toFixedArity(1),
  },
  'normalize-l2': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      return getUnit(vector, sourceCodeInfo)
    },
    arity: toFixedArity(1),
  },
  'normalize-log': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      if (vector.length === 0) {
        return []
      }

      const min = Math.min(...vector)

      if (min <= 0) {
        throw new LitsError('Log normalization requires all values to be positive', sourceCodeInfo)
      }

      return vector.map(val => Math.log(val / min))
    },
    arity: toFixedArity(1),
  },
  'angle': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (isZeroVector(vectorA) || isZeroVector(vectorB)) {
        throw new LitsError('Cannot calculate angle with zero-length vector', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      const magnitudeA = Math.sqrt(vectorA.reduce((acc, val) => acc + val * val, 0))
      const magnitudeB = Math.sqrt(vectorB.reduce((acc, val) => acc + val * val, 0))

      return Math.acos(dotProduct / (magnitudeA * magnitudeB))
    },
    arity: toFixedArity(2),
  },
  'projection': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number[] => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (isZeroVector(vectorB)) {
        throw new LitsError('Cannot project onto zero-length vector', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      const magnitudeB = Math.sqrt(vectorB.reduce((acc, val) => acc + val * val, 0))

      return vectorB.map(val => (dotProduct / (magnitudeB ** 2)) * val)
    },
    arity: toFixedArity(2),
  },
  'orthogonal?': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): boolean => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      return dotProduct === 0
    },
    arity: toFixedArity(2),
  },
  'parallel?': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): boolean => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return areVectorsParallel(vectorA, vectorB)
    },
    arity: toFixedArity(2),
  },
  'collinear?': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): boolean => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return areVectorsCollinear(vectorA, vectorB)
    },
    arity: toFixedArity(2),
  },
  'cosine-similarity': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (isZeroVector(vectorA) || isZeroVector(vectorB)) {
        throw new LitsError('Cannot calculate cosine similarity with zero-length vector', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const dotProduct = vectorA.reduce((acc, val, i) => acc + val * vectorB[i]!, 0)
      const magnitudeA = Math.sqrt(vectorA.reduce((acc, val) => acc + val * val, 0))
      const magnitudeB = Math.sqrt(vectorB.reduce((acc, val) => acc + val * val, 0))

      return dotProduct / (magnitudeA * magnitudeB)
    },
    arity: toFixedArity(2),
  },
  'euclidean-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return Math.sqrt(vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0))
    },
    arity: toFixedArity(2),
  },
  'euclidean-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return length(vector)
    },
    arity: toFixedArity(1),
  },
  'manhattan-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!), 0)
    },
    arity: toFixedArity(2),
  },
  'manhattan-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => acc + Math.abs(val), 0)
    },
    arity: toFixedArity(1),
  },
  'hamming-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + (val !== vectorB[i]! ? 1 : 0), 0)
    },
    arity: toFixedArity(2),
  },
  'hamming-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return vector.reduce((acc, val) => acc + (val !== 0 ? 1 : 0), 0)
    },
    arity: toFixedArity(1),
  },
  'chebyshev-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return Math.max(...vectorA.map((val, i) => Math.abs(val - vectorB[i]!)))
    },
    arity: toFixedArity(2),
  },
  'chebyshev-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return Math.max(...vector.map(val => Math.abs(val)))
    },
    arity: toFixedArity(1),
  },
  'minkowski-distance': {
    evaluate: ([vectorA, vectorB, p], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      assertNumber(p, sourceCodeInfo, { finite: true, positive: true })

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!) ** p, 0) ** (1 / p)
    },
    arity: toFixedArity(3),
  },
  'minkowski-norm': {
    evaluate: ([vector, p], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      assertNumber(p, sourceCodeInfo, { finite: true, positive: true })
      return vector.reduce((acc, val) => acc + Math.abs(val) ** p, 0) ** (1 / p)
    },
    arity: toFixedArity(2),
  },
  'cov': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      if (vectorA.length === 1) {
        return 0
      }

      return calcCovariance(vectorA, vectorB)
    },
    arity: toFixedArity(2),
  },
  'corr': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length <= 1) {
        throw new LitsError('Vectors must have at least 2 elements for corr', sourceCodeInfo)
      }

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
    arity: toFixedArity(2),
  },
  'spearman-corr': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length <= 1) {
        throw new LitsError('Vectors must have at least 2 elements for corr', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      const ranksA = calcFractionalRanks(vectorA)
      const ranksB = calcFractionalRanks(vectorB)

      try {
        return pearsonCorr(ranksA, ranksB)
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(2),
  },
  'pearson-corr': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length <= 1) {
        throw new LitsError('Vectors must have at least 2 elements for pearson-corr', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      try {
        return pearsonCorr(vectorA, vectorB)
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(2),
  },
  'kendall-tau': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length < 2) {
        throw new LitsError('Vectors must have at least 2 elements for kendall-tau', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      try {
        return kendallTau(vectorA, vectorB)
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(2),
  },
  'autocorrelation': {
    evaluate: ([vector, lag], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 2) {
        throw new LitsError('Vector must have at least 2 elements for autocorrelation', sourceCodeInfo)
      }

      assertNumber(lag, sourceCodeInfo, {
        integer: true,
        lt: vector.length,
        gt: -vector.length,
      })

      // For lag 0, return 1 (a series is perfectly correlated with itself)
      if (lag === 0) {
        return 1
      }
      const absLag = Math.abs(lag)
      const mean = calcMean(vector)

      // Calculate the numerator (sum of products of deviations)
      let numerator = 0
      const n = vector.length

      // If lag is positive, correlate current with past values
      // If lag is negative, correlate current with future values (same calculation, different interpretation)
      for (let i = 0; i < n - absLag; i++) {
        const currentIndex = lag < 0 ? i + absLag : i
        const laggedIndex = lag < 0 ? i : i + absLag

        numerator += (vector[currentIndex]! - mean) * (vector[laggedIndex]! - mean)
      }

      // Calculate the denominator (sum of squared deviations)
      let denominator = 0
      for (let i = 0; i < n; i++) {
        denominator += (vector[i]! - mean) ** 2
      }

      // Handle edge case of zero variance
      if (denominator === 0) {
        return 0 // Conventional definition
      }

      // Return the autocorrelation coefficient
      return numerator / denominator
    },
    arity: toFixedArity(2),
  },

  'cross-correlation': {
    evaluate: ([vectorA, vectorB, lag], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      if (vectorA.length < 2) {
        throw new LitsError('Vectors must have at least 2 elements', sourceCodeInfo)
      }

      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }

      assertNumber(lag, sourceCodeInfo, {
        integer: true,
        lt: vectorA.length,
        gt: -vectorA.length,
      })

      // For lag 0 between identical vectors, return 1
      if (lag === 0
        && vectorA.length === vectorB.length
        && vectorA.every((v, i) => v === vectorB[i])) {
        return 1
      }

      const [segmentA, segmentB] = extractOverlappingSegments(vectorA, vectorB, lag)
      return calcCorrelation(segmentA, segmentB)
    },
    arity: toFixedArity(3),
  },
  'rref': {
    evaluate: ([matrix], sourceCodeInfo): number[][] => {
      assertMatrix(matrix, sourceCodeInfo)

      // Reduced Row Echelon Form (RREF)
      const [rref] = gaussJordanElimination(matrix)
      return rref
    },
    arity: toFixedArity(1),
  },
  'solve': {
    evaluate: ([matrix, vector], sourceCodeInfo): number[] | null => {
      assertSquareMatrix(matrix, sourceCodeInfo)
      assertVector(vector, sourceCodeInfo)
      if (matrix.length !== vector.length) {
        throw new LitsError(`The number of rows in the matrix must be equal to the length of the vector, but got ${matrix.length} and ${vector.length}`, sourceCodeInfo)
      }
      return solve(matrix, vector)
    },
    arity: toFixedArity(2),
  },
  'to-polar': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assert2dVector(vector, sourceCodeInfo)
      if (isZeroVector(vector)) {
        return [0, 0]
      }
      const r = Math.sqrt(vector[0] ** 2 + vector[1] ** 2)
      const theta = Math.atan2(vector[1], vector[0])
      return [r, theta]
    },
    arity: toFixedArity(1),
  },
  'from-polar': {
    evaluate: ([polar], sourceCodeInfo): number[] => {
      assert2dVector(polar, sourceCodeInfo)
      const [r, theta] = polar
      if (r === 0) {
        return [0, 0]
      }
      return [r * Math.cos(theta), r * Math.sin(theta)]
    },
    arity: toFixedArity(1),
  },
}

for (const [key, docs] of Object.entries(moduleDocs)) {
  if (linearAlgebraNormalExpression[key])
    linearAlgebraNormalExpression[key].docs = docs
}

export const linearAlgebraModule: LitsModule = {
  name: 'Linear-Algebra',
  functions: linearAlgebraNormalExpression,
}
