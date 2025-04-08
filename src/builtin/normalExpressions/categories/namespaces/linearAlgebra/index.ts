import { LitsError } from '../../../../../errors'
import { assertVector } from '../../../../../typeGuards/annotatedArrays'
import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'
import { calcMean } from '../vector/calcMean'
import { calcVariance } from '../vector/calcVariance'

export const linearAlgebraNormalExpression: BuiltinNormalExpressions = {
  'l:dot': {
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
  'l:cross': {
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
  'l:normalize': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const magnitude = Math.sqrt(vector.reduce((acc, val) => acc + val * val, 0))

      if (magnitude === 0) {
        throw new LitsError('Cannot normalize a zero vector', sourceCodeInfo)
      }

      return vector.map(val => val / magnitude)
    },
    paramCount: 1,
  },
  'l:magnitude': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val * val, 0))
    },
    paramCount: 1,
  },
  'l:angle': {
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
  'l:projection': {
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
  'l:orthogonal?': {
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
  'l:parallel?': {
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
  'l:collinear?': {
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
  'l:cosine-similarity': {
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
  'l:distance': {
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
  'l:euclidean-distance': {
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
  'l:manhattan-distance': {
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
  'l:hamming-distance': {
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
  'l:chebyshev-distance': {
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
  'l:minkowski-distance': {
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
  'l:jaccard-distance': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      const intersection = vectorA.filter(val => vectorB.includes(val)).length
      const union = new Set([...vectorA, ...vectorB]).size

      return 1 - intersection / union
    },
    paramCount: 2,
  },
  'l:dice-coefficient': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)

      const intersection = vectorA.filter(val => vectorB.includes(val)).length
      return (2 * intersection) / (vectorA.length + vectorB.length)
    },
    paramCount: 2,
  },
  'l:levenshtein-distance': {
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
  'l:l1-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => acc + Math.abs(val), 0)
    },
    paramCount: 1,
  },
  'l:l2-norm': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val * val, 0))
    },
    paramCount: 1,
  },
  'l:covariance': {
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
  'l:correlation': {
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
  'l:spearman-correlation': {
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
  'l:kendall-tau': {
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
  'l:autocorrelation': {
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
  'l:cross-correlation': {
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
}
