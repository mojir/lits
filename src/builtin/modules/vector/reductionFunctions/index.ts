import { LitsError } from '../../../../errors'
import { assertVector } from '../../../../typeGuards/annotatedArrays'
import { assertNumber } from '../../../../typeGuards/number'
import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../../../../builtin/interface'
import { toFixedArity } from '../../../../utils/arity'
import { maxReductionFunction } from './max'
import { geometricMeanReductionFunction, harmonicMeanReductionFunction, meanReductionFunction } from './mean'
import { medianReductionFunction } from './median'
import { minReductionFunction } from './min'
import { prodReductionFunction } from './prod'
import { sumReductionFunction } from './sum'
import { sampleVarianceReductionFunction, varianceReductionFunction } from './variance'
import { sampleStdevReductionFunction, stdevReductionFunction } from './standardDeviation'
import { iqrReductionFunction } from './iqr'
import { spanReductionFunction } from './span'
import { sampleSkewnessReductionFunction, skewnessReductionFunction } from './skewness'
import { eccessKurtosisReductionFunction, kurtosisReductionFunction, sampleExcessKurtosisReductionFunction, sampleKurtosisReductionFunction } from './kurtosis'
import { rmsReductionFunction } from './rms'
import { madReductionFunction } from './mad'
import { medadReductionFunction } from './medad'
import { giniCoefficientReductionFunction } from './giniCoefficient'
import { entropyReductionFunction } from './entropy'

type VectorReductionKey<T extends string> = `${T}`
type VectorMovingWindowKey<T extends string> = `moving-${T}`
type VectorCenteredMovingWindowKey<T extends string> = `centered-moving-${T}`
type VectorRunningKey<T extends string> = `running-${T}`

export type VectorReductionKeys<T extends string> = VectorReductionKey<T> | VectorMovingWindowKey<T> | VectorCenteredMovingWindowKey<T> | VectorRunningKey<T>

type ReductionFunction = (vector: number[]) => number
// type MovingWindowFunction = (vector: number[], windowSize: number, sourceCodeInfo: SourceCodeInfo | undefined) => number[]

export type ReductionFunctionDefinition<T extends string> = Record<VectorReductionKey<T>, ReductionFunction> & {
  minLength?: number
  padding?: number
}

export const reductionFunctionNormalExpressions: BuiltinNormalExpressions = {}

addReductionFunctions(meanReductionFunction)
addReductionFunctions(geometricMeanReductionFunction)
addReductionFunctions(harmonicMeanReductionFunction)
addReductionFunctions(medianReductionFunction)
addReductionFunctions(sumReductionFunction)
addReductionFunctions(prodReductionFunction)
addReductionFunctions(minReductionFunction)
addReductionFunctions(maxReductionFunction)
addReductionFunctions(varianceReductionFunction)
addReductionFunctions(sampleVarianceReductionFunction)
addReductionFunctions(stdevReductionFunction)
addReductionFunctions(sampleStdevReductionFunction)
addReductionFunctions(iqrReductionFunction)
addReductionFunctions(spanReductionFunction)
addReductionFunctions(skewnessReductionFunction)
addReductionFunctions(sampleSkewnessReductionFunction)
addReductionFunctions(eccessKurtosisReductionFunction)
addReductionFunctions(kurtosisReductionFunction)
addReductionFunctions(sampleExcessKurtosisReductionFunction)
addReductionFunctions(sampleKurtosisReductionFunction)
addReductionFunctions(rmsReductionFunction)
addReductionFunctions(madReductionFunction)
addReductionFunctions(medadReductionFunction)
addReductionFunctions(giniCoefficientReductionFunction)
addReductionFunctions(entropyReductionFunction)

function addReductionFunctions<T extends string>(fns: ReductionFunctionDefinition<T>) {
  for (const [key, value] of Object.entries(fns)) {
    /* v8 ignore next 3 */
    if (reductionFunctionNormalExpressions[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    if (key !== 'minLength' && key !== 'padding' && typeof value === 'function') {
      const reductionFn = value as ReductionFunction
      const baseKey = key.replace(/^/, '')
      const movingKey = `moving-${baseKey}` as VectorMovingWindowKey<T>
      const centeredMovingKey = `centered-moving-${baseKey}` as VectorCenteredMovingWindowKey<T>
      const runningKey = `running-${baseKey}` as VectorRunningKey<T>
      const minLength = fns.minLength ?? 1
      assertNumber(minLength, undefined, { integer: true, finite: true, gte: 0 })
      reductionFunctionNormalExpressions[key] = createReductionNormalExpression(reductionFn, minLength)
      reductionFunctionNormalExpressions[movingKey] = createMovingNormalExpression(reductionFn, minLength)
      reductionFunctionNormalExpressions[centeredMovingKey] = createCenteredMovingNormalExpression(reductionFn, minLength, fns.padding ?? null)
      reductionFunctionNormalExpressions[runningKey] = createRunningNormalExpression(reductionFn, minLength)
    }
  }
}

function createReductionNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
): BuiltinNormalExpression<number> {
  return {
    evaluate: ([vector], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < minLength) {
        throw new LitsError(`Vector length must be at least ${minLength}`, sourceCodeInfo)
      }

      try {
        return reductionFunction(vector)
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(1),
  }
}

function createMovingNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
): BuiltinNormalExpression<number[]> {
  return {
    evaluate: ([vector, windowSize], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, finite: true, gte: minLength, lte: vector.length })
      if (vector.length === 0) {
        return []
      }
      try {
        if (windowSize >= vector.length) {
          return [reductionFunction(vector)]
        }
        const result = []
        for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
          result.push(reductionFunction(vector.slice(i, i + windowSize)))
        }
        return result
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(2),
  }
}

function createCenteredMovingNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
  padding: number | null,
): BuiltinNormalExpression<(number | null)[]> {
  return {
    evaluate: ([vector, windowSize, leftPadding, rightPadding], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < minLength) {
        throw new LitsError(`Vector length must be at least ${minLength}`, sourceCodeInfo)
      }

      assertNumber(windowSize, sourceCodeInfo, { integer: true, finite: true, gte: minLength, lte: vector.length })
      leftPadding = leftPadding ?? padding
      if (leftPadding !== null) {
        assertNumber(leftPadding, sourceCodeInfo, { finite: true })
      }
      rightPadding = rightPadding ?? padding
      if (rightPadding !== null) {
        assertNumber(rightPadding, sourceCodeInfo, { finite: true })
      }

      if (vector.length === 0) {
        return []
      }

      const halfWindowSize = Math.floor(windowSize / 2)
      const paddedVector = [
        ...Array<number | null>(halfWindowSize).fill(leftPadding),
        ...vector,
        ...Array<number | null>(halfWindowSize).fill(rightPadding),
      ]

      const start = typeof leftPadding === 'number' ? 0 : halfWindowSize
      const end = vector.length - (typeof rightPadding === 'number' ? 0 : (windowSize - halfWindowSize - 1))

      const result: (number | null)[] = [
        ...Array<null>(start).fill(null),
      ]

      try {
        for (let i = start; i < end; i += 1) {
          result.push(reductionFunction(paddedVector.slice(i, i + windowSize) as number[]))
        }
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }

      result.push(...Array<null>(vector.length - end).fill(null))
      return result
    },
    arity: { min: 2, max: 4 },
  }
}

function createRunningNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
): BuiltinNormalExpression<(number | null)[]> {
  return {
    evaluate: ([vector], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < minLength) {
        throw new LitsError(`Vector length must be at least ${minLength}`, sourceCodeInfo)
      }

      if (vector.length === 0) {
        return []
      }

      try {
        const nullsCount = Math.max(minLength - 1, 0)
        const result: (number | null)[] = Array<null>(nullsCount).fill(null)

        for (let i = nullsCount; i < vector.length; i += 1) {
          result.push(reductionFunction(vector.slice(0, i + 1)))
        }
        return result
      }
      catch (error) {
        throw new LitsError(error, sourceCodeInfo)
      }
    },
    arity: toFixedArity(1),
  }
}
