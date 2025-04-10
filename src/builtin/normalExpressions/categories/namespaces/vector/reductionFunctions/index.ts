import { LitsError } from '../../../../../../errors'
import { assertVector } from '../../../../../../typeGuards/annotatedArrays'
import { assertNumber } from '../../../../../../typeGuards/number'
import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../../../../../interface'
import { maxReductionFunction } from './max'
import { meanReductionFunction } from './mean'
import { minReductionFunction } from './min'
import { prodReductionFunction } from './prod'
import { sumReductionFunction } from './sum'

type VectorReductionKey<T extends string> = `vec:${T}`
type VectorMovingWindowKey<T extends string> = `vec:moving-${T}`
type VectorCenteredMovingWindowKey<T extends string> = `vec:centered-moving-${T}`
type VectorRunningKey<T extends string> = `vec:running-${T}`

export type VectorReductionKeys<T extends string> = VectorReductionKey<T> | VectorMovingWindowKey<T> | VectorCenteredMovingWindowKey<T> | VectorRunningKey<T>

type ReductionFunction = (vector: number[]) => number
// type MovingWindowFunction = (vector: number[], windowSize: number, sourceCodeInfo: SourceCodeInfo | undefined) => number[]

export type ReductionFunctionDefinition<T extends string> = Record<VectorReductionKey<T>, ReductionFunction> & {
  minLength: number
  paddingValue: number
  rightPaddingValue?: number
}

export const reductionFunctionNormalExpressions: BuiltinNormalExpressions = {}

addReductionFunctions(meanReductionFunction)
addReductionFunctions(sumReductionFunction)
addReductionFunctions(prodReductionFunction)
addReductionFunctions(minReductionFunction)
addReductionFunctions(maxReductionFunction)

function addReductionFunctions(fns: ReductionFunctionDefinition<string>) {
  for (const [key, value] of Object.entries(fns)) {
    /* v8 ignore next 3 */
    if (reductionFunctionNormalExpressions[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    if (key.startsWith('vec:')) {
      const movingKey = key.replace('vec:', 'vec:moving-') as VectorMovingWindowKey<string>
      const centeredMovingKey = key.replace('vec:', 'vec:centered-moving-') as VectorCenteredMovingWindowKey<string>
      const runningKey = key.replace('vec:', 'vec:running-') as VectorRunningKey<string>
      reductionFunctionNormalExpressions[key] = createReductionNormalExpression(value as ReductionFunction, fns.minLength)
      reductionFunctionNormalExpressions[movingKey] = createMovingNormalExpression(value as ReductionFunction, fns.minLength)
      reductionFunctionNormalExpressions[centeredMovingKey] = createCenteredMovingNormalExpression(value as ReductionFunction, fns.minLength, fns.paddingValue, fns.rightPaddingValue ?? fns.paddingValue)
      reductionFunctionNormalExpressions[runningKey] = createRunningNormalExpression(value as ReductionFunction, fns.minLength)
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
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  }
}

function createMovingNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
): BuiltinNormalExpression<number[]> {
  return {
    evaluate: ([vector, windowSize], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, finite: true, gte: minLength })
      if (vector.length < minLength) {
        throw new LitsError(`Vector length must be at least ${minLength}`, sourceCodeInfo)
      }

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
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 2,
  }
}

function createCenteredMovingNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
  defaultLeftPaddingValue: number,
  defaultRightPaddingValue: number,
): BuiltinNormalExpression<number[]> {
  return {
    evaluate: ([vector, windowSize, padding, rightPadding], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < minLength) {
        throw new LitsError(`Vector length must be at least ${minLength}`, sourceCodeInfo)
      }

      assertNumber(windowSize, sourceCodeInfo, { integer: true, finite: true, gte: minLength })
      const leftPaddingValue = padding ?? defaultLeftPaddingValue
      assertNumber(leftPaddingValue, sourceCodeInfo, { finite: true })
      const rightPaddingValue = rightPadding ?? padding ?? defaultRightPaddingValue
      assertNumber(rightPaddingValue, sourceCodeInfo, { finite: true })

      try {
        const result = []
        const halfWindowSize = Math.floor(windowSize / 2)
        const paddedVector = [
          ...Array<number>(halfWindowSize).fill(leftPaddingValue),
          ...vector,
          ...Array<number>(halfWindowSize).fill(rightPaddingValue),
        ]
        for (let i = 0; i < vector.length; i += 1) {
          result.push(reductionFunction(paddedVector.slice(i, i + windowSize)))
        }
        return result
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: { min: 2, max: 4 },
  }
}

function createRunningNormalExpression(
  reductionFunction: ReductionFunction,
  minLength: number,
): BuiltinNormalExpression<number[]> {
  return {
    evaluate: ([vector], sourceCodeInfo) => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < minLength) {
        throw new LitsError(`Vector length must be at least ${minLength}`, sourceCodeInfo)
      }

      try {
        const result = []
        for (let i = 0; i < vector.length; i += 1) {
          result.push(reductionFunction(vector.slice(0, i + 1)))
        }
        return result
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  }
}
