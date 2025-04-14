import { LitsError } from '../../../../../errors'
import type { Any, Arr } from '../../../../../interface'
import { assertArray } from '../../../../../typeGuards/array'
import { asAny } from '../../../../../typeGuards/lits'
import { assertNumber } from '../../../../../typeGuards/number'
import { assertString } from '../../../../../typeGuards/string'
import type { BuiltinNormalExpressions } from '../../../../interface'

export const randomNormalExpression: BuiltinNormalExpressions = {
  '!:random': {
    evaluate: (): number => {
      return Math.random()
    },
    paramCount: 0,
  },
  '!:random-int': {
    evaluate: ([min, max], sourceCodeInfo): number => {
      assertNumber(min, sourceCodeInfo, { integer: true })
      assertNumber(max, sourceCodeInfo, { integer: true, gt: min })
      return Math.floor(Math.random() * (max - min)) + min
    },
    paramCount: 2,
  },
  '!:random-int-inclusive': {
    evaluate: ([min, max], sourceCodeInfo): number => {
      assertNumber(min, sourceCodeInfo, { integer: true })
      assertNumber(max, sourceCodeInfo, { integer: true, gte: min })
      return Math.floor(Math.random() * (max - min + 1)) + min
    },
    paramCount: 2,
  },
  '!:random-float': {
    evaluate: ([min, max], sourceCodeInfo): number => {
      assertNumber(min, sourceCodeInfo)
      assertNumber(max, sourceCodeInfo, { gt: min })
      return Math.random() * (max - min) + min
    },
    paramCount: 2,
  },
  '!:random-boolean': {
    evaluate: ([prob], sourceCodeInfo): boolean => {
      const probability = prob ?? 0.5
      assertNumber(probability, sourceCodeInfo, { gte: 0, lte: 1 })
      return Math.random() < probability
    },
    paramCount: { min: 0, max: 1 },
  },
  '!:random-item': {
    evaluate: ([array], sourceCodeInfo): Any => {
      assertArray(array, sourceCodeInfo)
      const index = Math.floor(Math.random() * array.length)
      return asAny(array[index])
    },
    paramCount: 1,
  },
  '!:random-sample': {
    evaluate: ([array, n], sourceCodeInfo): Arr => {
      assertArray(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      if (array.length === 0) {
        throw new LitsError('Cannot sample from an empty array.', sourceCodeInfo)
      }

      const result: Arr = []

      for (let i = 0; i < n; i++) {
        // Pick a random index from the array
        const randomIndex = Math.floor(Math.random() * array.length)
        // Add the randomly selected item to the result
        result.push(array[randomIndex])
      }

      return result
    },
    paramCount: 2,
  },
  '!:random-sample-unique': {
    evaluate: ([array, n], sourceCodeInfo): Arr => {
      assertArray(array, sourceCodeInfo)
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true, lte: array.length })
      if (array.length === 0) {
        throw new LitsError('Cannot sample from an empty array.', sourceCodeInfo)
      }
      const result: Arr = []

      const copyArray = [...array]
      for (let i = 0; i < n; i++) {
        // Pick a random index from the array
        const randomIndex = Math.floor(Math.random() * copyArray.length)
        // Add the randomly selected item to the result
        result.push(copyArray[randomIndex])
        // Remove the used item from the copy array
        copyArray.splice(randomIndex, 1)
      }

      return result
    },
    paramCount: 2,
  },
  '!:shuffle': {
    evaluate: ([array], sourceCodeInfo): Arr => {
      assertArray(array, sourceCodeInfo)
      const shuffledArray = [...array]
      for (let i = shuffledArray.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1))
        ;[shuffledArray[i], shuffledArray[j]] = [shuffledArray[j], shuffledArray[i]]
      }
      return shuffledArray
    },
    paramCount: 1,
  },
  '!:random-normal': {
    evaluate: ([mean, stdDev], sourceCodeInfo): number => {
      assertNumber(mean, sourceCodeInfo)
      assertNumber(stdDev, sourceCodeInfo, { gt: 0 })
      const u1 = Math.random()
      const u2 = Math.random()
      const z0 = Math.sqrt(-2.0 * Math.log(u1)) * Math.cos(2.0 * Math.PI * u2)
      return z0 * stdDev + mean
    },
    paramCount: 2,
  },
  '!:random-exponential': {
    evaluate: ([lambda], sourceCodeInfo): number => {
      assertNumber(lambda, sourceCodeInfo, { gt: 0 })
      const u = Math.random()
      return -Math.log(u) / lambda
    },
    paramCount: 1,
  },
  '!:random-binomial': {
    evaluate: ([n, p], sourceCodeInfo): number => {
      assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true })
      assertNumber(p, sourceCodeInfo, { gte: 0, lte: 1 })
      let k = 0
      for (let i = 0; i < n; i++) {
        if (Math.random() < p) {
          k++
        }
      }
      return k
    },
    paramCount: 2,
  },
  '!:random-poisson': {
    evaluate: ([lambda], sourceCodeInfo): number => {
      assertNumber(lambda, sourceCodeInfo, { gt: 0 })

      const L = Math.exp(-lambda)
      let k = 0
      let p = 1

      do {
        k++
        p *= Math.random()
      } while (p > L)

      return k - 1
    },
    paramCount: 1,
  },
  '!:random-gamma': {
    evaluate: ([shape, scale], sourceCodeInfo): number => {
      assertNumber(shape, sourceCodeInfo, { gt: 0 })
      assertNumber(scale, sourceCodeInfo, { gt: 0 })
      return randomGamma(shape, scale)
    },
    paramCount: 2,
  },
  '!:random-pareto': {
    evaluate: ([alpha], sourceCodeInfo): number => {
      assertNumber(alpha, sourceCodeInfo, { gt: 0 })
      const u = Math.random()
      return (1 / u) ** (1 / alpha)
    },
    paramCount: 1,
  },
  '!:uuid': {
    evaluate: (): string => {
      return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (char) => {
        const random = Math.random() * 16 | 0
        const value = char === 'x' ? random : (random & 0x3 | 0x8)
        return value.toString(16)
      })
    },
    paramCount: 0,
  },
  '!:random-char': {
    evaluate: ([charSet], sourceCodeInfo): string => {
      assertString(charSet, sourceCodeInfo)
      if (charSet.length === 0) {
        throw new Error('Character set cannot be empty.')
      }
      const randomIndex = Math.floor(Math.random() * charSet.length)
      return charSet[randomIndex]!
    },
    paramCount: 1,
  },
  '!:random-string': {
    evaluate: ([length, charSet], sourceCodeInfo): string => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      assertString(charSet, sourceCodeInfo)
      if (charSet.length === 0) {
        throw new Error('Character set cannot be empty.')
      }
      let result = ''
      for (let i = 0; i < length; i++) {
        const randomIndex = Math.floor(Math.random() * charSet.length)
        result += charSet[randomIndex]!
      }
      return result
    },
    paramCount: 2,
  },
  '!:random-id': {
    evaluate: ([length], sourceCodeInfo): string => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
      let result = ''
      for (let i = 0; i < length; i++) {
        const randomIndex = Math.floor(Math.random() * chars.length)
        result += chars[randomIndex]!
      }
      return result
    },
    paramCount: 1,
  },
  '!:random-color': {
    evaluate: (): string => {
      const randomColor = Math.floor(Math.random() * 0x1000000).toString(16)
      return `#${randomColor.padStart(6, '0')}`
    },
    paramCount: 0,
  },
}

/**
 * Generates a random number from a gamma distribution
 * @param shape The shape parameter (alpha) - must be positive
 * @param scale The scale parameter (beta) - must be positive
 * @returns A random number following the gamma distribution
 */
function randomGamma(shape: number, scale: number = 1): number {
  // Input validation
  if (shape <= 0 || scale <= 0) {
    throw new Error('Shape and scale parameters must be positive')
  }

  // Special case for shape < 1
  if (shape < 1) {
    const d = shape + 1.0 - 1.0 / 3.0
    return randomGamma(d, scale) * Math.random() ** (1.0 / shape)
  }

  // Marsaglia and Tsang method for shape >= 1
  const d = shape - 1.0 / 3.0
  const c = 1.0 / Math.sqrt(9.0 * d)

  let x: number, v: number, u: number

  while (true) {
    do {
      x = randn() // Standard normal random variable
      v = 1.0 + c * x
    } while (v <= 0)

    v = v * v * v
    u = Math.random()

    if (u < 1.0 - 0.0331 * x * x * x * x) {
      return scale * d * v
    }

    if (Math.log(u) < 0.5 * x * x + d * (1.0 - v + Math.log(v))) {
      return scale * d * v
    }
  }
}

/**
 * Helper function to generate standard normal random variables
 * using Box-Muller transform
 */
function randn(): number {
  let u = 0
  let v = 0
  while (u === 0) u = Math.random()
  while (v === 0) v = Math.random()

  return Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v)
}
