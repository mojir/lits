import { LitsError } from '../../../errors'
import type { SourceCodeInfo } from '../../../tokenizer/token'
import { isMatrix, isVector } from '../../../typeGuards/annotatedArrays'
import { assertNumber, isNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../interface'

type NumberVectorOrMatrix = number | number[] | number[][]

function getNumberVectorOrMatrixOperation(
  params: unknown[],
  sourceCodeInfo: SourceCodeInfo | undefined,
):
  | ['number', number[]]
  | ['vector', number[][]]
  | ['matrix', number[][][]] {
  let hasVector: boolean = false
  let hasMatrix: boolean = false
  for (const param of params) {
    if (isVector(param)) {
      hasVector = true
    }
    else if (isMatrix(param)) {
      hasMatrix = true
    }
    else if (!isNumber(param)) {
      throw new LitsError(`Invalid parameter type: ${typeof param}`, sourceCodeInfo)
    }
  }
  if (hasMatrix) {
    if (hasVector) {
      throw new LitsError('Cannot mix vector and matrix types', sourceCodeInfo)
    }
    let rows: number | null = null
    let cold: number | null = null
    for (const param of params) {
      if (isMatrix(param)) {
        if (rows === null) {
          rows = param.length
          cold = param[0]!.length
        }
        else {
          if (param.length !== rows || param[0]!.length !== cold) {
            throw new LitsError('Matrix dimensions do not match', sourceCodeInfo)
          }
        }
      }
    }
    const matrices = params.map((param) => {
      if (isMatrix(param)) {
        return param
      }
      return Array.from({ length: rows as number }, () => Array.from({ length: cold as number }, () => param as number))
    })
    return ['matrix', matrices]
  }
  if (hasVector) {
    let length: number | null = null
    for (const param of params) {
      if (isVector(param)) {
        if (length === null) {
          length = param.length
        }
        else {
          if (param.length !== length) {
            throw new LitsError('Vector lengths do not match', sourceCodeInfo)
          }
        }
      }
    }
    const vectors = params.map((param) => {
      if (isVector(param)) {
        return param
      }
      return Array.from({ length: length as number }, () => param as number)
    })

    return ['vector', vectors]
  }
  return ['number', params as number[]]
}

export const mathNormalExpression: BuiltinNormalExpressions = {
  'inc': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return operands[0]! + 1
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        return firstVector.map(val => val + 1)
      }
      else {
        const firstMatrix = operands[0]!
        return firstMatrix.map(row => row.map(val => val + 1))
      }
    },
    arity: toFixedArity(1),
  },
  'dec': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return operands[0]! - 1
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        return firstVector.map(val => val - 1)
      }
      else {
        const firstMatrix = operands[0]!
        return firstMatrix.map(row => row.map(val => val - 1))
      }
    },
    arity: toFixedArity(1),
  },
  '+': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      if (params.length === 0) {
        return 0
      }

      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        return operands.reduce((result, param) => result + (param), 0)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const restVectors = operands.slice(1)
        return restVectors.reduce((acc, vector) => acc.map((val, i) => val + vector[i]!), firstVector)
      }
      else {
        const firstMatrix = operands[0]!
        const restMatrices = operands.slice(1)
        return restMatrices.reduce((acc, matrix) => acc.map((row, i) => row.map((val, j) => val + matrix[i]![j]!)), firstMatrix)
      }
    },
    arity: {},
  },
  '*': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      if (params.length === 0) {
        return 1
      }

      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        return operands.reduce((result, param) => result * (param), 1)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const restVectors = operands.slice(1)
        return restVectors.reduce((acc, vector) => acc.map((val, i) => val * vector[i]!), firstVector)
      }
      else {
        const firstMatrix = operands[0]!
        const restMatrices = operands.slice(1)
        return restMatrices.reduce((acc, matrix) => acc.map((row, i) => row.map((val, j) => val * matrix[i]![j]!)), firstMatrix)
      }
    },
    aliases: ['·'],
    arity: {},
  },
  '/': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      if (params.length === 0) {
        return 1
      }

      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        const [first, ...rest] = operands
        if (rest.length === 0) {
          return 1 / first!
        }
        return rest.reduce((result, param) => {
          return result / param
        }, first!)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const restVectors = operands.slice(1)
        return restVectors.reduce((acc, vector) => acc.map((val, i) => val / vector[i]!), firstVector)
      }
      else {
        const firstMatrix = operands[0]!
        const restMatrices = operands.slice(1)
        return restMatrices.reduce((acc, matrix) => acc.map((row, i) => row.map((val, j) => val / matrix[i]![j]!)), firstMatrix)
      }
    },
    arity: {},
  },
  '-': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      if (params.length === 0) {
        return 0
      }

      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        const [first, ...rest] = operands
        if (rest.length === 0)
          return -first!

        return rest.reduce((result, param) => {
          return result - param
        }, first!)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const restVectors = operands.slice(1)
        return restVectors.reduce((acc, vector) => acc.map((val, i) => val - vector[i]!), firstVector)
      }
      else {
        const firstMatrix = operands[0]!
        const restMatrices = operands.slice(1)
        return restMatrices.reduce((acc, matrix) => acc.map((row, i) => row.map((val, j) => val - matrix[i]![j]!)), firstMatrix)
      }
    },
    arity: {},
  },
  'quot': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        return Math.trunc(operands[0]! / operands[1]!)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const secondVector = operands[1]!
        return firstVector.map((val, i) => Math.trunc(val / secondVector[i]!))
      }
      else {
        const firstMatrix = operands[0]!
        const secondMatrix = operands[1]!
        return firstMatrix.map((row, i) => row.map((val, j) => Math.trunc(val / secondMatrix[i]![j]!)))
      }
    },
    arity: toFixedArity(2),
  },
  'mod': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        const quotient = Math.floor(operands[0]! / operands[1]!)
        return operands[0]! - operands[1]! * quotient
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const secondVector = operands[1]!
        return firstVector.map((dividend, i) => {
          const divisor = secondVector[i]!
          const quotient = Math.floor(dividend / divisor)
          return dividend - divisor * quotient
        })
      }
      else {
        const firstMatrix = operands[0]!
        const secondMatrix = operands[1]!
        return firstMatrix.map((row, i) => row.map((val, j) => {
          const quotient = Math.floor(val / secondMatrix[i]![j]!)
          return val - secondMatrix[i]![j]! * quotient
        }))
      }
    },
    arity: toFixedArity(2),
  },
  '%': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)

      if (operation === 'number') {
        return operands[0]! % operands[1]!
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const secondVector = operands[1]!
        return firstVector.map((dividend, i) => dividend % secondVector[i]!)
      }
      else {
        const firstMatrix = operands[0]!
        const secondMatrix = operands[1]!
        return firstMatrix.map((row, i) => row.map((dividend, j) => dividend % secondMatrix[i]![j]!))
      }
    },
    arity: toFixedArity(2),
    aliases: ['rem'],
  },
  'sqrt': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.sqrt(operands[0]!)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        return firstVector.map(val => Math.sqrt(val))
      }
      else {
        const firstMatrix = operands[0]!
        return firstMatrix.map(row => row.map(val => Math.sqrt(val)))
      }
    },
    arity: toFixedArity(1),
    aliases: ['√'],
  },
  'cbrt': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.cbrt(operands[0]!)
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        return firstVector.map(val => Math.cbrt(val))
      }
      else {
        const firstMatrix = operands[0]!
        return firstMatrix.map(row => row.map(val => Math.cbrt(val)))
      }
    },
    arity: toFixedArity(1),
    aliases: ['∛'],
  },
  '^': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return operands[0]! ** operands[1]!
      }
      else if (operation === 'vector') {
        const firstVector = operands[0]!
        const secondVector = operands[1]!
        return firstVector.map((base, i) => base ** secondVector[i]!)
      }
      else {
        const firstMatrix = operands[0]!
        const secondMatrix = operands[1]!
        return firstMatrix.map((row, i) => row.map((base, j) => base ** secondMatrix[i]![j]!))
      }
    },
    arity: toFixedArity(2),
  },
  'round': {
    evaluate: ([value, decimals], sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation([value], sourceCodeInfo)
      if (operation === 'number') {
        if (decimals === undefined || decimals === 0) {
          return Math.round(operands[0]!)
        }
        else {
          assertNumber(decimals, sourceCodeInfo, { integer: true, positive: true })
          const factor = 10 ** decimals
          return Math.round(operands[0]! * factor) / factor
        }
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        if (decimals === undefined || decimals === 0) {
          return vector.map(val => Math.round(val))
        }
        else {
          assertNumber(decimals, sourceCodeInfo, { integer: true, positive: true })
          const factor = 10 ** decimals
          return vector.map(val => Math.round(val * factor) / factor)
        }
      }
      else {
        const matrix = operands[0]!
        if (decimals === undefined || decimals === 0) {
          return matrix.map(row => row.map(val => Math.round(val)))
        }
        else {
          assertNumber(decimals, sourceCodeInfo, { integer: true, positive: true })
          const factor = 10 ** decimals
          return matrix.map(row => row.map(val => Math.round(val * factor) / factor))
        }
      }
    },
    arity: { min: 1, max: 2 },
  },
  'trunc': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.trunc(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.trunc(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.trunc(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'floor': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.floor(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.floor(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.floor(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'ceil': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.ceil(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.ceil(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.ceil(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'min': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      if (rest.length === 0)
        return first

      return rest.reduce((min: number, value) => {
        assertNumber(value, sourceCodeInfo)
        return Math.min(min, value)
      }, first)
    },
    arity: { min: 1 },
  },
  'max': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo)
      if (rest.length === 0)
        return first

      return rest.reduce((min: number, value) => {
        assertNumber(value, sourceCodeInfo)
        return Math.max(min, value)
      }, first)
    },
    arity: { min: 1 },
  },
  'abs': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.abs(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.abs(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.abs(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'sign': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.sign(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.sign(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.sign(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'ln': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.log(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.log(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.log(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'log2': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.log2(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.log2(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.log2(val)))
      }
    },
    arity: toFixedArity(1),
    aliases: ['log₂'],
  },
  'log10': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.log10(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.log10(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.log10(val)))
      }
    },
    arity: toFixedArity(1),
    aliases: ['log₁₀'],
  },
  'sin': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.sin(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.sin(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.sin(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'asin': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.asin(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.asin(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.asin(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'sinh': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.sinh(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.sinh(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.sinh(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'asinh': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.asinh(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.asinh(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.asinh(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'cos': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.cos(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.cos(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.cos(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'acos': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.acos(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.acos(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.acos(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'cosh': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.cosh(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.cosh(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.cosh(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'acosh': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.acosh(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.acosh(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.acosh(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'tan': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.tan(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.tan(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.tan(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'atan': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.atan(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.atan(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.atan(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'tanh': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.tanh(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.tanh(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.tanh(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'atanh': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return Math.atanh(operands[0]!)
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => Math.atanh(val))
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => Math.atanh(val)))
      }
    },
    arity: toFixedArity(1),
  },
  'to-rad': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return (operands[0]! * Math.PI) / 180
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => (val * Math.PI) / 180)
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => (val * Math.PI) / 180))
      }
    },
    arity: toFixedArity(1),
  },
  'to-deg': {
    evaluate: (params, sourceCodeInfo): NumberVectorOrMatrix => {
      const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
      if (operation === 'number') {
        return (operands[0]! * 180) / Math.PI
      }
      else if (operation === 'vector') {
        const vector = operands[0]!
        return vector.map(val => (val * 180) / Math.PI)
      }
      else {
        const matrix = operands[0]!
        return matrix.map(row => row.map(val => (val * 180) / Math.PI))
      }
    },
    arity: toFixedArity(1),
  },
}
