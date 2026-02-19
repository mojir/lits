import { LitsError } from '../../errors'
import type { SourceCodeInfo } from '../../tokenizer/token'
import { isMatrix, isVector } from '../../typeGuards/annotatedArrays'
import { assertNumber, isNumber } from '../../typeGuards/number'
import { toFixedArity } from '../../utils/arity'
import type { BuiltinNormalExpressions } from '../interface'

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
    docs: {
      category: 'Math',
      returns: { type: 'number' },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `inc` function increments its argument by 1, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it increases each element by 1 while preserving the original structure.',
      seeAlso: ['dec', '+'],
      examples: [
        'inc(0)',
        'inc(1)',
        'inc(100.1)',
        'inc([1, 2, 3])',
        'inc([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `dec` function decrements its argument by 1, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it decreases each element by 1 while preserving the original structure.',
      seeAlso: ['inc', '-'],
      examples: [
        'dec(0)',
        'dec(1)',
        'dec(100.1)',
        'dec([1, 2, 3])',
        'dec([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
        xs: { type: ['number', 'vector', 'matrix'], rest: true },
      },
      variants: [{ argumentNames: ['xs'] }],
      description: 'The `+` function performs addition of numbers and element-wise addition of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it adds the scalar to each element of the collection.',
      seeAlso: ['-', '*', '/', 'inc'],
      examples: [
        '1 + 2',
        '1 + 20 + 30',
        '+(1, 2, 3, 4)',
        '+()',
        '+(1)',
        '[1, 2, 3] + 2',
        '[1, 2, 3] + [4, 5, 6]',
        '[[1, 2, 3], [4, 5, 6]] + [[7, 8, 9], [10, 11, 12]]',
        '[[1, 2, 3], [4, 5, 6]] + 2',
      ],
    },
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
    arity: {},
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
        xs: { type: ['number', 'vector', 'matrix'], rest: true },
      },
      variants: [{ argumentNames: ['xs'] }],
      description: 'The `*` function performs multiplication of `numbers` and element-wise multiplication of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it multiplies each element of the collection by the scalar.',
      seeAlso: ['/', '+', '-', '^'],
      examples: [
        '6 * 7',
        '-1 * 4',
        '*(4, 7)',
        '*(1, 2, 3, 4, 5)',
        '*()',
        '*(8)',
        '[1, 2, 3] * 2',
        '[1, 2, 3] * [4, 5, 6]',
        '[[1, 2, 3], [4, 5, 6]] * [[7, 8, 9], [10, 11, 12]]',
        '[[1, 2, 3], [4, 5, 6]] * 2',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
        xs: { type: ['number', 'vector', 'matrix'], rest: true },
      },
      variants: [{ argumentNames: ['xs'] }],
      description: 'The `/` function performs division of `numbers` and element-wise division of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it divides each element of the collection by the scalar.',
      seeAlso: ['*', '+', '-', 'quot', 'mod', '%'],
      examples: [
        '12 / 100',
        '-1 / 4',
        '/(7, 4)',
        '/(1, 2, 4, 8)',
        '/()',
        '/(8)',
        '[1, 2, 3] / 2',
        '[1, 2, 3] / [4, 5, 6]',
        '[[1, 2, 3], [4, 5, 6]] / [[7, 8, 9], [10, 11, 12]]',
        '[[1, 2, 3], [4, 5, 6]] / 2',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
        xs: { type: ['number', 'vector', 'matrix'], rest: true },
      },
      variants: [{ argumentNames: ['xs'] }],
      description: 'Computes difference between first value and sum of the rest. When called with only one argument, it does negation.',
      seeAlso: ['+', '*', '/', 'dec', 'abs'],
      examples: [
        '50 - 8',
        '1 - 1 - 1',
        '-()',
        '-(4, 2)',
        '-(4, 3, 2, 1,)',
        '[1, 2, 3] - 2',
        '[1, 2, 3] - [4, 5, 6]',
        '[[1, 2, 3], [4, 5, 6]] - [[7, 8, 9], [10, 11, 12]]',
        '[[1, 2, 3], [4, 5, 6]] - 2',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'The `quot` function performs integer division truncated toward zero, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies integer division between each element of the collection and the scalar.',
      seeAlso: ['mod', '%', '/', 'trunc'],
      examples: [
        'quot(5, 3)',
        'quot(5.2, 3.1)',
        'quot(-5, 3)',
        '5 quot -3',
        '-5 quot -3',
        'quot(5, 0)',
        'quot(0, 5)',
        '[1, 2, 3] quot 2',
        '2 quot [1, 2, 3]',
        'quot([1, 2, 3], [4, 5, 6])',
        '[[1, 2, 3], [4, 5, 6]] quot [[7, 8, 9], [10, 11, 12]]',
        'quot([[1, 2, 3], [4, 5, 6]], 2)',
        '[[1, 2, 3], [4, 5, 6]] quot [[7, 8, 9], [10, 11, 12]]',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'The `mod` function computes the modulo of division with the same sign as the divisor, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the modulo operation between each element of the collection and the scalar.',
      seeAlso: ['%', 'quot', '/'],
      examples: [
        'mod(5, 3)',
        'mod(5.2, 3.1)',
        'mod(-5, 3)',
        '5 mod -3',
        '-5 mod -3',
        '[1, 2, 3] mod 2',
        '2 mod [1, 2, 3]',
        'mod([1, 2, 3], [4, 5, 6])',
        '[[1, 2, 3], [4, 5, 6]] mod [[7, 8, 9], [10, 11, 12]]',
        'mod([[1, 2, 3], [4, 5, 6]], 2)',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'The `%` function computes the remainder of division with the same sign as the dividend, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the remainder operation between each element of the collection and the scalar.',
      seeAlso: ['mod', 'quot', '/'],
      examples: [
        '5 % 3',
        '5.2 % 3.1',
        '-5 % 3',
        '%(5, -3)',
        '%(-5, -3)',
        '[1, 2, 3] % 2',
        '2 % [1, 2, 3]',
        '%([1, 2, 3], [4, 5, 6])',
        '[[1, 2, 3], [4, 5, 6]] % [[7, 8, 9], [10, 11, 12]]',
        '%([[1, 2, 3], [4, 5, 6]], 2)',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sqrt` function calculates the square root of `numbers` and computes element-wise square roots of `vectors` and `matrices`. When applied to collections, it returns the square root of each element while preserving the original structure.',
      seeAlso: ['cbrt', '^'],
      examples: [
        'sqrt(0)',
        'sqrt(9)',
        'sqrt(2)',
        'sqrt(0)',
        'sqrt(9)',
        'sqrt(2)',
        'sqrt([1, 4, 9])',
        'sqrt([[1, 4], [9, 16]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cbrt` function calculates the cube root of `numbers` and computes element-wise cube roots of `vectors` and `matrices`. When applied to collections, it returns the cube root of each element while preserving the original structure.',
      seeAlso: ['sqrt', '^'],
      examples: [
        'cbrt(0)',
        'cbrt(27)',
        'cbrt(2)',
        'cbrt(1)',
        'cbrt(0)',
        'cbrt(27)',
        'cbrt(2)',
        'cbrt(1)',
        'cbrt([1, 8, 27])',
        'cbrt([[1, 8], [27, 64]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['a', 'b'] }],
      description: 'The ^ function computes exponentiation, raising the first argument to the power of the second, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the power operation between each element of the collection and the scalar.',
      seeAlso: ['sqrt', 'cbrt', '*', 'ln'],
      examples: [
        '2 ^ 3',
        '2 ^ 0',
        '2 ^ -3',
        '^(-2, 3)',
        '^(-2, -3)',
        '[1, 2, 3] ^ 2',
        '2 ^ [1, 2, 3]',
        '^([1, 2, 3], [4, 5, 6])',
        '[[1, 2, 3], [4, 5, 6]] ^ [[7, 8, 9], [10, 11, 12]]',
        '^([[1, 2, 3], [4, 5, 6]], 2)',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        a: { type: ['number', 'vector', 'matrix'] },
        b: { type: 'integer' },
      },
      variants: [
        { argumentNames: ['a'] },
        { argumentNames: ['a', 'b'] },
      ],
      description: 'The `round` function rounds a `number` to the nearest `integer` or to a specified number of `decimal` places, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it rounds each element while preserving the original structure.',
      seeAlso: ['floor', 'ceil', 'trunc'],
      examples: [
        'round(2)',
        'round(2.49)',
        'round(2.5)',
        'round(-2.49)',
        'round(-2.5)',
        'round(-2.501)',
        'round(1.23456789, 4)',
        '1.123456789 round 2',
        'round([1.23456789, 2.3456789], 1)',
        '[1.23456789, 2.3456789] round 4',
        '[[1.23456789, 2.3456789], [3.456789, 4.56789]] round 4',
        'round([[1.23456789, 2.3456789], [3.456789, 4.56789]], 2)',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['integer', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `trunc` function truncates `numbers` toward zero (removing decimal portions without rounding), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it truncates each element while preserving the original structure.',
      seeAlso: ['round', 'floor', 'ceil', 'quot'],
      examples: [
        'trunc(2)',
        'trunc(2.49)',
        'trunc(2.5)',
        'trunc(-2.49)',
        'trunc(-2.5)',
        'trunc(-2.501)',
        'trunc([1.23456789, 2.3456789])',
        'trunc([[1.23456789, 2.3456789], [3.456789, 4.56789]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['integer', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `floor` function returns the largest `integer` less than or equal to a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the floor of each element while preserving the original structure.',
      seeAlso: ['ceil', 'round', 'trunc'],
      examples: [
        'floor(2)',
        'floor(2.49)',
        'floor(2.5)',
        'floor(-2.49)',
        'floor(-2.5)',
        'floor(-2.501)',
        'floor([1.23456789, 2.3456789])',
        'floor([[1.23456789, 2.3456789], [3.456789, 4.56789]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['integer', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `ceil` function returns the smallest `integer` greater than or equal to a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the ceiling of each element while preserving the original structure.',
      seeAlso: ['floor', 'round', 'trunc'],
      examples: [
        'ceil(2)',
        'ceil(2.49)',
        'ceil(2.5)',
        'ceil(-2.49)',
        'ceil(-2.5)',
        'ceil(-2.501)',
        'ceil([1.23456789, 2.3456789])',
        'ceil([[1.23456789, 2.3456789], [3.456789, 4.56789]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: 'number' },
      args: {
        a: { type: 'number' },
        b: { type: 'number' },
        xs: { type: 'number', rest: true },
      },
      variants: [{ argumentNames: ['xs'] }],
      description: 'Returns the smallest number of the arguments.',
      seeAlso: ['max', 'Vector.min'],
      examples: [
        '2 min 3',
        'min(2, 0, 1)',
        'min(2, -1, 1)',
        'min(2.5)',
        '12 min 14',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: 'number' },
      args: {
        a: { type: 'number' },
        b: { type: 'number' },
        xs: { type: 'number', rest: true },
      },
      variants: [{ argumentNames: ['xs'] }],
      description: 'Returns the largest number of the arguments.',
      seeAlso: ['min', 'Vector.max'],
      examples: [
        ' 2 max 3',
        'max(2, 0, 1)',
        'max(2, -1, 1)',
        'max(2, 0.5)',
        '4 max 2',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The abs function returns the absolute value (magnitude) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the absolute value of each element while preserving the original structure.',
      seeAlso: ['sign', '-'],
      examples: [
        'abs(-2.3)',
        'abs(0)',
        'abs(2.5)',
        'abs([1, -2, 3])',
        'abs([[1, -2], [3, -4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sign` function returns the `sign` of a `number` (-1 for negative, 0 for zero, 1 for positive), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sign of each element while preserving the original structure.',
      seeAlso: ['abs'],
      examples: [
        'sign(-2.3)',
        'sign(-0)',
        'sign(0)',
        'sign(12312)',
        'sign([1, -2, 3])',
        'sign([[1, -2], [3, -4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `ln` function computes the natural logarithm (base `e`) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the natural logarithm of each element while preserving the original structure.',
      seeAlso: ['log2', 'log10', '^'],
      examples: [
        'ln(0.01)',
        'ln(2.5)',
        'ln(E)',
        'ln([1, 2, 3])',
        'ln([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `log2` function computes the base `2` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-2 logarithm of each element while preserving the original structure.',
      seeAlso: ['ln', 'log10'],
      examples: [
        'log2(0.01)',
        'log2(2 ^ 12)',
        'log2(2.5)',
        'log2([1, 2, 3])',
        'log2([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `log10` function computes the base `10` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-10 logarithm of each element while preserving the original structure.',
      seeAlso: ['ln', 'log2'],
      examples: [
        'log10(0.01)',
        'log10(10 ^ 12)',
        'log10(2.5)',
        'log10([1, 2, 3])',
        'log10([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sin` function computes the sine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sine of each element while preserving the original structure.',
      seeAlso: ['asin', 'sinh', 'cos', 'tan', 'to-rad'],
      examples: [
        'sin(0)',
        'sin(1)',
        'sin(PI)',
        'sin(-0.5)',
        'sin([1, 2, 3])',
        'sin([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `asin` function computes the arcsine (inverse sine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arcsine of each element while preserving the original structure.',
      seeAlso: ['sin', 'asinh', 'acos', 'atan'],
      examples: [
        'asin(0)',
        'asin(1)',
        'asin(-0.5)',
        'asin([1, 2, 3])',
        'asin([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sinh` function computes the hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic sine of each element while preserving the original structure.',
      seeAlso: ['asinh', 'sin', 'cosh', 'tanh'],
      examples: [
        'sinh(0)',
        'sinh(1)',
        'sinh(-0.5)',
        'sinh([0.1, 0.2, 0.3])',
        'sinh([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `asinh` function computes the inverse hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic sine of each element while preserving the original structure.',
      seeAlso: ['sinh', 'asin', 'acosh', 'atanh'],
      examples: [
        'asinh(10)',
        'asinh(90)',
        'asinh (50)',
        'asinh([10, 20, 30])',
        'asinh([[10, 20], [30, 40]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cos` function computes the cosine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the cosine of each element while preserving the original structure.',
      seeAlso: ['acos', 'cosh', 'sin', 'tan', 'to-rad'],
      examples: [
        'cos(0)',
        'cos(1)',
        'cos(PI)',
        'cos(-0.5)',
        'cos([1, 2, 3])',
        'cos([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `acos` function computes the arccosine (inverse cosine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arccosine of each element while preserving the original structure.',
      seeAlso: ['cos', 'acosh', 'asin', 'atan'],
      examples: [
        'acos(0)',
        'acos(1)',
        'acos(-0.5)',
        'acos([0.1, 0.2, 0.3])',
        'acos([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cosh` function computes the hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic cosine of each element while preserving the original structure.',
      seeAlso: ['acosh', 'cos', 'sinh', 'tanh'],
      examples: [
        'cosh(0)',
        'cosh(1)',
        'cosh(-0.5)',
        'cosh([0.1, 0.2, 0.3])',
        'cosh([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `acosh` function computes the inverse hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic cosine of each element while preserving the original structure.',
      seeAlso: ['cosh', 'acos', 'asinh', 'atanh'],
      examples: [
        'acosh(1)',
        'acosh(2)',
        'acosh(100)',
        'acosh(50)',
        'acosh([1, 2, 3])',
        'acosh([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `tan` function computes the tangent of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the tangent of each element while preserving the original structure.',
      seeAlso: ['atan', 'tanh', 'sin', 'cos', 'to-rad'],
      examples: [
        'tan(0)',
        'tan(1)',
        'tan(PI)',
        'tan(-0.5)',
        'tan([1, 2, 3])',
        'tan([[1, 2], [3, 4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `atan` function computes the arctangent (inverse tangent) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arctangent of each element while preserving the original structure.',
      seeAlso: ['tan', 'atanh', 'asin', 'acos'],
      examples: [
        'atan(0)',
        'atan(1)',
        'atan(-0.5)',
        'atan([0.1, 0.2, 0.3])',
        'atan([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `tanh` function computes the hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic tangent of each element while preserving the original structure.',
      seeAlso: ['atanh', 'tan', 'sinh', 'cosh'],
      examples: ['tanh(0)', 'tanh(1)', 'tanh(-0.5)', 'tanh(50)'],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `atanh` function computes the inverse hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic tangent of each element while preserving the original structure.',
      seeAlso: ['tanh', 'atan', 'asinh', 'acosh'],
      examples: [
        'atanh(0)',
        'atanh(0.9)',
        'atanh(-0.5)',
        'atanh([0.1, 0.2, 0.3])',
        'atanh([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `to-rad` function converts an angle from degrees to radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
      seeAlso: ['to-deg', 'sin', 'cos', 'tan'],
      examples: [
        'to-rad(0)',
        'to-rad(90)',
        'to-rad(180)',
        'to-rad(360)',
        'to-rad([0, 90, 180])',
        'to-rad([[0, 90], [180, 360]])',
      ],
    },
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
    docs: {
      category: 'Math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `to-deg` function converts an angle from radians to degrees, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
      seeAlso: ['to-rad'],
      examples: [
        'to-deg(0)',
        'to-deg(PI)',
        'to-deg(PI / 2)',
        'to-deg(3 * PI / 2)',
        'to-deg([0, PI, PI / 2])',
        'to-deg([[0, PI], [PI / 2, 3 * PI / 2]])',
      ],
    },
  },
}
