import type { SourceCodeInfo } from '../../../tokenizer/token'
import { isMatrix, isVector } from '../../../typeGuards/annotatedArrays'
import { isNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import { LitsError } from '../../../errors'
import type { BuiltinNormalExpressions } from '../../interface'
import type { LitsModule } from '../interface'

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

function unaryMathOp(
  fn: (val: number) => number,
): (params: unknown[], sourceCodeInfo: SourceCodeInfo | undefined) => NumberVectorOrMatrix {
  return (params, sourceCodeInfo) => {
    const [operation, operands] = getNumberVectorOrMatrixOperation(params, sourceCodeInfo)
    if (operation === 'number') {
      return fn(operands[0]!)
    }
    else if (operation === 'vector') {
      return operands[0]!.map(val => fn(val))
    }
    else {
      return operands[0]!.map(row => row.map(val => fn(val)))
    }
  }
}

const mathUtilsFunctions: BuiltinNormalExpressions = {
  'sin': {
    evaluate: unaryMathOp(val => Math.sin(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sin` function computes the sine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.asin', 'Math-Utils.sinh', 'Math-Utils.cos', 'Math-Utils.tan', 'Math-Utils.to-rad'],
      examples: [
        'let { sin } = import("Math-Utils"); sin(0)',
        'let { sin } = import("Math-Utils"); sin(1)',
        'let { sin } = import("Math-Utils"); sin(PI)',
        'let { sin } = import("Math-Utils"); sin(-0.5)',
        'let { sin } = import("Math-Utils"); sin([1, 2, 3])',
        'let { sin } = import("Math-Utils"); sin([[1, 2], [3, 4]])',
      ],
    },
  },
  'asin': {
    evaluate: unaryMathOp(val => Math.asin(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `asin` function computes the arcsine (inverse sine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arcsine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.sin', 'Math-Utils.asinh', 'Math-Utils.acos', 'Math-Utils.atan'],
      examples: [
        'let { asin } = import("Math-Utils"); asin(0)',
        'let { asin } = import("Math-Utils"); asin(1)',
        'let { asin } = import("Math-Utils"); asin(-0.5)',
        'let { asin } = import("Math-Utils"); asin([1, 2, 3])',
        'let { asin } = import("Math-Utils"); asin([[1, 2], [3, 4]])',
      ],
    },
  },
  'sinh': {
    evaluate: unaryMathOp(val => Math.sinh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sinh` function computes the hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic sine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.asinh', 'Math-Utils.sin', 'Math-Utils.cosh', 'Math-Utils.tanh'],
      examples: [
        'let { sinh } = import("Math-Utils"); sinh(0)',
        'let { sinh } = import("Math-Utils"); sinh(1)',
        'let { sinh } = import("Math-Utils"); sinh(-0.5)',
        'let { sinh } = import("Math-Utils"); sinh([0.1, 0.2, 0.3])',
        'let { sinh } = import("Math-Utils"); sinh([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
  },
  'asinh': {
    evaluate: unaryMathOp(val => Math.asinh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `asinh` function computes the inverse hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic sine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.sinh', 'Math-Utils.asin', 'Math-Utils.acosh', 'Math-Utils.atanh'],
      examples: [
        'let { asinh } = import("Math-Utils"); asinh(10)',
        'let { asinh } = import("Math-Utils"); asinh(90)',
        'let { asinh } = import("Math-Utils"); asinh(50)',
        'let { asinh } = import("Math-Utils"); asinh([10, 20, 30])',
        'let { asinh } = import("Math-Utils"); asinh([[10, 20], [30, 40]])',
      ],
    },
  },
  'cos': {
    evaluate: unaryMathOp(val => Math.cos(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cos` function computes the cosine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the cosine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.acos', 'Math-Utils.cosh', 'Math-Utils.sin', 'Math-Utils.tan', 'Math-Utils.to-rad'],
      examples: [
        'let { cos } = import("Math-Utils"); cos(0)',
        'let { cos } = import("Math-Utils"); cos(1)',
        'let { cos } = import("Math-Utils"); cos(PI)',
        'let { cos } = import("Math-Utils"); cos(-0.5)',
        'let { cos } = import("Math-Utils"); cos([1, 2, 3])',
        'let { cos } = import("Math-Utils"); cos([[1, 2], [3, 4]])',
      ],
    },
  },
  'acos': {
    evaluate: unaryMathOp(val => Math.acos(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `acos` function computes the arccosine (inverse cosine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arccosine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.cos', 'Math-Utils.acosh', 'Math-Utils.asin', 'Math-Utils.atan'],
      examples: [
        'let { acos } = import("Math-Utils"); acos(0)',
        'let { acos } = import("Math-Utils"); acos(1)',
        'let { acos } = import("Math-Utils"); acos(-0.5)',
        'let { acos } = import("Math-Utils"); acos([0.1, 0.2, 0.3])',
        'let { acos } = import("Math-Utils"); acos([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
  },
  'cosh': {
    evaluate: unaryMathOp(val => Math.cosh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cosh` function computes the hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic cosine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.acosh', 'Math-Utils.cos', 'Math-Utils.sinh', 'Math-Utils.tanh'],
      examples: [
        'let { cosh } = import("Math-Utils"); cosh(0)',
        'let { cosh } = import("Math-Utils"); cosh(1)',
        'let { cosh } = import("Math-Utils"); cosh(-0.5)',
        'let { cosh } = import("Math-Utils"); cosh([0.1, 0.2, 0.3])',
        'let { cosh } = import("Math-Utils"); cosh([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
  },
  'acosh': {
    evaluate: unaryMathOp(val => Math.acosh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `acosh` function computes the inverse hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic cosine of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.cosh', 'Math-Utils.acos', 'Math-Utils.asinh', 'Math-Utils.atanh'],
      examples: [
        'let { acosh } = import("Math-Utils"); acosh(1)',
        'let { acosh } = import("Math-Utils"); acosh(2)',
        'let { acosh } = import("Math-Utils"); acosh(100)',
        'let { acosh } = import("Math-Utils"); acosh(50)',
        'let { acosh } = import("Math-Utils"); acosh([1, 2, 3])',
        'let { acosh } = import("Math-Utils"); acosh([[1, 2], [3, 4]])',
      ],
    },
  },
  'tan': {
    evaluate: unaryMathOp(val => Math.tan(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `tan` function computes the tangent of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the tangent of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.atan', 'Math-Utils.tanh', 'Math-Utils.sin', 'Math-Utils.cos', 'Math-Utils.to-rad'],
      examples: [
        'let { tan } = import("Math-Utils"); tan(0)',
        'let { tan } = import("Math-Utils"); tan(1)',
        'let { tan } = import("Math-Utils"); tan(PI)',
        'let { tan } = import("Math-Utils"); tan(-0.5)',
        'let { tan } = import("Math-Utils"); tan([1, 2, 3])',
        'let { tan } = import("Math-Utils"); tan([[1, 2], [3, 4]])',
      ],
    },
  },
  'atan': {
    evaluate: unaryMathOp(val => Math.atan(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `atan` function computes the arctangent (inverse tangent) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arctangent of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.tan', 'Math-Utils.atanh', 'Math-Utils.asin', 'Math-Utils.acos'],
      examples: [
        'let { atan } = import("Math-Utils"); atan(0)',
        'let { atan } = import("Math-Utils"); atan(1)',
        'let { atan } = import("Math-Utils"); atan(-0.5)',
        'let { atan } = import("Math-Utils"); atan([0.1, 0.2, 0.3])',
        'let { atan } = import("Math-Utils"); atan([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
  },
  'tanh': {
    evaluate: unaryMathOp(val => Math.tanh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `tanh` function computes the hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic tangent of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.atanh', 'Math-Utils.tan', 'Math-Utils.sinh', 'Math-Utils.cosh'],
      examples: [
        'let { tanh } = import("Math-Utils"); tanh(0)',
        'let { tanh } = import("Math-Utils"); tanh(1)',
        'let { tanh } = import("Math-Utils"); tanh(-0.5)',
        'let { tanh } = import("Math-Utils"); tanh(50)',
      ],
    },
  },
  'atanh': {
    evaluate: unaryMathOp(val => Math.atanh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `atanh` function computes the inverse hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic tangent of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.tanh', 'Math-Utils.atan', 'Math-Utils.asinh', 'Math-Utils.acosh'],
      examples: [
        'let { atanh } = import("Math-Utils"); atanh(0)',
        'let { atanh } = import("Math-Utils"); atanh(0.9)',
        'let { atanh } = import("Math-Utils"); atanh(-0.5)',
        'let { atanh } = import("Math-Utils"); atanh([0.1, 0.2, 0.3])',
        'let { atanh } = import("Math-Utils"); atanh([[0.1, 0.2], [0.3, 0.4]])',
      ],
    },
  },
  'ln': {
    evaluate: unaryMathOp(val => Math.log(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `ln` function computes the natural logarithm (base `e`) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the natural logarithm of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.log2', 'Math-Utils.log10', '^'],
      examples: [
        'let { ln } = import("Math-Utils"); ln(0.01)',
        'let { ln } = import("Math-Utils"); ln(2.5)',
        'let { ln } = import("Math-Utils"); ln(E)',
        'let { ln } = import("Math-Utils"); ln([1, 2, 3])',
        'let { ln } = import("Math-Utils"); ln([[1, 2], [3, 4]])',
      ],
    },
  },
  'log2': {
    evaluate: unaryMathOp(val => Math.log2(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `log2` function computes the base `2` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-2 logarithm of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.ln', 'Math-Utils.log10'],
      examples: [
        'let { log2 } = import("Math-Utils"); log2(0.01)',
        'let { log2 } = import("Math-Utils"); log2(2 ^ 12)',
        'let { log2 } = import("Math-Utils"); log2(2.5)',
        'let { log2 } = import("Math-Utils"); log2([1, 2, 3])',
        'let { log2 } = import("Math-Utils"); log2([[1, 2], [3, 4]])',
      ],
    },
  },
  'log10': {
    evaluate: unaryMathOp(val => Math.log10(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `log10` function computes the base `10` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-10 logarithm of each element while preserving the original structure.',
      seeAlso: ['Math-Utils.ln', 'Math-Utils.log2'],
      examples: [
        'let { log10 } = import("Math-Utils"); log10(0.01)',
        'let { log10 } = import("Math-Utils"); log10(10 ^ 12)',
        'let { log10 } = import("Math-Utils"); log10(2.5)',
        'let { log10 } = import("Math-Utils"); log10([1, 2, 3])',
        'let { log10 } = import("Math-Utils"); log10([[1, 2], [3, 4]])',
      ],
    },
  },
  'to-rad': {
    evaluate: unaryMathOp(val => (val * Math.PI) / 180),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `to-rad` function converts an angle from degrees to radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
      seeAlso: ['Math-Utils.to-deg', 'Math-Utils.sin', 'Math-Utils.cos', 'Math-Utils.tan'],
      examples: [
        'let { to-rad } = import("Math-Utils"); to-rad(0)',
        'let { to-rad } = import("Math-Utils"); to-rad(90)',
        'let { to-rad } = import("Math-Utils"); to-rad(180)',
        'let { to-rad } = import("Math-Utils"); to-rad(360)',
        'let { to-rad } = import("Math-Utils"); to-rad([0, 90, 180])',
        'let { to-rad } = import("Math-Utils"); to-rad([[0, 90], [180, 360]])',
      ],
    },
  },
  'to-deg': {
    evaluate: unaryMathOp(val => (val * 180) / Math.PI),
    arity: toFixedArity(1),
    docs: {
      category: 'Math-Utils',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `to-deg` function converts an angle from radians to degrees, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
      seeAlso: ['Math-Utils.to-rad'],
      examples: [
        'let { to-deg } = import("Math-Utils"); to-deg(0)',
        'let { to-deg } = import("Math-Utils"); to-deg(PI)',
        'let { to-deg } = import("Math-Utils"); to-deg(PI / 2)',
        'let { to-deg } = import("Math-Utils"); to-deg(3 * PI / 2)',
        'let { to-deg } = import("Math-Utils"); to-deg([0, PI, PI / 2])',
        'let { to-deg } = import("Math-Utils"); to-deg([[0, PI], [PI / 2, 3 * PI / 2]])',
      ],
    },
  },
}

export const mathUtilsModule: LitsModule = {
  name: 'Math-Utils',
  functions: mathUtilsFunctions,
}
