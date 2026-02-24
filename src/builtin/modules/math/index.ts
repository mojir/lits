import type { SourceCodeInfo } from '../../../tokenizer/token'
import { isMatrix, isVector } from '../../../typeGuards/annotatedArrays'
import { isNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import { LitsError } from '../../../errors'
import type { BuiltinNormalExpressions } from '../../interface'
import type { LitsModule } from '../interface'

type NumberVectorOrMatrix = number | number[] | number[][]

function getNumberVectorOrMatrixOperation(
  param: unknown,
  sourceCodeInfo: SourceCodeInfo | undefined,
):
  | ['number', number]
  | ['vector', number[]]
  | ['matrix', number[][]] {
  if (isVector(param)) {
    return ['vector', param]
  }
  if (isMatrix(param)) {
    return ['matrix', param]
  }
  if (!isNumber(param)) {
    throw new LitsError(`Invalid parameter type: ${typeof param}`, sourceCodeInfo)
  }
  return ['number', param]
}

function unaryMathOp(
  fn: (val: number) => number,
): (params: unknown[], sourceCodeInfo: SourceCodeInfo | undefined) => NumberVectorOrMatrix {
  return ([param], sourceCodeInfo) => {
    const [operation, operand] = getNumberVectorOrMatrixOperation(param, sourceCodeInfo)
    if (operation === 'number') {
      return fn(operand)
    }
    else if (operation === 'vector') {
      return operand.map(val => fn(val))
    }
    else {
      return operand.map(row => row.map(val => fn(val)))
    }
  }
}

const mathUtilsFunctions: BuiltinNormalExpressions = {
  'sin': {
    evaluate: unaryMathOp(val => Math.sin(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sin` function computes the sine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sine of each element while preserving the original structure.',
      seeAlso: ['math.asin', 'math.sinh', 'math.cos', 'math.tan', 'math.to-rad'],
      examples: [
        `let { sin } = import(math);
sin(0)`,
        `let { sin } = import(math);
sin(1)`,
        `let { sin } = import(math);
sin(PI)`,
        `let { sin } = import(math);
sin(-0.5)`,
        `let { sin } = import(math);
sin([1, 2, 3])`,
        `let { sin } = import(math);
sin([[1, 2], [3, 4]])`,
      ],
    },
  },
  'asin': {
    evaluate: unaryMathOp(val => Math.asin(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `asin` function computes the arcsine (inverse sine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arcsine of each element while preserving the original structure.',
      seeAlso: ['math.sin', 'math.asinh', 'math.acos', 'math.atan'],
      examples: [
        `let { asin } = import(math);
asin(0)`,
        `let { asin } = import(math);
asin(1)`,
        `let { asin } = import(math);
asin(-0.5)`,
        `let { asin } = import(math);
asin([1, 2, 3])`,
        `let { asin } = import(math);
asin([[1, 2], [3, 4]])`,
      ],
    },
  },
  'sinh': {
    evaluate: unaryMathOp(val => Math.sinh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `sinh` function computes the hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic sine of each element while preserving the original structure.',
      seeAlso: ['math.asinh', 'math.sin', 'math.cosh', 'math.tanh'],
      examples: [
        `let { sinh } = import(math);
sinh(0)`,
        `let { sinh } = import(math);
sinh(1)`,
        `let { sinh } = import(math);
sinh(-0.5)`,
        `let { sinh } = import(math);
sinh([0.1, 0.2, 0.3])`,
        `let { sinh } = import(math);
sinh([[0.1, 0.2], [0.3, 0.4]])`,
      ],
    },
  },
  'asinh': {
    evaluate: unaryMathOp(val => Math.asinh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `asinh` function computes the inverse hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic sine of each element while preserving the original structure.',
      seeAlso: ['math.sinh', 'math.asin', 'math.acosh', 'math.atanh'],
      examples: [
        `let { asinh } = import(math);
asinh(10)`,
        `let { asinh } = import(math);
asinh(90)`,
        `let { asinh } = import(math);
asinh(50)`,
        `let { asinh } = import(math);
asinh([10, 20, 30])`,
        `let { asinh } = import(math);
asinh([[10, 20], [30, 40]])`,
      ],
    },
  },
  'cos': {
    evaluate: unaryMathOp(val => Math.cos(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cos` function computes the cosine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the cosine of each element while preserving the original structure.',
      seeAlso: ['math.acos', 'math.cosh', 'math.sin', 'math.tan', 'math.to-rad'],
      examples: [
        `let { cos } = import(math);
cos(0)`,
        `let { cos } = import(math);
cos(1)`,
        `let { cos } = import(math);
cos(PI)`,
        `let { cos } = import(math);
cos(-0.5)`,
        `let { cos } = import(math);
cos([1, 2, 3])`,
        `let { cos } = import(math);
cos([[1, 2], [3, 4]])`,
      ],
    },
  },
  'acos': {
    evaluate: unaryMathOp(val => Math.acos(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `acos` function computes the arccosine (inverse cosine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arccosine of each element while preserving the original structure.',
      seeAlso: ['math.cos', 'math.acosh', 'math.asin', 'math.atan'],
      examples: [
        `let { acos } = import(math);
acos(0)`,
        `let { acos } = import(math);
acos(1)`,
        `let { acos } = import(math);
acos(-0.5)`,
        `let { acos } = import(math);
acos([0.1, 0.2, 0.3])`,
        `let { acos } = import(math);
acos([[0.1, 0.2], [0.3, 0.4]])`,
      ],
    },
  },
  'cosh': {
    evaluate: unaryMathOp(val => Math.cosh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `cosh` function computes the hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic cosine of each element while preserving the original structure.',
      seeAlso: ['math.acosh', 'math.cos', 'math.sinh', 'math.tanh'],
      examples: [
        `let { cosh } = import(math);
cosh(0)`,
        `let { cosh } = import(math);
cosh(1)`,
        `let { cosh } = import(math);
cosh(-0.5)`,
        `let { cosh } = import(math);
cosh([0.1, 0.2, 0.3])`,
        `let { cosh } = import(math);
cosh([[0.1, 0.2], [0.3, 0.4]])`,
      ],
    },
  },
  'acosh': {
    evaluate: unaryMathOp(val => Math.acosh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `acosh` function computes the inverse hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic cosine of each element while preserving the original structure.',
      seeAlso: ['math.cosh', 'math.acos', 'math.asinh', 'math.atanh'],
      examples: [
        `let { acosh } = import(math);
acosh(1)`,
        `let { acosh } = import(math);
acosh(2)`,
        `let { acosh } = import(math);
acosh(100)`,
        `let { acosh } = import(math);
acosh(50)`,
        `let { acosh } = import(math);
acosh([1, 2, 3])`,
        `let { acosh } = import(math);
acosh([[1, 2], [3, 4]])`,
      ],
    },
  },
  'tan': {
    evaluate: unaryMathOp(val => Math.tan(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `tan` function computes the tangent of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the tangent of each element while preserving the original structure.',
      seeAlso: ['math.atan', 'math.tanh', 'math.sin', 'math.cos', 'math.to-rad'],
      examples: [
        `let { tan } = import(math);
tan(0)`,
        `let { tan } = import(math);
tan(1)`,
        `let { tan } = import(math);
tan(PI)`,
        `let { tan } = import(math);
tan(-0.5)`,
        `let { tan } = import(math);
tan([1, 2, 3])`,
        `let { tan } = import(math);
tan([[1, 2], [3, 4]])`,
      ],
    },
  },
  'atan': {
    evaluate: unaryMathOp(val => Math.atan(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `atan` function computes the arctangent (inverse tangent) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arctangent of each element while preserving the original structure.',
      seeAlso: ['math.tan', 'math.atanh', 'math.asin', 'math.acos'],
      examples: [
        `let { atan } = import(math);
atan(0)`,
        `let { atan } = import(math);
atan(1)`,
        `let { atan } = import(math);
atan(-0.5)`,
        `let { atan } = import(math);
atan([0.1, 0.2, 0.3])`,
        `let { atan } = import(math);
atan([[0.1, 0.2], [0.3, 0.4]])`,
      ],
    },
  },
  'tanh': {
    evaluate: unaryMathOp(val => Math.tanh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `tanh` function computes the hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic tangent of each element while preserving the original structure.',
      seeAlso: ['math.atanh', 'math.tan', 'math.sinh', 'math.cosh'],
      examples: [
        `let { tanh } = import(math);
tanh(0)`,
        `let { tanh } = import(math);
tanh(1)`,
        `let { tanh } = import(math);
tanh(-0.5)`,
        `let { tanh } = import(math);
tanh(50)`,
      ],
    },
  },
  'atanh': {
    evaluate: unaryMathOp(val => Math.atanh(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `atanh` function computes the inverse hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic tangent of each element while preserving the original structure.',
      seeAlso: ['math.tanh', 'math.atan', 'math.asinh', 'math.acosh'],
      examples: [
        `let { atanh } = import(math);
atanh(0)`,
        `let { atanh } = import(math);
atanh(0.9)`,
        `let { atanh } = import(math);
atanh(-0.5)`,
        `let { atanh } = import(math);
atanh([0.1, 0.2, 0.3])`,
        `let { atanh } = import(math);
atanh([[0.1, 0.2], [0.3, 0.4]])`,
      ],
    },
  },
  'ln': {
    evaluate: unaryMathOp(val => Math.log(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `ln` function computes the natural logarithm (base `e`) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the natural logarithm of each element while preserving the original structure.',
      seeAlso: ['math.log2', 'math.log10', '^'],
      examples: [
        `let { ln } = import(math);
ln(0.01)`,
        `let { ln } = import(math);
ln(2.5)`,
        `let { ln } = import(math);
ln(E)`,
        `let { ln } = import(math);
ln([1, 2, 3])`,
        `let { ln } = import(math);
ln([[1, 2], [3, 4]])`,
      ],
    },
  },
  'log2': {
    evaluate: unaryMathOp(val => Math.log2(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `log2` function computes the base `2` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-2 logarithm of each element while preserving the original structure.',
      seeAlso: ['math.ln', 'math.log10'],
      examples: [
        `let { log2 } = import(math);
log2(0.01)`,
        `let { log2 } = import(math);
log2(2 ^ 12)`,
        `let { log2 } = import(math);
log2(2.5)`,
        `let { log2 } = import(math);
log2([1, 2, 3])`,
        `let { log2 } = import(math);
log2([[1, 2], [3, 4]])`,
      ],
    },
  },
  'log10': {
    evaluate: unaryMathOp(val => Math.log10(val)),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `log10` function computes the base `10` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-10 logarithm of each element while preserving the original structure.',
      seeAlso: ['math.ln', 'math.log2'],
      examples: [
        `let { log10 } = import(math);
log10(0.01)`,
        `let { log10 } = import(math);
log10(10 ^ 12)`,
        `let { log10 } = import(math);
log10(2.5)`,
        `let { log10 } = import(math);
log10([1, 2, 3])`,
        `let { log10 } = import(math);
log10([[1, 2], [3, 4]])`,
      ],
    },
  },
  'to-rad': {
    evaluate: unaryMathOp(val => (val * Math.PI) / 180),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `to-rad` function converts an angle from degrees to radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
      seeAlso: ['math.to-deg', 'math.sin', 'math.cos', 'math.tan'],
      examples: [
        `let { to-rad } = import(math);
to-rad(0)`,
        `let { to-rad } = import(math);
to-rad(90)`,
        `let { to-rad } = import(math);
to-rad(180)`,
        `let { to-rad } = import(math);
to-rad(360)`,
        `let { to-rad } = import(math);
to-rad([0, 90, 180])`,
        `let { to-rad } = import(math);
to-rad([[0, 90], [180, 360]])`,
      ],
    },
  },
  'to-deg': {
    evaluate: unaryMathOp(val => (val * 180) / Math.PI),
    arity: toFixedArity(1),
    docs: {
      category: 'math',
      returns: { type: ['number', 'vector', 'matrix'] },
      args: {
        x: { type: ['number', 'vector', 'matrix'] },
      },
      variants: [{ argumentNames: ['x'] }],
      description: 'The `to-deg` function converts an angle from radians to degrees, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
      seeAlso: ['math.to-rad'],
      examples: [
        `let { to-deg } = import(math);
to-deg(0)`,
        `let { to-deg } = import(math);
to-deg(PI)`,
        `let { to-deg } = import(math);
to-deg(PI / 2)`,
        `let { to-deg } = import(math);
to-deg(3 * PI / 2)`,
        `let { to-deg } = import(math);
to-deg([0, PI, PI / 2])`,
        `let { to-deg } = import(math);
to-deg([[0, PI], [PI / 2, 3 * PI / 2]])`,
      ],
    },
  },
}

export const mathUtilsModule: LitsModule = {
  name: 'math',
  functions: mathUtilsFunctions,
}
