import type { FunctionReference, NormalExpressionArgument } from '..'
import { type MathApiName, getOperatorArgs } from '../api'

const mixedArgs: NormalExpressionArgument = {
  type: ['number', 'vector', 'matrix'],
  rest: true,
}
const mixedOperatorArgs = getOperatorArgs(['number', 'vector', 'matrix'], ['number', 'vector', 'matrix'])
export const mathReference: Record<MathApiName, FunctionReference<'Math'>> = {
  '+': {
    title: '+',
    category: 'Math',
    linkName: '-plus',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      xs: mixedArgs,
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'The `+` function performs addition of numbers and element-wise addition of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it adds the scalar to each element of the collection.',
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
  '-': {
    title: '-',
    category: 'Math',
    linkName: '-minus',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      xs: mixedArgs,
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Computes difference between first value and sum of the rest. When called with only one argument, it does negation.',
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
  '*': {
    title: '*',
    category: 'Math',
    linkName: '-star',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      xs: mixedArgs,
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'The `*` function performs multiplication of `numbers` and element-wise multiplication of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it multiplies each element of the collection by the scalar.',
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
    aliases: ['·'],
  },
  '/': {
    title: '/',
    category: 'Math',
    linkName: '-slash',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      xs: mixedArgs,
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'The `/` function performs division of `numbers` and element-wise division of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it divides each element of the collection by the scalar.',
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
  '~': {
    title: '~',
    category: 'Math',
    linkName: '-tilde',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['a, b'] },
    ],
    description: 'The `~` checks for approximately equal values, returning `true` if the absolute difference between the two numbers is less than or equal to a small threshold (default: `1e-10`). It works on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it checks each element for approximate equality.',
    examples: [
      '~(0.1, 0.1)',
      '~(0.1, 0.10000000000001)',
      '~(0.1, 0.2)',
      '~(0.1, 0.101, 0.1)',
      '~([1, 2, 3], [1.00000000000001, 2.00000000000001, 3.00000000000001])',
      '~([1, 2, 3], [1.1, 2.1, 3.1])',
      '~([[1, 2, 3], [1, 2, 3]], [[1.01, 2.01, 3.01], [1.01, 2.01, 3.01]], 0.1)',
    ],
    aliases: ['≈'],
  },
  'mod': {
    title: 'mod',
    category: 'Math',
    linkName: 'mod',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'The `mod` function computes the modulo of division with the same sign as the divisor, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the modulo operation between each element of the collection and the scalar.',
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
  'rem': {
    title: 'rem',
    category: 'Math',
    linkName: 'rem',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'The `rem` function computes the remainder of division with the same sign as the dividend, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the remainder operation between each element of the collection and the scalar.',
    examples: [
      '5 % 3',
      '5.2 % 3.1',
      '-5 % 3',
      '%(5, -3)',
      '%(-5, -3)',
      '5 rem -3',
      '-5 rem -3',
      '[1, 2, 3] % 2',
      '2 % [1, 2, 3]',
      '%([1, 2, 3], [4, 5, 6])',
      '[[1, 2, 3], [4, 5, 6]] % [[7, 8, 9], [10, 11, 12]]',
      '%([[1, 2, 3], [4, 5, 6]], 2)',
      '[[1, 2, 3], [4, 5, 6]] rem [[7, 8, 9], [10, 11, 12]]',
    ],
    aliases: ['%'],
  },
  'quot': {
    title: 'quot',
    category: 'Math',
    linkName: 'quot',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'The `quot` function performs integer division truncated toward zero, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies integer division between each element of the collection and the scalar.',
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
  'inc': {
    title: 'inc',
    category: 'Math',
    linkName: 'inc',
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `inc` function increments its argument by 1, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it increases each element by 1 while preserving the original structure.',
    examples: [
      'inc(0)',
      'inc(1)',
      'inc(100.1)',
      'inc([1, 2, 3])',
      'inc([[1, 2], [3, 4]])',
    ],
  },
  'dec': {
    title: 'dec',
    category: 'Math',
    linkName: 'dec',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `dec` function decrements its argument by 1, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it decreases each element by 1 while preserving the original structure.',
    examples: [
      'dec(0)',
      'dec(1)',
      'dec(100.1)',
      'dec([1, 2, 3])',
      'dec([[1, 2], [3, 4]])',
    ],
  },
  'sqrt': {
    title: 'sqrt',
    category: 'Math',
    linkName: 'sqrt',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `sqrt` function calculates the square root of `numbers` and computes element-wise square roots of `vectors` and `matrices`. When applied to collections, it returns the square root of each element while preserving the original structure.',
    examples: [
      '√(0)',
      '√(9)',
      '√(2)',
      'sqrt(0)',
      'sqrt(9)',
      'sqrt(2)',
      'sqrt([1, 4, 9])',
      'sqrt([[1, 4], [9, 16]])',
    ],
    aliases: ['√'],
  },
  'cbrt': {
    title: 'cbrt',
    category: 'Math',
    linkName: 'cbrt',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `cbrt` function calculates the cube root of `numbers` and computes element-wise cube roots of `vectors` and `matrices`. When applied to collections, it returns the cube root of each element while preserving the original structure.',
    examples: [
      '∛(0)',
      '∛(27)',
      '∛(2)',
      '∛(1)',
      'cbrt(0)',
      'cbrt(27)',
      'cbrt(2)',
      'cbrt(1)',
      'cbrt([1, 8, 27])',
      'cbrt([[1, 8], [27, 64]])',
    ],
    aliases: ['∛'],
  },
  '^': {
    title: '^',
    category: 'Math',
    linkName: '-caret',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      ...mixedOperatorArgs,
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'The ^ function computes exponentiation, raising the first argument to the power of the second, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the power operation between each element of the collection and the scalar.',
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
  'round': {
    title: 'round',
    category: 'Math',
    linkName: 'round',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      ...getOperatorArgs(['number', 'vector', 'matrix'], 'integer'),
    },
    variants: [
      { argumentNames: ['a'] },
      { argumentNames: ['a', 'b'] },
    ],
    description: 'The `round` function rounds a `number` to the nearest `integer` or to a specified number of `decimal` places, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it rounds each element while preserving the original structure.',
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
  'trunc': {
    title: 'trunc',
    category: 'Math',
    linkName: 'trunc',
    returns: {
      type: ['integer', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `trunc` function truncates `numbers` toward zero (removing decimal portions without rounding), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it truncates each element while preserving the original structure.',
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
  'floor': {
    title: 'floor',
    category: 'Math',
    linkName: 'floor',
    returns: {
      type: ['integer', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `floor` function returns the largest `integer` less than or equal to a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the floor of each element while preserving the original structure.',
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
  'ceil': {
    title: 'ceil',
    category: 'Math',
    linkName: 'ceil',
    returns: {
      type: ['integer', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `ceil` function returns the smallest `integer` greater than or equal to a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the ceiling of each element while preserving the original structure.',
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
  'min': {
    title: 'min',
    category: 'Math',
    linkName: 'min',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('number', 'number'),
      xs: {
        type: 'number',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Returns the smallest number of the arguments.',
    examples: [
      '2 min 3',
      'min(2, 0, 1)',
      'min(2, -1, 1)',
      'min(2.5)',
      '12 min 14',
    ],
  },
  'max': {
    title: 'max',
    category: 'Math',
    linkName: 'max',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('number', 'number'),
      xs: {
        type: 'number',
        rest: true,
      },
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Returns the largest number of the arguments.',
    examples: [
      ' 2 max 3',
      'max(2, 0, 1)',
      'max(2, -1, 1)',
      'max(2, 0.5)',
      '4 max 2',
    ],
  },
  'abs': {
    title: 'abs',
    category: 'Math',
    linkName: 'abs',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The abs function returns the absolute value (magnitude) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the absolute value of each element while preserving the original structure.',
    examples: [
      'abs(-2.3)',
      'abs(0)',
      'abs(2.5)',
      'abs([1, -2, 3])',
      'abs([[1, -2], [3, -4]])',
    ],
  },
  'sign': {
    title: 'sign',
    category: 'Math',
    linkName: 'sign',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `sign` function returns the `sign** of a **number` (-1 for negative, 0 for zero, 1 for positive), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sign of each element while preserving the original structure.',
    examples: [
      'sign(-2.3)',
      'sign(-0)',
      'sign(0)',
      'sign(12312)',
      'sign([1, -2, 3])',
      'sign([[1, -2], [3, -4]])',
    ],
  },
  'ln': {
    title: 'ln',
    category: 'Math',
    linkName: 'ln',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `ln` function computes the natural logarithm (base `e`) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the natural logarithm of each element while preserving the original structure.',
    examples: [
      'ln(0.01)',
      'ln(2.5)',
      'ln(E)',
      'ln([1, 2, 3])',
      'ln([[1, 2], [3, 4]])',
    ],
  },
  'log2': {
    title: 'log2',
    category: 'Math',
    linkName: 'log2',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `log2` function computes the base `2` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-2 logarithm of each element while preserving the original structure.',
    examples: [
      'log2(0.01)',
      'log2(2 ^ 12)',
      'log2(2.5)',
      'log2([1, 2, 3])',
      'log2([[1, 2], [3, 4]])',
    ],
    aliases: ['log₂'],
  },
  'log10': {
    title: 'log10',
    category: 'Math',
    linkName: 'log10',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    aliases: ['log₁₀'],
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `log10` function computes the base `10` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-10 logarithm of each element while preserving the original structure.',
    examples: [
      'log10(0.01)',
      'log10(10 ^ 12)',
      'log10(2.5)',
      'log10([1, 2, 3])',
      'log10([[1, 2], [3, 4]])',
    ],
  },
  'sin': {
    title: 'sin',
    category: 'Math',
    linkName: 'sin',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `sin` function computes the sine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sine of each element while preserving the original structure.',
    examples: [
      'sin(0)',
      'sin(1)',
      'sin(PI)',
      'sin(-0.5)',
      'sin([1, 2, 3])',
      'sin([[1, 2], [3, 4]])',
    ],
  },
  'cos': {
    title: 'cos',
    category: 'Math',
    linkName: 'cos',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `cos` function computes the cosine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the cosine of each element while preserving the original structure.',
    examples: [
      'cos(0)',
      'cos(1)',
      'cos(PI)',
      'cos(-0.5)',
      'cos([1, 2, 3])',
      'cos([[1, 2], [3, 4]])',
    ],
  },
  'tan': {
    title: 'tan',
    category: 'Math',
    linkName: 'tan',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `tan` function computes the tangent of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the tangent of each element while preserving the original structure.',
    examples: [
      'tan(0)',
      'tan(1)',
      'tan(PI)',
      'tan(-0.5)',
      'tan([1, 2, 3])',
      'tan([[1, 2], [3, 4]])',
    ],
  },
  'asin': {
    title: 'asin',
    category: 'Math',
    linkName: 'asin',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `asin` function computes the arcsine (inverse sine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arcsine of each element while preserving the original structure.',
    examples: [
      'asin(0)',
      'asin(1)',
      'asin(-0.5)',
      'asin([1, 2, 3])',
      'asin([[1, 2], [3, 4]])',
    ],
  },
  'acos': {
    title: 'acos',
    category: 'Math',
    linkName: 'acos',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `acos` function computes the arccosine (inverse cosine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arccosine of each element while preserving the original structure.',
    examples: [
      'acos(0)',
      'acos(1)',
      'acos(-0.5)',
      'acos([0.1, 0.2, 0.3])',
      'acos([[0.1, 0.2], [0.3, 0.4]])',
    ],
  },
  'atan': {
    title: 'atan',
    category: 'Math',
    linkName: 'atan',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `atan` function computes the arctangent (inverse tangent) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arctangent of each element while preserving the original structure.',
    examples: [
      'atan(0)',
      'atan(1)',
      'atan(-0.5)',
      'atan([0.1, 0.2, 0.3])',
      'atan([[0.1, 0.2], [0.3, 0.4]])',
    ],
  },
  'sinh': {
    title: 'sinh',
    category: 'Math',
    linkName: 'sinh',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `sinh` function computes the hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic sine of each element while preserving the original structure.',
    examples: [
      'sinh(0)',
      'sinh(1)',
      'sinh(-0.5)',
      'sinh([0.1, 0.2, 0.3])',
      'sinh([[0.1, 0.2], [0.3, 0.4]])',
    ],
  },
  'cosh': {
    title: 'cosh',
    category: 'Math',
    linkName: 'cosh',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `cosh` function computes the hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic cosine of each element while preserving the original structure.',
    examples: [
      'cosh(0)',
      'cosh(1)',
      'cosh(-0.5)',
      'cosh([0.1, 0.2, 0.3])',
      'cosh([[0.1, 0.2], [0.3, 0.4]])',
    ],
  },
  'tanh': {
    title: 'tanh',
    category: 'Math',
    linkName: 'tanh',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `tanh` function computes the hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic tangent of each element while preserving the original structure.',
    examples: ['tanh(0)', 'tanh(1)', 'tanh(-0.5)', 'tanh(50)'],
  },
  'asinh': {
    title: 'asinh',
    category: 'Math',
    linkName: 'asinh',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `asinh` function computes the inverse hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic sine of each element while preserving the original structure.',
    examples: [
      'asinh(10)',
      'asinh(90)',
      'asinh (50)',
      'asinh([10, 20, 30])',
      'asinh([[10, 20], [30, 40]])',
    ],
  },
  'acosh': {
    title: 'acosh',
    category: 'Math',
    linkName: 'acosh',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `acosh` function computes the inverse hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic cosine of each element while preserving the original structure.',
    examples: [
      'acosh(1)',
      'acosh(2)',
      'acosh(100)',
      'acosh(50)',
      'acosh([1, 2, 3])',
      'acosh([[1, 2], [3, 4]])',
    ],
  },
  'atanh': {
    title: 'atanh',
    category: 'Math',
    linkName: 'atanh',
    returns: {
      type: ['number', 'vector', 'matrix'],
    },
    args: {
      x: {
        type: ['number', 'vector', 'matrix'],
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'The `atanh` function computes the inverse hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic tangent of each element while preserving the original structure.',
    examples: [
      'atanh(0)',
      'atanh(0.9)',
      'atanh(-0.5)',
      'atanh([0.1, 0.2, 0.3])',
      'atanh([[0.1, 0.2], [0.3, 0.4]])',
    ],
  },
}
