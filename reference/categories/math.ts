import type { FunctionReference } from '..'
import { type MathApiName, getOperatorArgs } from '../api'

export const mathReference: Record<MathApiName, FunctionReference<'Math'>> = {
  '+': {
    title: '+',
    category: 'Math',
    linkName: '-plus',
    returns: {
      type: 'number',
    },
    args: {
      xs: {
        type: 'number',
        rest: true,
      },
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Computes sum of $xs.',
    examples: ['1 + 2', '1 + 20 + 30', '+(1, 2, 3, 4)', '+()', '+(1)'],
  },
  '-': {
    title: '-',
    category: 'Math',
    linkName: '-minus',
    returns: {
      type: 'number',
    },
    args: {
      xs: {
        type: 'number',
        rest: true,
      },
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Computes difference between first value and sum of the rest. When called with only one argument, it does negation.',
    examples: ['50 - 8', '1 - 1 - 1', '-()', '-(4, 2)', '-(4, 3, 2, 1,)'],
  },
  '*': {
    title: '*',
    category: 'Math',
    linkName: '-star',
    returns: {
      type: 'number',
    },
    args: {
      xs: {
        type: 'number',
        rest: true,
      },
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Computes product of $xs.',
    examples: ['6 * 7', '-1 * 4', '*(4, 7)', '*(1, 2, 3, 4, 5)', '*()', '*(8)'],
  },
  '/': {
    title: '/',
    category: 'Math',
    linkName: '-slash',
    clojureDocs: '_fs',
    returns: {
      type: 'number',
    },
    args: {
      xs: {
        type: 'number',
        rest: true,
      },
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['xs'] },
    ],
    description: 'Computes division or reciprocal. When called with one argument it computes reciprocal. When called with two or more arguments it does compute division of the first by the all remaining $xs.',
    examples: ['12 / 100', '-1 / 4', '/(7, 4)', '/(1, 2, 4, 8)', '/()', '/(8)'],
  },
  'mod': {
    title: 'mod',
    category: 'Math',
    linkName: 'mod',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Modulus of `dividend` and `divisor`. Truncates toward negative infinity.',
    examples: ['mod(5, 3)', 'mod(5.2, 3.1)', 'mod(-5, 3)', '5 mod -3', '-5 mod -3'],
  },
  '%': {
    title: '%',
    category: 'Math',
    linkName: '-percent',
    clojureDocs: 'rem',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Remainder of dividing `dividend` and `divisor`.',
    examples: ['5 % 3', '5.2 % 3.1', '-5 % 3', '%(5, -3)', '%(-5, -3)'],
    aliases: ['rem'],
  },
  'quot': {
    title: 'quot',
    category: 'Math',
    linkName: 'quot',
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Quotient of dividing `dividend` and `divisor`.',
    examples: ['quot(5, 3)', 'quot(5.2, 3.1)', 'quot(-5, 3)', '5 quot -3', '-5 quot -3'],
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
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Adds one to $x.',
    examples: ['inc(0)', 'inc(1)', 'inc(100.1)'],
  },
  'dec': {
    title: 'dec',
    category: 'Math',
    linkName: 'dec',
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Subtracts one from $x.',
    examples: ['dec(0)', 'dec(1)', 'dec(100.1)'],
  },
  '√': {
    title: '√',
    category: 'Math',
    linkName: 'sqrt',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Computes square root of $x.',
    examples: ['√(0)', '√(9)', '√(2)', 'sqrt(0)', 'sqrt(9)', 'sqrt(2)'],
    aliases: ['sqrt'],
  },
  '∛': {
    title: '∛',
    category: 'Math',
    linkName: 'cbrt',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Computes cube root of $x.',
    examples: ['∛(0)', '∛(27)', '∛(2)', '∛(1)', 'cbrt(0)', 'cbrt(27)', 'cbrt(2)', 'cbrt(1)'],
    aliases: ['cbrt'],
  },
  '**': {
    title: '**',
    category: 'Math',
    linkName: '-star-star',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      ...getOperatorArgs('number', 'number'),
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Computes returns $a raised to the power of $b.',
    examples: ['2 ** 3', '2 ** 0', '2 ** -3', '**(-2, 3)', '**(-2, -3)'],
  },
  'round': {
    title: 'round',
    category: 'Math',
    linkName: 'round',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
      decimals: {
        type: 'integer',
      },
    },
    variants: [
      { argumentNames: ['x'] },
      { argumentNames: ['x', 'decimals'] },
    ],
    description: 'Returns rounded $x. If $decimals is provided it return a number with that many decimals.',
    examples: [
      'round(2)',
      'round(2.49)',
      'round(2.5)',
      'round(-2.49)',
      'round(-2.5)',
      'round(-2.501)',
      'round(1.23456789, 4)',
      '1.123456789 round 2',
    ],
    noOperatorDocumentation: true,
  },
  'trunc': {
    title: 'trunc',
    category: 'Math',
    linkName: 'trunc',
    clojureDocs: null,
    returns: {
      type: 'integer',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the integer part of $x by removing any fractional digits.',
    examples: ['trunc(2)', 'trunc(2.49)', 'trunc(2.5)', 'trunc(-2.49)', 'trunc(-2.5)', 'trunc(-2.501)'],
  },
  'floor': {
    title: 'floor',
    category: 'Math',
    linkName: 'floor',
    clojureDocs: null,
    returns: {
      type: 'integer',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the largest `integer` less than or equal to $x.',
    examples: ['floor(2)', 'floor(2.49)', 'floor(2.5)', 'floor(-2.49)', 'floor(-2.5)', 'floor(-2.501)'],
  },
  'ceil': {
    title: 'ceil',
    category: 'Math',
    linkName: 'ceil',
    clojureDocs: null,
    returns: {
      type: 'integer',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the smallest `integer` larger than or equal to $x.',
    examples: ['ceil(2)', 'ceil(2.49)', 'ceil(2.5)', 'ceil(-2.49)', 'ceil(-2.5)', 'ceil(-2.501)'],
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
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the absolute value of $x.',
    examples: ['abs(-2.3)', 'abs(0)', 'abs(2.5)'],
  },
  'sign': {
    title: 'sign',
    category: 'Math',
    linkName: 'sign',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns `1` if $x `> 0`, `-1` if $x `< 0`, `0` if $x `= 0` or `-0` if $x `= -0`.',
    examples: ['sign(-2.3)', 'sign(-0)', 'sign(0)', 'sign(12312)'],
  },
  'log': {
    title: 'log',
    category: 'Math',
    linkName: 'log',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the natural logarithm (base e) of $x.',
    examples: ['log(0.01)', 'log(2.5)'],
  },
  'log2': {
    title: 'log2',
    category: 'Math',
    linkName: 'log2',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the base `2` logarithm of a number.',
    examples: ['log2(0.01)', 'log2(2 ** 12)', 'log2(2.5)'],
  },
  'log10': {
    title: 'log10',
    category: 'Math',
    linkName: 'log10',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the `10` logarithm of a number.',
    examples: ['log10(0.01)', 'log10(10 ** 12)', 'log10(2.5)'],
  },
  'sin': {
    title: 'sin',
    category: 'Math',
    linkName: 'sin',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the sine of $x. $x must be specified in radians.',
    examples: ['sin(0)', 'sin(1)', 'sin(PI)', 'sin(-0.5)'],
  },
  'cos': {
    title: 'cos',
    category: 'Math',
    linkName: 'cos',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the cosine of $x. $x must be specified in radians.',
    examples: ['cos(0)', 'cos(1)', 'cos(PI)', 'cos(-0.5)'],
  },
  'tan': {
    title: 'tan',
    category: 'Math',
    linkName: 'tan',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the tangent of $x. $x must be specified in radians.',
    examples: ['tan(0)', 'tan(1)', 'tan(PI)', 'tan(-0.5)'],
  },
  'asin': {
    title: 'asin',
    category: 'Math',
    linkName: 'asin',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the arcsine (in radians) of $x.',
    examples: ['asin(0)', 'asin(1)', 'asin(-0.5)'],
  },
  'acos': {
    title: 'acos',
    category: 'Math',
    linkName: 'acos',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the arccosine (in radians) of $x.',
    examples: ['acos(0)', 'acos(1)', 'acos(-0.5)'],
  },
  'atan': {
    title: 'atan',
    category: 'Math',
    linkName: 'atan',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the arctangent (in radians) of $x.',
    examples: ['atan(0)', 'atan(1)', 'atan(-0.5)'],
  },
  'sinh': {
    title: 'sinh',
    category: 'Math',
    linkName: 'sinh',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the hyperbolic sine of $x.',
    examples: ['sinh(0)', 'sinh(1)', 'sinh(-0.5)'],
  },
  'cosh': {
    title: 'cosh',
    category: 'Math',
    linkName: 'cosh',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the hyperbolic cosine of $x.',
    examples: ['cosh(0)', 'cosh(1)', 'cosh(-0.5)'],
  },
  'tanh': {
    title: 'tanh',
    category: 'Math',
    linkName: 'tanh',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the hyperbolic tangent of $x.',
    examples: ['tanh(0)', 'tanh(1)', 'tanh(-0.5)', 'tanh(50)'],
  },
  'asinh': {
    title: 'asinh',
    category: 'Math',
    linkName: 'asinh',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the hyperbolic arcsine of $x.',
    examples: ['asinh(0)', 'asinh(0.9)', 'asinh (0.5)'],
  },
  'acosh': {
    title: 'acosh',
    category: 'Math',
    linkName: 'acosh',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the hyperbolic arccosine of $x.',
    examples: ['acosh(1)', 'acosh(2)', 'acosh(100)'],
  },
  'atanh': {
    title: 'atanh',
    category: 'Math',
    linkName: 'atanh',
    clojureDocs: null,
    returns: {
      type: 'number',
    },
    args: {
      x: {
        type: 'number',
      },
    },
    variants: [
      { argumentNames: ['x'] },
    ],
    description: 'Returns the hyperbolic arctangent of $x.',
    examples: ['atanh(0)', 'atanh(0.9)', 'atanh(-0.5)'],
  },
}
