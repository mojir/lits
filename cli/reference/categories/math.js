module.exports = {
  '+': {
    name: `+`,
    category: `Math`,
    linkName: `_plus`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `numbers`,
        type: `number[]`,
        description: `zero or more`,
      },
    ],
    description: `Computes sum of \`numbers\`.`,
    examples: [`(+)`, `(+ 1)`, `(+ 2 4)`, `(+ 1 2 3 4)`, `(+ (+ 2 3) (+ 5 6))`],
  },
  '-': {
    name: `-`,
    category: `Math`,
    linkName: `_minus`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `numbers`,
        type: `number[]`,
        description: `zero or more`,
      },
    ],
    description: `Computes difference between first value and sum of the rest. When called with only one argument, it does negation.`,
    examples: [`(-)`, `(- 1)`, `(- 2 4)`, `(- 4 3 2 1)`],
  },
  '*': {
    name: `*`,
    category: `Math`,
    linkName: `_star`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `numbers`,
        type: `number[]`,
        description: `zero or more`,
      },
    ],
    description: `Computes product of \`numbers\`.`,
    examples: [`(*)`, `(* 2)`, `(* 2 4)`, `(* 1 2 3 4)`],
  },
  '/': {
    name: `/`,
    category: `Math`,
    linkName: `_slash`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `numbers`,
        type: `number[]`,
        description: `zero or more`,
      },
    ],
    description: `Computes division or reciprocal. When called with one argument it computes reciprocal. When called with two or more arguments it does compute division of the first by the all remaining \`numbers\`.`,
    examples: [`(/)`, `(/ 2)`, `(/ 2 4)`, `(/ 4 3 2 1)`],
  },
  mod: {
    name: `mod`,
    category: `Math`,
    linkName: `mod`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `dividend`,
        type: `number`,
      },
      {
        name: `divisor`,
        type: `number`,
      },
    ],
    description: `Modulus of \`dividend\` and \`divisor\`. Truncates toward negative infinity.`,
    examples: [`(mod 5 3)`, `(mod 5.2 3.1)`, `(mod -5 3)`, `(mod 5 -3)`, `(mod -5 -3)`],
  },
  rem: {
    name: `rem`,
    category: `Math`,
    linkName: `rem`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `dividend`,
        type: `number`,
      },
      {
        name: `divisor`,
        type: `number`,
      },
    ],
    description: `Remainder of dividing \`dividend\` and \`divisor\`.`,
    examples: [`(rem 5 3)`, `(rem 5.2 3.1)`, `(rem -5 3)`, `(rem 5 -3)`, `(rem -5 -3)`],
  },
  quot: {
    name: `quot`,
    category: `Math`,
    linkName: `quot`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `dividend`,
        type: `number`,
      },
      {
        name: `divisor`,
        type: `number`,
      },
    ],
    description: `Quotient of dividing \`dividend\` and \`divisor\`.`,
    examples: [`(quot 5 3)`, `(quot 5.2 3.1)`, `(quot -5 3)`, `(quot 5 -3)`, `(quot -5 -3)`],
  },
  inc: {
    name: `inc`,
    category: `Math`,
    linkName: `inc`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Adds one to \`number\`.`,
    examples: [`(inc 0)`, `(inc 1)`, `(inc 100.1)`],
  },
  dec: {
    name: `dec`,
    category: `Math`,
    linkName: `dec`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Subtracts one from \`number\`.`,
    examples: [`(dec 0)`, `(dec 1)`, `(dec 100.1)`],
  },
  sqrt: {
    name: `sqrt`,
    category: `Math`,
    linkName: `sqrt`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Computes square root of \`number\`.`,
    examples: [`(sqrt 0)`, `(sqrt 9)`, `(sqrt 2)`],
  },
  cbrt: {
    name: `cbrt`,
    category: `Math`,
    linkName: `cbrt`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Computes cube root of \`number\`.`,
    examples: [`(cbrt 0)`, `(cbrt 27)`, `(cbrt 2)`, `(cbrt 1)`],
  },
  pow: {
    name: `pow`,
    category: `Math`,
    linkName: `pow`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `base-number`,
        type: `number`,
      },
      {
        name: `power-number`,
        type: `number`,
      },
    ],
    description: `Computes returns \`base-number\` raised to the \`power-number\`.`,
    examples: [`(pow 2 3)`, `(pow 2 0)`, `(pow 2 -3)`, `(pow -2 3)`, `(pow -2 -3)`],
  },
  exp: {
    name: `exp`,
    category: `Math`,
    linkName: `exp`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `power-number`,
        type: `number`,
      },
    ],
    description: `Computes \`e\` rasied to the \`power-number\`.`,
    examples: [`(exp 3)`, `(exp 0)`, `(exp -3)`, `(exp 3)`],
  },
  round: {
    name: `round`,
    category: `Math`,
    linkName: `round`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
      {
        name: `decimals`,
        type: `integer`,
        description: `optional`,
      },
    ],
    description: `Returns rounded \`number\`. If \`decimals\` is provided it return a number with that many decimals.`,
    examples: [
      `(round 2)`,
      `(round 2.49)`,
      `(round 2.5)`,
      `(round -2.49)`,
      `(round -2.5)`,
      `(round -2.501)`,
      `(round 1.23456789 4)`,
    ],
  },
  trunc: {
    name: `trunc`,
    category: `Math`,
    linkName: `trunc`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the integer part of \`number\` by removing any fractional digits.`,
    examples: [`(trunc 2)`, `(trunc 2.49)`, `(trunc 2.5)`, `(trunc -2.49)`, `(trunc -2.5)`, `(trunc -2.501)`],
  },
  floor: {
    name: `floor`,
    category: `Math`,
    linkName: `floor`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the largest \`integer\` less than or equal to \`number\`.`,
    examples: [`(floor 2)`, `(floor 2.49)`, `(floor 2.5)`, `(floor -2.49)`, `(floor -2.5)`, `(floor -2.501)`],
  },
  ceil: {
    name: `ceil`,
    category: `Math`,
    linkName: `ceil`,
    returns: {
      type: `integer`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the smallest \`integer\` larger than or equal to \`number\`.`,
    examples: [`(ceil 2)`, `(ceil 2.49)`, `(ceil 2.5)`, `(ceil -2.49)`, `(ceil -2.5)`, `(ceil -2.501)`],
  },
  min: {
    name: `min`,
    category: `Math`,
    linkName: `min`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `numbers`,
        type: `number[]`,
        description: `(one or many)`,
      },
    ],
    description: `Returns the smallest number of the arguments.`,
    examples: [`(min 2 0 1)`, `(min 2 -1 1)`, `(min 2.5)`],
  },
  max: {
    name: `max`,
    category: `Math`,
    linkName: `max`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `numbers`,
        type: `number[]`,
        description: `(one or many)`,
      },
    ],
    description: `Returns the largest number of the arguments.`,
    examples: [`(max 2 0 1)`, `(max 2 -1 1)`, `(max 2.5)`],
  },
  abs: {
    name: `abs`,
    category: `Math`,
    linkName: `abs`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the absolute value of \`number\`.`,
    examples: [`(abs -2.3)`, `(abs 0)`, `(abs 2.5)`],
  },
  sign: {
    name: `sign`,
    category: `Math`,
    linkName: `sign`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns \`1\` if \`number > 0\`, \`-1\` if \`number < 0\`, \`0\` if \`number = 0\` or \`-0\` if \`number = -0\`.`,
    examples: [`(sign -2.3)`, `(sign -0)`, `(sign 0)`, `(sign 12312)`],
  },
  infinity: {
    name: `infinity`,
    category: `Math`,
    linkName: `infinity`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing positive infinity.`,
    examples: [`(infinity)`],
  },
  '-infinity': {
    name: `-infinity`,
    category: `Math`,
    linkName: `-infinity`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing negative infinity.`,
    examples: [`(-infinity)`],
  },
  'max-safe-integer': {
    name: `max-safe-integer`,
    category: `Math`,
    linkName: `max-safe-integer`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing the maximum safe integer.`,
    examples: [`(max-safe-integer)`],
  },
  'min-safe-integer': {
    name: `min-safe-integer`,
    category: `Math`,
    linkName: `min-safe-integer`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing the minimum safe integer.`,
    examples: [`(min-safe-integer)`],
  },
  'max-value': {
    name: `max-value`,
    category: `Math`,
    linkName: `max-value`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing the maximum numeric value.`,
    examples: [`(max-value)`],
  },
  'min-value': {
    name: `min-value`,
    category: `Math`,
    linkName: `min-value`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing the smallest positive numeric value.`,
    examples: [`(min-value)`],
  },
  epsilon: {
    name: `epsilon`,
    category: `Math`,
    linkName: `epsilon`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing the difference between 1 and the smallest floating point number greater than 1.`,
    examples: [`(epsilon)`],
  },
  nan: {
    name: `nan`,
    category: `Math`,
    linkName: `nan`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns a number representing Not-A-Number.`,
    examples: [`(nan)`],
  },
  e: {
    name: `e`,
    category: `Math`,
    linkName: `e`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns Euler's number, the base of natural logarithms, e.`,
    examples: [`(e)`],
  },
  pi: {
    name: `pi`,
    category: `Math`,
    linkName: `pi`,
    returns: {
      type: `number`,
    },
    arguments: [],
    description: `Returns Pi, the ratio of the circumference of a circle to its diameter.`,
    examples: [`(pi)`],
  },
  log: {
    name: `log`,
    category: `Math`,
    linkName: `log`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the natural logarithm (base e) of \`number\`.`,
    examples: [`(log 0.01)`, `(log (exp 12))`, `(log 2.5)`],
  },
  log2: {
    name: `log2`,
    category: `Math`,
    linkName: `log2`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the base 2 logarithm of a number.`,
    examples: [`(log2 0.01)`, `(log2 (pow 2 12))`, `(log2 2.5)`],
  },
  log10: {
    name: `log10`,
    category: `Math`,
    linkName: `log10`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    description: `Returns the base 2 logarithm of a number.`,
    examples: [`(log10 0.01)`, `(log10 (pow 10 12))`, `(log10 2.5)`],
  },
  'rand!': {
    name: `rand!`,
    category: `Math`,
    linkName: `rand_exclamation`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `positive number`,
        description: `optional`,
      },
    ],
    description: `Returns a semi random number between \`0\` (inclusive) and \`number\` (default 1) (exclusive).`,
    examples: [`(rand! 1)`, `(rand! 0.01)`, `(rand! 2.5)`],
  },
  'rand-int!': {
    name: `rand-int!`,
    category: `Math`,
    linkName: `rand-int_exclamation`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `positive number`,
        description: `optional`,
      },
    ],
    description: `Returns a semi random integer between \`0\` (inclusive) and \`number\` (exclusive).`,
    examples: [`(rand-int! 1)`, `(rand-int! 10.12)`, `(rand-int! 123)`],
  },
  sin: {
    name: `sin`,
    category: `Math`,
    linkName: `sin`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `angle`,
        type: `number`,
      },
    ],
    description: `Returns the sine of \`angle\`. \`angle\` must be specified in radians.`,
    examples: [`(sin 0)`, `(sin 1)`, `(sin (pi))`, `(sin -0.5)`],
  },
  cos: {
    name: `cos`,
    category: `Math`,
    linkName: `cos`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `angle`,
        type: `number`,
      },
    ],
    description: `Returns the cosine of \`angle\`. \`angle\` must be specified in radians.`,
    examples: [`(cos 0)`, `(cos 1)`, `(cos (pi))`, `(cos -0.5)`],
  },
  tan: {
    name: `tan`,
    category: `Math`,
    linkName: `tan`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `angle`,
        type: `number`,
      },
    ],
    description: `Returns the tangent of \`angle\`. \`angle\` must be specified in radians.`,
    examples: [`(tan 0)`, `(tan 1)`, `(tan (pi))`, `(tan -0.5)`],
  },
  asin: {
    name: `asin`,
    category: `Math`,
    linkName: `asin`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the arcsine (in radians) of \`value\`.`,
    examples: [`(asin 0)`, `(asin 1)`, `(asin -0.5)`],
  },
  acos: {
    name: `acos`,
    category: `Math`,
    linkName: `acos`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the arccosine (in radians) of \`value\`.`,
    examples: [`(acos 0)`, `(acos 1)`, `(acos -0.5)`],
  },
  atan: {
    name: `atan`,
    category: `Math`,
    linkName: `atan`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the arctangent (in radians) of \`value\`.`,
    examples: [`(atan 0)`, `(atan 1)`, `(atan -0.5)`],
  },
  sinh: {
    name: `sinh`,
    category: `Math`,
    linkName: `sinh`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the hyperbolic sine of \`value\`.`,
    examples: [`(sinh 0)`, `(sinh 1)`, `(sinh -0.5)`],
  },
  cosh: {
    name: `cosh`,
    category: `Math`,
    linkName: `cosh`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the hyperbolic cosine of \`value\`.`,
    examples: [`(cosh 0)`, `(cosh 1)`, `(cosh -0.5)`],
  },
  tanh: {
    name: `tanh`,
    category: `Math`,
    linkName: `tanh`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the hyperbolic tangent of \`value\`.`,
    examples: [`(tanh 0)`, `(tanh 1)`, `(tanh -0.5)`, `(tanh 50)`],
  },
  asinh: {
    name: `asinh`,
    category: `Math`,
    linkName: `asinh`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the hyperbolic arcsine of \`value\`.`,
    examples: [`(asinh 0)`, `(asinh 0.9)`, `(asinh -0.5)`],
  },
  acosh: {
    name: `acosh`,
    category: `Math`,
    linkName: `acosh`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the hyperbolic arccosine of \`value\`.`,
    examples: [`(acosh 1)`, `(acosh 2)`, `(acosh 100)`],
  },
  atanh: {
    name: `atanh`,
    category: `Math`,
    linkName: `atanh`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `value`,
        type: `number`,
      },
    ],
    description: `Returns the hyperbolic arctangent of \`value\`.`,
    examples: [`(atanh 0)`, `(atanh 0.9)`, `(atanh -0.5)`],
  },
}
