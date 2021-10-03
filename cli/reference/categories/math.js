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
    shortDescription: `Computes sum of \`numbers\`.`,
    longDescription: `Computes sum of \`numbers\`.`,
    examples: [`(+)`, `(+ 1)`, `(+ 2 4)`, `(+ 1 2 3 4)`, `(+ (+ 2 3) (+ 5 6))`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes difference between first value and sum of the rest.`,
    longDescription: `Computes difference between first value and sum of the rest. When called with only one argument, it does negation.`,
    examples: [`(-)`, `(- 1)`, `(- 2 4)`, `(- 4 3 2 1)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes product of \`numbers\`.`,
    longDescription: `Computes product of \`numbers\`.`,
    examples: [`(*)`, `(* 2)`, `(* 2 4)`, `(* 1 2 3 4)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes division or reciprocal.`,
    longDescription: `Computes division or reciprocal. When called with one argument it computes reciprocal. When called with two or more arguments it does compute division of the first by the all remaining \`numbers\`.`,
    examples: [`(/)`, `(/ 2)`, `(/ 2 4)`, `(/ 4 3 2 1)`],
    specialExpression: false,
    sideEffects: [],
  },
  '%': {
    name: `%`,
    category: `Math`,
    linkName: `_percent`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
      {
        name: `divisor`,
        type: `number`,
      },
    ],
    shortDescription: `Returns modulus of two number arguments.`,
    longDescription: `Returns modulus of two number arguments.`,
    examples: [`(% 5 3)`, `(% 5.2 3.1)`, `(% -5 3)`, `(% 5 -3)`, `(% -5 -3)`],
    specialExpression: false,
    sideEffects: [],
  },
  '1+': {
    name: `1+`,
    category: `Math`,
    linkName: `1_plus`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    shortDescription: `Adds one to \`number\`.`,
    longDescription: `Adds one to \`number\`.`,
    examples: [`(1+ 0)`, `(1+ 1)`, `(1+ 100.1)`],
    specialExpression: false,
    sideEffects: [],
  },
  '1-': {
    name: `1-`,
    category: `Math`,
    linkName: `1_minus`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    shortDescription: `Subtracts one from \`number\`.`,
    longDescription: `Subtracts one from \`number\`.`,
    examples: [`(1- 0)`, `(1- 1)`, `(1- 100.1)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes square root of \`number\`.`,
    longDescription: `Computes square root of \`number\`.`,
    examples: [`(sqrt 0)`, `(sqrt 9)`, `(sqrt 2)`, `(sqrt -1)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes cube root of \`number\`.`,
    longDescription: `Computes cube root of \`number\`.`,
    examples: [`(cbrt 0)`, `(cbrt 27)`, `(cbrt 2)`, `(cbrt -1)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes returns \`base-number\` raised to the \`power-number\`.`,
    longDescription: `Computes returns \`base-number\` raised to the \`power-number\`.`,
    examples: [`(pow 2 3)`, `(pow 2 0)`, `(pow 2 -3)`, `(pow -2 3)`, `(pow -2 -3)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Computes \`e\` rasied to the \`power-number\`.`,
    longDescription: `Computes \`e\` rasied to the \`power-number\`.`,
    examples: [`(exp 3)`, `(exp 0)`, `(exp -3)`, `(exp 3)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns rounded \`number\`.`,
    longDescription: `Returns rounded \`number\`. If \`decimals\` is provided it return a number with that many decimals.`,
    examples: [
      `(round 2)`,
      `(round 2.49)`,
      `(round 2.5)`,
      `(round -2.49)`,
      `(round -2.5)`,
      `(round -2.501)`,
      `(round 1.23456789 4)`,
    ],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the integer part of \`number\` by removing any fractional digits.`,
    longDescription: `Returns the integer part of \`number\` by removing any fractional digits.`,
    examples: [`(trunc 2)`, `(trunc 2.49)`, `(trunc 2.5)`, `(trunc -2.49)`, `(trunc -2.5)`, `(trunc -2.501)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the largest \`integer\` less than or equal to \`number\`.`,
    longDescription: `Returns the largest \`integer\` less than or equal to \`number\`.`,
    examples: [`(floor 2)`, `(floor 2.49)`, `(floor 2.5)`, `(floor -2.49)`, `(floor -2.5)`, `(floor -2.501)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the smallest \`integer\` larger than or equal to \`number\`.`,
    longDescription: `Returns the smallest \`integer\` larger than or equal to \`number\`.`,
    examples: [`(ceil 2)`, `(ceil 2.49)`, `(ceil 2.5)`, `(ceil -2.49)`, `(ceil -2.5)`, `(ceil -2.501)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the smallest number of the arguments.`,
    longDescription: `Returns the smallest number of the arguments.`,
    examples: [`(min 2 0 1)`, `(min 2 -1 1)`, `(min 2.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the largest number of the arguments.`,
    longDescription: `Returns the largest number of the arguments.`,
    examples: [`(max 2 0 1)`, `(max 2 -1 1)`, `(max 2.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the absolute value of \`number\`.`,
    longDescription: `Returns the absolute value of \`number\`.`,
    examples: [`(abs -2.3)`, `(abs 0)`, `(abs 2.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns \`1\` if \`number > 0\`, \`-1\` if \`number < 0\`, \`0\` if \`number = 0\` or \`-0\` if \`number = -0\`.`,
    longDescription: `Returns \`1\` if \`number > 0\`, \`-1\` if \`number < 0\`, \`0\` if \`number = 0\` or \`-0\` if \`number = -0\`.`,
    examples: [`(sign -2.3)`, `(sign -0)`, `(sign 0)`, `(sign 12312)`],
    specialExpression: false,
    sideEffects: [],
  },
  e: {
    name: `e`,
    category: `Math`,
    linkName: `e`,
    returns: {
      type: `number`,
    },
    arguments: [],
    shortDescription: `Returns Euler's number.`,
    longDescription: `Returns Euler's number, the base of natural logarithms, e.`,
    examples: [`(e)`],
    specialExpression: false,
    sideEffects: [],
  },
  pi: {
    name: `pi`,
    category: `Math`,
    linkName: `pi`,
    returns: {
      type: `number`,
    },
    arguments: [],
    shortDescription: `Returns Pi.`,
    longDescription: `Returns Pi, the ratio of the circumference of a circle to its diameter.`,
    examples: [`(pi)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the natural logarithm (base e) of \`number\`.`,
    longDescription: `Returns the natural logarithm (base e) of \`number\`.`,
    examples: [`(log 0.01)`, `(log (exp 12))`, `(log 2.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the base 2 logarithm of a number.`,
    longDescription: `Returns the base 2 logarithm of a number.`,
    examples: [`(log2 0.01)`, `(log2 (pow 2 12))`, `(log2 2.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the base 2 logarithm of a number.`,
    longDescription: `Returns the base 2 logarithm of a number.`,
    examples: [`(log10 0.01)`, `(log10 (pow 10 12))`, `(log10 2.5)`],
    specialExpression: false,
    sideEffects: [],
  },
  random: {
    name: `random`,
    category: `Math`,
    linkName: `random`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `number`,
        type: `positive number`,
      },
    ],
    shortDescription: `Returns a semi random number between \`0\` (inclusive) and \`number\` (exclusive).`,
    longDescription: `Returns a semi random number between \`0\` (inclusive) and \`number\` (exclusive).`,
    examples: [`(random 1)`, `(random 0.01)`, `(random 2.5)`, `(random 0)`, `(random -1)`],
    specialExpression: false,
    sideEffects: [],
  },
  '<': {
    name: `<`,
    category: `Math`,
    linkName: `_lt`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    shortDescription: `Compares \`numbers\` according to "less than" predicate.`,
    longDescription: `Compares \`numbers\` according to "less than" predicate. Each (overlapping) pair of the \`numbers\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(< 0 1)`, `(< 1 1.01)`, `(< 1 1)`, `(< 1 2 3 4)`, `(< 1 2 2 3)`],
    specialExpression: false,
    sideEffects: [],
  },
  '>': {
    name: `>`,
    category: `Math`,
    linkName: `_gt`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    shortDescription: `Compares \`numbers\` according to "greater than" predicate.`,
    longDescription: `Compares \`numbers\` according to "greater than" predicate. Each (overlapping) pair of the \`numbers\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(> 1 0)`, `(> 1.01 1)`, `(> 1 1)`, `(> 4 3 2 1)`, `(> 3 2 2 1)`],
    specialExpression: false,
    sideEffects: [],
  },
  '<=': {
    name: `<=`,
    category: `Math`,
    linkName: `_lte`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    shortDescription: `Compares \`numbers\` according to "less than or equal" predicate.`,
    longDescription: `Compares \`numbers\` according to "less than or equal" predicate. Each (overlapping) pair of the \`numbers\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(<= 0 1)`, `(<= 1 1.01)`, `(<= 1 1)`, `(<= 1 2 3 4)`, `(<= 1 2 2 3)`],
    specialExpression: false,
    sideEffects: [],
  },
  '>=': {
    name: `>=`,
    category: `Math`,
    linkName: `_gte`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `number`,
        type: `number`,
      },
    ],
    shortDescription: `Compares \`numbers\` according to "greater than or equal" predicate.`,
    longDescription: `Compares \`numbers\` according to "greater than or equal" predicate. Each (overlapping) pair of the \`numbers\` is compared by it. The result is \`true\` if all compared pairs satisfy comparison.`,
    examples: [`(>= 1 0)`, `(>= 1.01 1)`, `(>= 1 1)`, `(>= 4 3 2 1)`, `(>= 3 2 2 1)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the sine of \`angle\`.`,
    longDescription: `Returns the sine of \`angle\`. \`angle\` must be specified in radians.`,
    examples: [`(sin 0)`, `(sin 1)`, `(sin (pi))`, `(sin -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the cosine of \`angle\`.`,
    longDescription: `Returns the cosine of \`angle\`. \`angle\` must be specified in radians.`,
    examples: [`(cos 0)`, `(cos 1)`, `(cos (pi))`, `(cos -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the tangent of \`angle\`.`,
    longDescription: `Returns the tangent of \`angle\`. \`angle\` must be specified in radians.`,
    examples: [`(tan 0)`, `(tan 1)`, `(tan (pi))`, `(tan -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the arcsine (in radians) of \`value\`.`,
    longDescription: `Returns the arcsine (in radians) of \`value\`.`,
    examples: [`(asin 0)`, `(asin 1)`, `(asin -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the arccosine (in radians) of \`value\`.`,
    longDescription: `Returns the arccosine (in radians) of \`value\`.`,
    examples: [`(acos 0)`, `(acos 1)`, `(acos -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the arctangent (in radians) of \`value\`.`,
    longDescription: `Returns the arctangent (in radians) of \`value\`.`,
    examples: [`(atan 0)`, `(atan 1)`, `(atan -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the hyperbolic sine of \`value\`.`,
    longDescription: `Returns the hyperbolic sine of \`value\`.`,
    examples: [`(sinh 0)`, `(sinh 1)`, `(sinh -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the hyperbolic cosine of \`value\`.`,
    longDescription: `Returns the hyperbolic cosine of \`value\`.`,
    examples: [`(cosh 0)`, `(cosh 1)`, `(cosh -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the hyperbolic tangent of \`value\`.`,
    longDescription: `Returns the hyperbolic tangent of \`value\`.`,
    examples: [`(tanh 0)`, `(tanh 1)`, `(tanh -0.5)`, `(tanh 50)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the hyperbolic arcsine of \`value\`.`,
    longDescription: `Returns the hyperbolic arcsine of \`value\`.`,
    examples: [`(asinh 0)`, `(asinh 0.9)`, `(asinh -0.5)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the hyperbolic arccosine of \`value\`.`,
    longDescription: `Returns the hyperbolic arccosine of \`value\`.`,
    examples: [`(acosh 1)`, `(acosh 2)`, `(acosh 100)`],
    specialExpression: false,
    sideEffects: [],
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
    shortDescription: `Returns the hyperbolic arctangent of \`value\`.`,
    longDescription: `Returns the hyperbolic arctangent of \`value\`.`,
    examples: [`(atanh 0)`, `(atanh 0.9)`, `(atanh -0.5)`],
    specialExpression: false,
    sideEffects: [],
  },
}
