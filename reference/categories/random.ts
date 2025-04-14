import type { FunctionReference } from '..'
import type { RandomApiName } from '../api'

export const randomReference: Record<RandomApiName, FunctionReference<'Random'>> = {
  '!:random': {
    title: '!:random',
    category: 'Random',
    linkName: '-exclamation-colon-random',
    returns: {
      type: 'number',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns a random number between 0 and 1.',
    examples: [
      '!:random()',
    ],
  },
  '!:random-int': {
    title: '!:random-int',
    category: 'Random',
    linkName: '-exclamation-colon-random-int',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
        description: 'The minimum value.',
      },
      b: {
        type: 'integer',
        description: 'The maximum value (exclusive).',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns a random integer between min and max (exclusive).',
    examples: [
      '!:random-int(0, 10)',
      '!:random-int(1, 100)',
    ],
  },
  '!:random-int-inclusive': {
    title: '!:random-int-inclusive',
    category: 'Random',
    linkName: '-exclamation-colon-random-int-inclusive',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
        description: 'The minimum value.',
      },
      b: {
        type: 'integer',
        description: 'The maximum value (inclusive).',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns a random integer between min and max (inclusive).',
    examples: [
      '!:random-int-inclusive(0, 10)',
    ],
  },
  '!:random-float': {
    title: '!:random-float',
    category: 'Random',
    linkName: '-exclamation-colon-random-float',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'number',
        description: 'The minimum value.',
      },
      b: {
        type: 'number',
        description: 'The maximum value.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns a random float between min and max.',
    examples: [
      '!:random-float(0, 10)',
      '!:random-float(1, 100)',
    ],
  },
  '!:random-boolean': {
    title: '!:random-boolean',
    category: 'Random',
    linkName: '-exclamation-colon-random-boolean',
    returns: {
      type: 'boolean',
    },
    args: {
      prob: {
        type: 'number',
        description: 'The probability of returning true (between 0 and 1).',
      },
    },
    variants: [
      { argumentNames: ['prob'] },
    ],
    description: 'Returns a random boolean.',
    examples: [
      '!:random-boolean()',
      '!:random-boolean(0.99)',
    ],
  },
  '!:random-item': {
    title: '!:random-item',
    category: 'Random',
    linkName: '-exclamation-colon-random-item',
    returns: {
      type: 'any',
    },
    args: {
      a: {
        type: 'array',
        description: 'The array to sample from.',
      },
    },
    variants: [
      { argumentNames: ['a'] },
    ],
    description: 'Returns a random item from the array.',
    examples: [
      '!:random-item([1, 2, 3, 4, 5])',
      '!:random-item(["apple", "banana", "cherry"])',
    ],
  },
  '!:random-sample-unique': {
    title: '!:random-sample-unique',
    category: 'Random',
    linkName: '-exclamation-colon-random-sample-unique',
    returns: {
      type: 'array',
    },
    args: {
      a: {
        type: 'array',
        description: 'The array to sample from.',
      },
      b: {
        type: 'integer',
        description: 'The number of items to sample.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns a random sample of n unique items from the array.',
    examples: [
      '!:random-sample-unique([1, 2, 3, 4, 5], 3)',
      '!:random-sample-unique(["apple", "banana", "cherry"], 2)',
    ],
  },
  '!:random-sample': {
    title: '!:random-sample',
    category: 'Random',
    linkName: '-exclamation-colon-random-sample',
    returns: {
      type: 'array',
    },
    args: {
      a: {
        type: 'array',
        description: 'The array to sample from.',
      },
      b: {
        type: 'integer',
        description: 'The number of items to sample.',
      },
    },
    variants: [
      { argumentNames: ['a', 'b'] },
    ],
    description: 'Returns a random sample of n items from the array.',
    examples: [
      '!:random-sample([1, 2, 3, 4, 5], 3)',
      '!:random-sample(["apple", "banana", "cherry"], 10)',
    ],
  },
  '!:shuffle': {
    title: '!:shuffle',
    category: 'Random',
    linkName: '-exclamation-colon-shuffle',
    returns: {
      type: 'array',
    },
    args: {
      a: {
        type: 'array',
        description: 'The array to shuffle.',
      },
    },
    variants: [
      { argumentNames: ['a'] },
    ],
    description: 'Returns a shuffled version of the array.',
    examples: [
      '!:shuffle([1, 2, 3, 4, 5])',
      '!:shuffle(["apple", "banana", "cherry"])',
    ],
  },
  '!:random-normal': {
    title: '!:random-normal',
    category: 'Random',
    linkName: '-exclamation-colon-random-normal',
    returns: {
      type: 'number',
    },
    args: {
      mean: {
        type: 'number',
        description: 'The mean of the normal distribution.',
      },
      stdDev: {
        type: 'number',
        description: 'The standard deviation of the normal distribution.',
      },
    },
    variants: [
      { argumentNames: ['mean', 'stdDev'] },
    ],
    description: 'Returns a random number from a normal distribution with the given mean and standard deviation.',
    examples: [
      '!:random-normal(0, 1)',
      '!:random-normal(5, 2)',
    ],
    noOperatorDocumentation: true,
  },
  '!:random-exponential': {
    title: '!:random-exponential',
    category: 'Random',
    linkName: '-exclamation-colon-random-exponential',
    returns: {
      type: 'number',
    },
    args: {
      lambda: {
        type: 'number',
        description: 'The rate parameter of the exponential distribution.',
      },
    },
    variants: [
      { argumentNames: ['lambda'] },
    ],
    description: 'Returns a random number from an exponential distribution with the given rate parameter.',
    examples: [
      '!:random-exponential(1)',
      '!:random-exponential(0.5)',
    ],
  },
  '!:random-binomial': {
    title: '!:random-binomial',
    category: 'Random',
    linkName: '-exclamation-colon-random-binomial',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number of trials.',
      },
      p: {
        type: 'number',
        description: 'The probability of success on each trial.',
      },
    },
    variants: [
      { argumentNames: ['n', 'p'] },
    ],
    description: 'Returns a random number from a binomial distribution with the given number of trials and probability of success.',
    examples: [
      '!:random-binomial(10, 0.5)',
      '!:random-binomial(20, 0.3)',
    ],
    noOperatorDocumentation: true,
  },
  '!:random-poisson': {
    title: '!:random-poisson',
    category: 'Random',
    linkName: '-exclamation-colon-random-poisson',
    returns: {
      type: 'integer',
    },
    args: {
      lambda: {
        type: 'number',
        description: 'The rate parameter of the Poisson distribution.',
      },
    },
    variants: [
      { argumentNames: ['lambda'] },
    ],
    description: 'Returns a random number from a Poisson distribution with the given rate parameter.',
    examples: [
      '!:random-poisson(1)',
      '!:random-poisson(5)',
    ],
  },
  '!:random-gamma': {
    title: '!:random-gamma',
    category: 'Random',
    linkName: '-exclamation-colon-random-gamma',
    returns: {
      type: 'number',
    },
    args: {
      shape: {
        type: 'number',
        description: 'The shape parameter of the gamma distribution.',
      },
      scale: {
        type: 'number',
        description: 'The scale parameter of the gamma distribution.',
      },
    },
    variants: [
      { argumentNames: ['shape', 'scale'] },
    ],
    description: 'Returns a random number from a gamma distribution with the given shape and scale parameters.',
    examples: [
      '!:random-gamma(2, 2)',
      '!:random-gamma(5, 1)',
    ],
    noOperatorDocumentation: true,
  },
  '!:random-pareto': {
    title: '!:random-pareto',
    category: 'Random',
    linkName: '-exclamation-colon-random-pareto',
    returns: {
      type: 'number',
    },
    args: {
      alpha: {
        type: 'number',
        description: 'The shape parameter of the Pareto distribution.',
      },
    },
    variants: [
      { argumentNames: ['alpha'] },
    ],
    description: 'Returns a random number from a Pareto distribution with the given shape parameter.',
    examples: [
      '!:random-pareto(1)',
      '!:random-pareto(2)',
    ],
  },
  '!:uuid': {
    title: '!:uuid',
    category: 'Random',
    linkName: '-exclamation-colon-uuid',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns a random UUID v4 (Universally Unique Identifier).',
    examples: [
      '!:uuid()',
    ],
  },
  '!:random-char': {
    title: '!:random-char',
    category: 'Random',
    linkName: '-exclamation-colon-random-char',
    returns: {
      type: 'string',
    },
    args: {
      charSet: {
        type: 'string',
        description: 'The string to sample from.',
      },
    },
    variants: [
      { argumentNames: ['charSet'] },
    ],
    description: 'Returns a random character from the given string.',
    examples: [
      '!:random-char("abcde")',
      '!:random-char("ABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ],
  },
  '!:random-string': {
    title: '!:random-string',
    category: 'Random',
    linkName: '-exclamation-colon-random-string',
    returns: {
      type: 'string',
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the random string.',
      },
      charSet: {
        type: 'string',
        description: 'The string to sample from.',
      },
    },
    variants: [
      { argumentNames: ['length', 'charSet'] },
    ],
    description: 'Returns a random string of the given length from the given string.',
    examples: [
      '!:random-string(10, "abcde")',
      '!:random-string(5, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ],
    noOperatorDocumentation: true,
  },
  '!:random-id': {
    title: '!:random-id',
    category: 'Random',
    linkName: '-exclamation-colon-random-id',
    returns: {
      type: 'string',
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the random ID.',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    description: 'Returns a random ID of the given length.',
    examples: [
      '!:random-id(10)',
      '!:random-id(5)',
    ],
  },
  '!:random-color': {
    title: '!:random-color',
    category: 'Random',
    linkName: '-exclamation-colon-random-color',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns a random color in hex format.',
    examples: [
      '!:random-color()',
    ],
  },
}
