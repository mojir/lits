import type { FunctionReference } from '..'
import type { RandomApiName } from '../api'

export const randomReference: Record<RandomApiName, FunctionReference<'Random'>> = {
  'Random.random!': {
    title: 'Random.random!',
    category: 'Random',
    returns: {
      type: 'number',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns a random number between 0 and 1.',
    examples: [
      'let { random! } = import("Random"); random!()',
    ],
  },
  'Random.random-int!': {
    title: 'Random.random-int!',
    category: 'Random',
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
      'let { random-int! } = import("Random"); random-int!(0, 10)',
      'let { random-int! } = import("Random"); random-int!(1, 100)',
    ],
  },
  'Random.random-int-inclusive!': {
    title: 'Random.random-int-inclusive!',
    category: 'Random',
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
      'let { random-int-inclusive! } = import("Random"); random-int-inclusive!(0, 10)',
    ],
  },
  'Random.random-float!': {
    title: 'Random.random-float!',
    category: 'Random',
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
      'let { random-float! } = import("Random"); random-float!(0, 10)',
      'let { random-float! } = import("Random"); random-float!(1, 100)',
    ],
  },
  'Random.random-boolean!': {
    title: 'Random.random-boolean!',
    category: 'Random',
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
      'let { random-boolean! } = import("Random"); random-boolean!()',
      'let { random-boolean! } = import("Random"); random-boolean!(0.99)',
    ],
  },
  'Random.random-item!': {
    title: 'Random.random-item!',
    category: 'Random',
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
      'let { random-item! } = import("Random"); random-item!([1, 2, 3, 4, 5])',
      'let { random-item! } = import("Random"); random-item!(["apple", "banana", "cherry"])',
    ],
  },
  'Random.random-sample-unique!': {
    title: 'Random.random-sample-unique!',
    category: 'Random',
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
      'let { random-sample-unique! } = import("Random"); random-sample-unique!([1, 2, 3, 4, 5], 3)',
      'let { random-sample-unique! } = import("Random"); random-sample-unique!(["apple", "banana", "cherry"], 2)',
    ],
  },
  'Random.random-sample!': {
    title: 'Random.random-sample!',
    category: 'Random',
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
      'let { random-sample! } = import("Random"); random-sample!([1, 2, 3, 4, 5], 3)',
      'let { random-sample! } = import("Random"); random-sample!(["apple", "banana", "cherry"], 10)',
    ],
  },
  'Random.shuffle!': {
    title: 'Random.shuffle!',
    category: 'Random',
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
      'let { shuffle! } = import("Random"); shuffle!([1, 2, 3, 4, 5])',
      'let { shuffle! } = import("Random"); shuffle!(["apple", "banana", "cherry"])',
    ],
  },
  'Random.random-normal!': {
    title: 'Random.random-normal!',
    category: 'Random',
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
      'let { random-normal! } = import("Random"); random-normal!(0, 1)',
      'let { random-normal! } = import("Random"); random-normal!(5, 2)',
    ],
    noOperatorDocumentation: true,
  },
  'Random.random-exponential!': {
    title: 'Random.random-exponential!',
    category: 'Random',
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
      'let { random-exponential! } = import("Random"); random-exponential!(1)',
      'let { random-exponential! } = import("Random"); random-exponential!(0.5)',
    ],
  },
  'Random.random-binomial!': {
    title: 'Random.random-binomial!',
    category: 'Random',
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
      'let { random-binomial! } = import("Random"); random-binomial!(10, 0.5)',
      'let { random-binomial! } = import("Random"); random-binomial!(20, 0.3)',
    ],
    noOperatorDocumentation: true,
  },
  'Random.random-poisson!': {
    title: 'Random.random-poisson!',
    category: 'Random',
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
      'let { random-poisson! } = import("Random"); random-poisson!(1)',
      'let { random-poisson! } = import("Random"); random-poisson!(5)',
    ],
  },
  'Random.random-gamma!': {
    title: 'Random.random-gamma!',
    category: 'Random',
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
      'let { random-gamma! } = import("Random"); random-gamma!(2, 2)',
      'let { random-gamma! } = import("Random"); random-gamma!(5, 1)',
    ],
    noOperatorDocumentation: true,
  },
  'Random.random-pareto!': {
    title: 'Random.random-pareto!',
    category: 'Random',
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
      'let { random-pareto! } = import("Random"); random-pareto!(1)',
      'let { random-pareto! } = import("Random"); random-pareto!(2)',
    ],
  },
  'Random.uuid!': {
    title: 'Random.uuid!',
    category: 'Random',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns a random UUID v4 (Universally Unique Identifier).',
    examples: [
      'let { uuid! } = import("Random"); uuid!()',
    ],
  },
  'Random.random-char!': {
    title: 'Random.random-char!',
    category: 'Random',
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
      'let { random-char! } = import("Random"); random-char!("abcde")',
      'let { random-char! } = import("Random"); random-char!("ABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ],
  },
  'Random.random-string!': {
    title: 'Random.random-string!',
    category: 'Random',
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
      'let { random-string! } = import("Random"); random-string!(10, "abcde")',
      'let { random-string! } = import("Random"); random-string!(5, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ],
    noOperatorDocumentation: true,
  },
  'Random.random-id!': {
    title: 'Random.random-id!',
    category: 'Random',
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
      'let { random-id! } = import("Random"); random-id!(10)',
      'let { random-id! } = import("Random"); random-id!(5)',
    ],
  },
  'Random.random-color!': {
    title: 'Random.random-color!',
    category: 'Random',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      { argumentNames: [] },
    ],
    description: 'Returns a random color in hex format.',
    examples: [
      'let { random-color! } = import("Random"); random-color!()',
    ],
  },
}
