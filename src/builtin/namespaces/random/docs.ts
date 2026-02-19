import type { FunctionDocs } from '../../interface'

export const namespaceDocs: Record<string, FunctionDocs> = {
  'random!': {
    category: 'Random',
    description: 'Returns a random number between 0 and 1.',
    returns: {
      type: 'number',
    },
    args: {},
    variants: [
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { random! } = import("Random"); random!()',
    ],
    seeAlso: ['Random.random-float!', 'Random.random-int!', 'Random.random-boolean!'],
  },
  'random-int!': {
    category: 'Random',
    description: 'Returns a random integer between min and max (exclusive).',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { random-int! } = import("Random"); random-int!(0, 10)',
      'let { random-int! } = import("Random"); random-int!(1, 100)',
    ],
    seeAlso: ['Random.random-int-inclusive!', 'Random.random-float!', 'Random.random!'],
  },
  'random-int-inclusive!': {
    category: 'Random',
    description: 'Returns a random integer between min and max (inclusive).',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { random-int-inclusive! } = import("Random"); random-int-inclusive!(0, 10)',
    ],
    seeAlso: ['Random.random-int!', 'Random.random-float!'],
  },
  'random-float!': {
    category: 'Random',
    description: 'Returns a random float between min and max.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { random-float! } = import("Random"); random-float!(0, 10)',
      'let { random-float! } = import("Random"); random-float!(1, 100)',
    ],
    seeAlso: ['Random.random!', 'Random.random-int!', 'Random.random-int-inclusive!'],
  },
  'random-boolean!': {
    category: 'Random',
    description: 'Returns a random boolean.',
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
      {
        argumentNames: [
          'prob',
        ],
      },
    ],
    examples: [
      'let { random-boolean! } = import("Random"); random-boolean!()',
      'let { random-boolean! } = import("Random"); random-boolean!(0.99)',
    ],
    seeAlso: ['Random.random!'],
  },
  'random-item!': {
    category: 'Random',
    description: 'Returns a random item from the array.',
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
      {
        argumentNames: [
          'a',
        ],
      },
    ],
    examples: [
      'let { random-item! } = import("Random"); random-item!([1, 2, 3, 4, 5])',
      'let { random-item! } = import("Random"); random-item!(["apple", "banana", "cherry"])',
    ],
    seeAlso: ['Random.random-sample!', 'Random.random-sample-unique!', 'Random.random-char!'],
  },
  'random-sample-unique!': {
    category: 'Random',
    description: 'Returns a random sample of n unique items from the array.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { random-sample-unique! } = import("Random"); random-sample-unique!([1, 2, 3, 4, 5], 3)',
      'let { random-sample-unique! } = import("Random"); random-sample-unique!(["apple", "banana", "cherry"], 2)',
    ],
    seeAlso: ['Random.random-sample!', 'Random.random-item!', 'Random.shuffle!'],
  },
  'random-sample!': {
    category: 'Random',
    description: 'Returns a random sample of n items from the array.',
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
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { random-sample! } = import("Random"); random-sample!([1, 2, 3, 4, 5], 3)',
      'let { random-sample! } = import("Random"); random-sample!(["apple", "banana", "cherry"], 10)',
    ],
    seeAlso: ['Random.random-sample-unique!', 'Random.random-item!', 'Random.shuffle!'],
  },
  'shuffle!': {
    category: 'Random',
    description: 'Returns a shuffled version of the array.',
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
      {
        argumentNames: [
          'a',
        ],
      },
    ],
    examples: [
      'let { shuffle! } = import("Random"); shuffle!([1, 2, 3, 4, 5])',
      'let { shuffle! } = import("Random"); shuffle!(["apple", "banana", "cherry"])',
    ],
    seeAlso: ['Random.random-sample!', 'Random.random-sample-unique!'],
  },
  'random-normal!': {
    category: 'Random',
    description: 'Returns a random number from a normal distribution with the given mean and standard deviation.',
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
      {
        argumentNames: [
          'mean',
          'stdDev',
        ],
      },
    ],
    examples: [
      'let { random-normal! } = import("Random"); random-normal!(0, 1)',
      'let { random-normal! } = import("Random"); random-normal!(5, 2)',
    ],
    seeAlso: ['Random.random-exponential!', 'Random.random-binomial!', 'Random.random-poisson!'],
    hideOperatorForm: true,
  },
  'random-exponential!': {
    category: 'Random',
    description: 'Returns a random number from an exponential distribution with the given rate parameter.',
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
      {
        argumentNames: [
          'lambda',
        ],
      },
    ],
    examples: [
      'let { random-exponential! } = import("Random"); random-exponential!(1)',
      'let { random-exponential! } = import("Random"); random-exponential!(0.5)',
    ],
    seeAlso: ['Random.random-normal!', 'Random.random-poisson!', 'Random.random-gamma!', 'Random.random-pareto!'],
  },
  'random-binomial!': {
    category: 'Random',
    description: 'Returns a random number from a binomial distribution with the given number of trials and probability of success.',
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
      {
        argumentNames: [
          'n',
          'p',
        ],
      },
    ],
    examples: [
      'let { random-binomial! } = import("Random"); random-binomial!(10, 0.5)',
      'let { random-binomial! } = import("Random"); random-binomial!(20, 0.3)',
    ],
    seeAlso: ['Random.random-normal!', 'Random.random-poisson!'],
    hideOperatorForm: true,
  },
  'random-poisson!': {
    category: 'Random',
    description: 'Returns a random number from a Poisson distribution with the given rate parameter.',
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
      {
        argumentNames: [
          'lambda',
        ],
      },
    ],
    examples: [
      'let { random-poisson! } = import("Random"); random-poisson!(1)',
      'let { random-poisson! } = import("Random"); random-poisson!(5)',
    ],
    seeAlso: ['Random.random-binomial!', 'Random.random-normal!', 'Random.random-exponential!'],
  },
  'random-gamma!': {
    category: 'Random',
    description: 'Returns a random number from a gamma distribution with the given shape and scale parameters.',
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
      {
        argumentNames: [
          'shape',
          'scale',
        ],
      },
    ],
    examples: [
      'let { random-gamma! } = import("Random"); random-gamma!(2, 2)',
      'let { random-gamma! } = import("Random"); random-gamma!(5, 1)',
    ],
    seeAlso: ['Random.random-exponential!', 'Random.random-pareto!'],
    hideOperatorForm: true,
  },
  'random-pareto!': {
    category: 'Random',
    description: 'Returns a random number from a Pareto distribution with the given shape parameter.',
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
      {
        argumentNames: [
          'alpha',
        ],
      },
    ],
    examples: [
      'let { random-pareto! } = import("Random"); random-pareto!(1)',
      'let { random-pareto! } = import("Random"); random-pareto!(2)',
    ],
    seeAlso: ['Random.random-gamma!', 'Random.random-exponential!'],
  },
  'uuid!': {
    category: 'Random',
    description: 'Returns a random UUID v4 (Universally Unique Identifier).',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { uuid! } = import("Random"); uuid!()',
    ],
    seeAlso: ['Random.random-id!', 'Random.random-string!'],
  },
  'random-char!': {
    category: 'Random',
    description: 'Returns a random character from the given string.',
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
      {
        argumentNames: [
          'charSet',
        ],
      },
    ],
    examples: [
      'let { random-char! } = import("Random"); random-char!("abcde")',
      'let { random-char! } = import("Random"); random-char!("ABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ],
    seeAlso: ['Random.random-string!', 'Random.random-item!'],
  },
  'random-string!': {
    category: 'Random',
    description: 'Returns a random string of the given length from the given string.',
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
      {
        argumentNames: [
          'length',
          'charSet',
        ],
      },
    ],
    examples: [
      'let { random-string! } = import("Random"); random-string!(10, "abcde")',
      'let { random-string! } = import("Random"); random-string!(5, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ],
    seeAlso: ['Random.random-char!', 'Random.random-id!', 'Random.uuid!'],
    hideOperatorForm: true,
  },
  'random-id!': {
    category: 'Random',
    description: 'Returns a random ID of the given length.',
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
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { random-id! } = import("Random"); random-id!(10)',
      'let { random-id! } = import("Random"); random-id!(5)',
    ],
    seeAlso: ['Random.random-string!', 'Random.uuid!'],
  },
  'random-color!': {
    category: 'Random',
    description: 'Returns a random color in hex format.',
    returns: {
      type: 'string',
    },
    args: {},
    variants: [
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { random-color! } = import("Random"); random-color!()',
    ],
  },
}
