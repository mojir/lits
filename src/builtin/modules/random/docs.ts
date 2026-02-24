import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'random!': {
    category: 'random',
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
      `let { random! } = import(random);
random!()`,
    ],
    seeAlso: ['random.random-float!', 'random.random-int!', 'random.random-boolean!'],
  },
  'random-int!': {
    category: 'random',
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
      `let { random-int! } = import(random);
random-int!(0, 10)`,
      `let { random-int! } = import(random);
random-int!(1, 100)`,
    ],
    seeAlso: ['random.random-int-inclusive!', 'random.random-float!', 'random.random!'],
  },
  'random-int-inclusive!': {
    category: 'random',
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
      `let { random-int-inclusive! } = import(random);
random-int-inclusive!(0, 10)`,
    ],
    seeAlso: ['random.random-int!', 'random.random-float!'],
  },
  'random-float!': {
    category: 'random',
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
      `let { random-float! } = import(random);
random-float!(0, 10)`,
      `let { random-float! } = import(random);
random-float!(1, 100)`,
    ],
    seeAlso: ['random.random!', 'random.random-int!', 'random.random-int-inclusive!'],
  },
  'random-boolean!': {
    category: 'random',
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
      `let { random-boolean! } = import(random);
random-boolean!()`,
      `let { random-boolean! } = import(random);
random-boolean!(0.99)`,
    ],
    seeAlso: ['random.random!'],
  },
  'random-item!': {
    category: 'random',
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
      `let { random-item! } = import(random);
random-item!([1, 2, 3, 4, 5])`,
      `let { random-item! } = import(random);
random-item!(["apple", "banana", "cherry"])`,
    ],
    seeAlso: ['random.random-sample!', 'random.random-sample-unique!', 'random.random-char!'],
  },
  'random-sample-unique!': {
    category: 'random',
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
      `let { random-sample-unique! } = import(random);
random-sample-unique!([1, 2, 3, 4, 5], 3)`,
      `let { random-sample-unique! } = import(random);
random-sample-unique!(["apple", "banana", "cherry"], 2)`,
    ],
    seeAlso: ['random.random-sample!', 'random.random-item!', 'random.shuffle!'],
  },
  'random-sample!': {
    category: 'random',
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
      `let { random-sample! } = import(random);
random-sample!([1, 2, 3, 4, 5], 3)`,
      `let { random-sample! } = import(random);
random-sample!(["apple", "banana", "cherry"], 10)`,
    ],
    seeAlso: ['random.random-sample-unique!', 'random.random-item!', 'random.shuffle!'],
  },
  'shuffle!': {
    category: 'random',
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
      `let { shuffle! } = import(random);
shuffle!([1, 2, 3, 4, 5])`,
      `let { shuffle! } = import(random);
shuffle!(["apple", "banana", "cherry"])`,
    ],
    seeAlso: ['random.random-sample!', 'random.random-sample-unique!'],
  },
  'random-normal!': {
    category: 'random',
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
      `let { random-normal! } = import(random);
random-normal!(0, 1)`,
      `let { random-normal! } = import(random);
random-normal!(5, 2)`,
    ],
    seeAlso: ['random.random-exponential!', 'random.random-binomial!', 'random.random-poisson!'],
    hideOperatorForm: true,
  },
  'random-exponential!': {
    category: 'random',
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
      `let { random-exponential! } = import(random);
random-exponential!(1)`,
      `let { random-exponential! } = import(random);
random-exponential!(0.5)`,
    ],
    seeAlso: ['random.random-normal!', 'random.random-poisson!', 'random.random-gamma!', 'random.random-pareto!'],
  },
  'random-binomial!': {
    category: 'random',
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
      `let { random-binomial! } = import(random);
random-binomial!(10, 0.5)`,
      `let { random-binomial! } = import(random);
random-binomial!(20, 0.3)`,
    ],
    seeAlso: ['random.random-normal!', 'random.random-poisson!'],
    hideOperatorForm: true,
  },
  'random-poisson!': {
    category: 'random',
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
      `let { random-poisson! } = import(random);
random-poisson!(1)`,
      `let { random-poisson! } = import(random);
random-poisson!(5)`,
    ],
    seeAlso: ['random.random-binomial!', 'random.random-normal!', 'random.random-exponential!'],
  },
  'random-gamma!': {
    category: 'random',
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
      `let { random-gamma! } = import(random);
random-gamma!(2, 2)`,
      `let { random-gamma! } = import(random);
random-gamma!(5, 1)`,
    ],
    seeAlso: ['random.random-exponential!', 'random.random-pareto!'],
    hideOperatorForm: true,
  },
  'random-pareto!': {
    category: 'random',
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
      `let { random-pareto! } = import(random);
random-pareto!(1)`,
      `let { random-pareto! } = import(random);
random-pareto!(2)`,
    ],
    seeAlso: ['random.random-gamma!', 'random.random-exponential!'],
  },
  'uuid!': {
    category: 'random',
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
      `let { uuid! } = import(random);
uuid!()`,
    ],
    seeAlso: ['random.random-id!', 'random.random-string!'],
  },
  'random-char!': {
    category: 'random',
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
      `let { random-char! } = import(random);
random-char!("abcde")`,
      `let { random-char! } = import(random);
random-char!("ABCDEFGHIJKLMNOPQRSTUVWXYZ")`,
    ],
    seeAlso: ['random.random-string!', 'random.random-item!'],
  },
  'random-string!': {
    category: 'random',
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
      `let { random-string! } = import(random);
random-string!(10, "abcde")`,
      `let { random-string! } = import(random);
random-string!(5, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")`,
    ],
    seeAlso: ['random.random-char!', 'random.random-id!', 'random.uuid!'],
    hideOperatorForm: true,
  },
  'random-id!': {
    category: 'random',
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
      `let { random-id! } = import(random);
random-id!(10)`,
      `let { random-id! } = import(random);
random-id!(5)`,
    ],
    seeAlso: ['random.random-string!', 'random.uuid!'],
  },
  'random-color!': {
    category: 'random',
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
      `let { random-color! } = import(random);
random-color!()`,
    ],
  },
}
