import type { NumberTheorySequenceReference } from '.'

export const fibonacciReference: NumberTheorySequenceReference<'fibonacci'> = {
  'nth:fibonacci-seq': {
    title: 'nth:fibonacci-seq',
    category: 'Number Theory',
    description: 'Generates the fibonacci sequence up to a specified length.',
    linkName: 'nth-colon-fibonacci-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 79 (the maximum length of the pre-calculated Fibonacci numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'nth:fibonacci-seq(1)',
      'nth:fibonacci-seq(2)',
      'nth:fibonacci-seq()',
    ],
  },
  'nth:fibonacci-take-while': {
    title: 'nth:fibonacci-take-while',
    category: 'Number Theory',
    description: 'Generates the fibonacci sequence while a condition is met.',
    linkName: 'nth-colon-fibonacci-take-while',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['takeWhile'] },
    ],
    examples: [
      'nth:fibonacci-take-while(-> $ < 100)',
    ],
  },
  'nth:fibonacci-nth': {
    title: 'nth:fibonacci-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the fibonacci sequence.',
    linkName: 'nth-colon-fibonacci-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'nth:fibonacci-nth(5)',
      'nth:fibonacci-nth(50)',
    ],
  },
  'nth:fibonacci?': {
    title: 'nth:fibonacci?',
    category: 'Number Theory',
    description: 'Determines if a number is in the fibonacci sequence.',
    linkName: 'nth-colon-fibonacci-question',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'nth:fibonacci?(0)',
      'nth:fibonacci?(1)',
      'nth:fibonacci?(2)',
      'nth:fibonacci?(3)',
      'nth:fibonacci?(4)',
      'nth:fibonacci?(5)',
      'nth:fibonacci?(6)',
      'nth:fibonacci?(7)',
      'nth:fibonacci?(8)',
      'nth:fibonacci?(9)',
    ],
  },
}
