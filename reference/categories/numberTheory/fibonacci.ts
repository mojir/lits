import type { NumberTheorySequenceReference } from '.'

export const fibonacciReference: NumberTheorySequenceReference<'fibonacci'> = {
  'n:fibonacci-seq': {
    title: 'n:fibonacci-seq',
    category: 'Number Theory',
    description: 'Generates the fibonacci sequence up to a specified length.',
    linkName: 'c-colon-fibonacci-seq',
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
      'n:fibonacci-seq(1)',
      'n:fibonacci-seq(2)',
      'n:fibonacci-seq()',
    ],
  },
  'n:fibonacci-take-while': {
    title: 'n:fibonacci-take-while',
    category: 'Number Theory',
    description: 'Generates the fibonacci sequence while a condition is met.',
    linkName: 'c-colon-fibonacci-take-while',
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
      'n:fibonacci-take-while(-> $ < 100)',
    ],
  },
  'n:fibonacci-nth': {
    title: 'n:fibonacci-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the fibonacci sequence.',
    linkName: 'c-colon-fibonacci-nth',
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
      'n:fibonacci-nth(5)',
      'n:fibonacci-nth(50)',
    ],
  },
  'n:fibonacci?': {
    title: 'n:fibonacci?',
    category: 'Number Theory',
    description: 'Determines if a number is in the fibonacci sequence.',
    linkName: 'c-colon-fibonacci-question-mark',
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
      'n:fibonacci?(0)',
      'n:fibonacci?(1)',
      'n:fibonacci?(2)',
      'n:fibonacci?(3)',
      'n:fibonacci?(4)',
      'n:fibonacci?(5)',
      'n:fibonacci?(6)',
      'n:fibonacci?(7)',
      'n:fibonacci?(8)',
      'n:fibonacci?(9)',
    ],
  },
}
