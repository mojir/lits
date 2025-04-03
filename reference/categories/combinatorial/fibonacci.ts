import type { CombinatorialSequenceReference } from '.'

export const fibonacciReference: CombinatorialSequenceReference<'fibonacci'> = {
  'c:fibonacci-seq': {
    title: 'c:fibonacci-seq',
    category: 'Combinatorial',
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
      'c:fibonacci-seq(1)',
      'c:fibonacci-seq(2)',
      'c:fibonacci-seq()',
    ],
  },
  'c:fibonacci-take-while': {
    title: 'c:fibonacci-take-while',
    category: 'Combinatorial',
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
      'c:fibonacci-take-while(-> $ < 100)',
    ],
  },
  'c:fibonacci-nth': {
    title: 'c:fibonacci-nth',
    category: 'Combinatorial',
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
      'c:fibonacci-nth(5)',
      'c:fibonacci-nth(50)',
    ],
  },
  'c:fibonacci?': {
    title: 'c:fibonacci?',
    category: 'Combinatorial',
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
      'c:fibonacci?(0)',
      'c:fibonacci?(1)',
      'c:fibonacci?(2)',
      'c:fibonacci?(3)',
      'c:fibonacci?(4)',
      'c:fibonacci?(5)',
      'c:fibonacci?(6)',
      'c:fibonacci?(7)',
      'c:fibonacci?(8)',
      'c:fibonacci?(9)',
    ],
  },
}
