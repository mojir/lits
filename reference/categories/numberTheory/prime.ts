import type { NumberTheorySequenceReference } from '.'

export const primeReference: NumberTheorySequenceReference<'prime'> = {
  'nth:prime-seq': {
    title: 'nth:prime-seq',
    category: 'Number Theory',
    description: 'Generates the prime sequence up to a specified length.',
    linkName: 'nth-colon-prime-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'nth:prime-seq(1)',
      'nth:prime-seq(2)',
      'nth:prime-seq(10)',
    ],
  },
  'nth:prime-take-while': {
    title: 'nth:prime-take-while',
    category: 'Number Theory',
    description: 'Generates the prime sequence while a condition is met.',
    linkName: 'nth-colon-prime-take-while',
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
      'nth:prime-take-while(-> $ < 50)',
    ],
  },
  'nth:prime-nth': {
    title: 'nth:prime-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the prime sequence.',
    linkName: 'nth-colon-prime-nth',
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
      'nth:prime-nth(1)',
      'nth:prime-nth(2)',
      'nth:prime-nth(10)',
    ],
  },
  'nth:prime?': {
    title: 'nth:prime?',
    category: 'Number Theory',
    description: 'Determines if a number is prime.',
    linkName: 'nth-colon-prime-question',
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
      'nth:prime?(1)',
      'nth:prime?(2)',
      'nth:prime?(3)',
      'nth:prime?(4)',
      'nth:prime?(997)',
      'nth:prime?(1001)',
    ],
  },
}
