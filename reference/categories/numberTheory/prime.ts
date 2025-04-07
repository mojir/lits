import type { NumberTheorySequenceReference } from '.'

export const primeReference: NumberTheorySequenceReference<'prime'> = {
  'n:prime-seq': {
    title: 'n:prime-seq',
    category: 'Number Theory',
    description: 'Generates the prime sequence up to a specified length.',
    linkName: 'c-colon-prime-seq',
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
      'n:prime-seq(1)',
      'n:prime-seq(2)',
      'n:prime-seq(10)',
    ],
  },
  'n:prime-take-while': {
    title: 'n:prime-take-while',
    category: 'Number Theory',
    description: 'Generates the prime sequence while a condition is met.',
    linkName: 'c-colon-prime-take-while',
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
      'n:prime-take-while(-> $ < 50)',
    ],
  },
  'n:prime-nth': {
    title: 'n:prime-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the prime sequence.',
    linkName: 'c-colon-prime-nth',
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
      'n:prime-nth(1)',
      'n:prime-nth(2)',
      'n:prime-nth(10)',
    ],
  },
  'n:prime?': {
    title: 'n:prime?',
    category: 'Number Theory',
    description: 'Determines if a number is prime.',
    linkName: 'c-colon-prime-question-mark',
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
      'n:prime?(1)',
      'n:prime?(2)',
      'n:prime?(3)',
      'n:prime?(4)',
      'n:prime?(997)',
      'n:prime?(1001)',
    ],
  },
}
