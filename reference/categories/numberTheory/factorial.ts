import type { NumberTheorySequenceReference } from '.'

export const factorialReference: NumberTheorySequenceReference<'factorial'> = {
  'n:factorial-seq': {
    title: 'n:factorial-seq',
    category: 'Number Theory',
    description: 'Generates the factorial sequence up to a specified length.',
    linkName: 'c-colon-factorial-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 19 (the maximum length of the pre-calculated factorial numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'n:factorial-seq(1)',
      'n:factorial-seq(2)',
      'n:factorial-seq(3)',
      'n:factorial-seq(4)',
      'n:factorial-seq(5)',
      'n:factorial-seq(10)',
    ],
  },
  'n:factorial-take-while': {
    title: 'n:factorial-take-while',
    category: 'Number Theory',
    description: 'Generates the factorial sequence while a condition is met.',
    linkName: 'c-colon-factorial-take-while',
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
      'n:factorial-take-while(-> $ < 1000)',
    ],
  },
  'n:factorial-nth': {
    title: 'n:factorial-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the factorial sequence.',
    linkName: 'c-colon-factorial-nth',
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
      'n:factorial-nth(1)',
      'n:factorial-nth(2)',
      'n:factorial-nth(3)',
      'n:factorial-nth(4)',
      'n:factorial-nth(5)',
      'n:factorial-nth(10)',
    ],
  },
  'n:factorial?': {
    title: 'n:factorial?',
    category: 'Number Theory',
    description: 'Checks if a number is in the factorial sequence.',
    linkName: 'c-colon-factorial-question-mark',
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
      'n:factorial?(1)',
      'n:factorial?(2)',
      'n:factorial?(3)',
      'n:factorial?(4)',
      'n:factorial?(5)',
      'n:factorial?(6)',
      'n:factorial?(7)',
      'n:factorial?(8)',
      'n:factorial?(9)',
      'n:factorial?(3628800)',
    ],
  },
}
