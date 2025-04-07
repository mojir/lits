import type { NumberTheorySequenceReference } from '.'

export const abundantReference: NumberTheorySequenceReference<'abundant'> = {
  'n:abundant-seq': {
    title: 'n:abundant-seq',
    category: 'Number Theory',
    description: 'Generates the abundant numbers up to a specified length.',
    linkName: 'c-colon-abundant-seq',
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
      'n:abundant-seq(1)',
      'n:abundant-seq(5)',
    ],
  },
  'n:abundant-take-while': {
    title: 'n:abundant-take-while',
    category: 'Number Theory',
    description: 'Generates the abundant numbers while a condition is met.',
    linkName: 'c-colon-abundant-take-while',
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
      'n:abundant-take-while(-> $ < 100)',
    ],
  },
  'n:abundant-nth': {
    title: 'n:abundant-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the abundant numbers.',
    linkName: 'c-colon-abundant-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the number in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:abundant-nth(1)',
      'n:abundant-nth(5)',
    ],
  },
  'n:abundant?': {
    title: 'n:abundant?',
    category: 'Number Theory',
    description: 'Checks if a number is abundant.',
    linkName: 'c-colon-abundant',
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
      'n:abundant?(12)',
      'n:abundant?(15)',
    ],
  },
}
