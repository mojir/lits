import type { NumberTheorySequenceReference } from '.'

export const abundantReference: NumberTheorySequenceReference<'abundant'> = {
  'nth:abundant-seq': {
    title: 'nth:abundant-seq',
    category: 'Number Theory',
    description: 'Generates the abundant numbers up to a specified length.',
    linkName: 'nth-colon-abundant-seq',
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
      'nth:abundant-seq(1)',
      'nth:abundant-seq(5)',
    ],
  },
  'nth:abundant-take-while': {
    title: 'nth:abundant-take-while',
    category: 'Number Theory',
    description: 'Generates the abundant numbers while a condition is met.',
    linkName: 'nth-colon-abundant-take-while',
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
      'nth:abundant-take-while(-> $ < 100)',
    ],
  },
  'nth:abundant-nth': {
    title: 'nth:abundant-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the abundant numbers.',
    linkName: 'nth-colon-abundant-nth',
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
      'nth:abundant-nth(1)',
      'nth:abundant-nth(5)',
    ],
  },
  'nth:abundant?': {
    title: 'nth:abundant?',
    category: 'Number Theory',
    description: 'Checks if a number is abundant.',
    linkName: 'nth-colon-abundant-question',
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
      'nth:abundant?(12)',
      'nth:abundant?(15)',
    ],
  },
}
