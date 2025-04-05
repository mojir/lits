import type { CombinatorialSequenceReference } from '.'

export const abundantReference: CombinatorialSequenceReference<'abundant'> = {
  'c:abundant-seq': {
    title: 'c:abundant-seq',
    category: 'Combinatorial',
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
      'c:abundant-seq(1)',
      'c:abundant-seq(5)',
    ],
  },
  'c:abundant-take-while': {
    title: 'c:abundant-take-while',
    category: 'Combinatorial',
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
      'c:abundant-take-while(-> $ < 100)',
    ],
  },
  'c:abundant-nth': {
    title: 'c:abundant-nth',
    category: 'Combinatorial',
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
      'c:abundant-nth(1)',
      'c:abundant-nth(5)',
    ],
  },
  'c:abundant?': {
    title: 'c:abundant?',
    category: 'Combinatorial',
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
      'c:abundant?(12)',
      'c:abundant?(15)',
    ],
  },
}
