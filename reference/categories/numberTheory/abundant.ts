import type { NumberTheorySequenceReference } from '.'

export const abundantReference: NumberTheorySequenceReference<'abundant'> = {
  'nth.abundant-seq': {
    title: 'nth.abundant-seq',
    category: 'Number Theory',
    description: 'Generates the abundant numbers up to a specified length.',
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
      'let nt = import("nth");\nnt.abundant-seq(1)',
      'let nt = import("nth");\nnt.abundant-seq(5)',
    ],
  },
  'nth.abundant-take-while': {
    title: 'nth.abundant-take-while',
    category: 'Number Theory',
    description: 'Generates the abundant numbers while a condition is met.',
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
      'let nt = import("nth");\nnt.abundant-take-while(-> $ < 100)',
    ],
  },
  'nth.abundant-nth': {
    title: 'nth.abundant-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the abundant numbers.',
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
      'let nt = import("nth");\nnt.abundant-nth(1)',
      'let nt = import("nth");\nnt.abundant-nth(5)',
    ],
  },
  'nth.abundant?': {
    title: 'nth.abundant?',
    category: 'Number Theory',
    description: 'Checks if a number is abundant.',
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
      'let nt = import("nth");\nnt.abundant?(12)',
      'let nt = import("nth");\nnt.abundant?(15)',
    ],
  },
}
