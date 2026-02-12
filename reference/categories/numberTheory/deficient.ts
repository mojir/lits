import type { NumberTheorySequenceReference } from '.'

export const deficientReference: NumberTheorySequenceReference<'deficient'> = {
  'nth.deficient-seq': {
    title: 'nth.deficient-seq',
    category: 'Number Theory',
    description: 'Generates the deficient numbers up to a specified length.',
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
      'let nt = import("nth");\nnt.deficient-seq(1)',
      'let nt = import("nth");\nnt.deficient-seq(5)',
    ],
  },
  'nth.deficient-take-while': {
    title: 'nth.deficient-take-while',
    category: 'Number Theory',
    description: 'Generates the deficient numbers while a condition is met.',
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
      'let nt = import("nth");\nnt.deficient-take-while(-> $ < 100)',
    ],
  },
  'nth.deficient-nth': {
    title: 'nth.deficient-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the deficient numbers.',
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
      'let nt = import("nth");\nnt.deficient-nth(5)',
      'let nt = import("nth");\nnt.deficient-nth(12)',
    ],
  },
  'nth.deficient?': {
    title: 'nth.deficient?',
    category: 'Number Theory',
    description: 'Checks if a number is deficient.',
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
      'let nt = import("nth");\nnt.deficient?(12)',
      'let nt = import("nth");\nnt.deficient?(15)',
    ],
  },
}
