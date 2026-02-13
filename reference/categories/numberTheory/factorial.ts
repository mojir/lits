import type { NumberTheorySequenceReference } from '.'

export const factorialReference: NumberTheorySequenceReference<'factorial'> = {
  'TEMP-nth.factorial-seq': {
    title: 'TEMP-nth.factorial-seq',
    category: 'Number Theory',
    description: 'Generates the factorial sequence up to a specified length.',
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
      'let nt = import("TEMP-nth");\nnt.factorial-seq(1)',
      'let nt = import("TEMP-nth");\nnt.factorial-seq(2)',
      'let nt = import("TEMP-nth");\nnt.factorial-seq(3)',
      'let nt = import("TEMP-nth");\nnt.factorial-seq(4)',
      'let nt = import("TEMP-nth");\nnt.factorial-seq(5)',
      'let nt = import("TEMP-nth");\nnt.factorial-seq(10)',
    ],
  },
  'TEMP-nth.factorial-take-while': {
    title: 'TEMP-nth.factorial-take-while',
    category: 'Number Theory',
    description: 'Generates the factorial sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.factorial-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.factorial-nth': {
    title: 'TEMP-nth.factorial-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the factorial sequence.',
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
      'let nt = import("TEMP-nth");\nnt.factorial-nth(1)',
      'let nt = import("TEMP-nth");\nnt.factorial-nth(2)',
      'let nt = import("TEMP-nth");\nnt.factorial-nth(3)',
      'let nt = import("TEMP-nth");\nnt.factorial-nth(4)',
      'let nt = import("TEMP-nth");\nnt.factorial-nth(5)',
      'let nt = import("TEMP-nth");\nnt.factorial-nth(10)',
    ],
  },
  'TEMP-nth.factorial?': {
    title: 'TEMP-nth.factorial?',
    category: 'Number Theory',
    description: 'Checks if a number is in the factorial sequence.',
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
      'let nt = import("TEMP-nth");\nnt.factorial?(1)',
      'let nt = import("TEMP-nth");\nnt.factorial?(2)',
      'let nt = import("TEMP-nth");\nnt.factorial?(3)',
      'let nt = import("TEMP-nth");\nnt.factorial?(4)',
      'let nt = import("TEMP-nth");\nnt.factorial?(5)',
      'let nt = import("TEMP-nth");\nnt.factorial?(6)',
      'let nt = import("TEMP-nth");\nnt.factorial?(7)',
      'let nt = import("TEMP-nth");\nnt.factorial?(8)',
      'let nt = import("TEMP-nth");\nnt.factorial?(9)',
      'let nt = import("TEMP-nth");\nnt.factorial?(3628800)',
    ],
  },
}
