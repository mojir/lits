import type { NumberTheorySequenceReference } from '.'

export const perfectReference: NumberTheorySequenceReference<'perfect'> = {
  'TEMP-nth.perfect-seq': {
    title: 'TEMP-nth.perfect-seq',
    category: 'Number Theory',
    description: 'Generates the perfect numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If no length is provided, it defaults to 7 (the maximum length of the pre-calculated perfect numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.perfect-seq(1)',
      'let nt = import("TEMP-nth");\nnt.perfect-seq(5)',
      'let nt = import("TEMP-nth");\nnt.perfect-seq()',
    ],
  },
  'TEMP-nth.perfect-take-while': {
    title: 'TEMP-nth.perfect-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect numbers while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.perfect-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.perfect-nth': {
    title: 'TEMP-nth.perfect-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the perfect number to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.perfect-nth(1)',
      'let nt = import("TEMP-nth");\nnt.perfect-nth(5)',
    ],
  },
  'TEMP-nth.perfect?': {
    title: 'TEMP-nth.perfect?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect numbers.',
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
      'let nt = import("TEMP-nth");\nnt.perfect?(0)',
      'let nt = import("TEMP-nth");\nnt.perfect?(1)',
      'let nt = import("TEMP-nth");\nnt.perfect?(2)',
      'let nt = import("TEMP-nth");\nnt.perfect?(3)',
      'let nt = import("TEMP-nth");\nnt.perfect?(4)',
      'let nt = import("TEMP-nth");\nnt.perfect?(5)',
      'let nt = import("TEMP-nth");\nnt.perfect?(6)',
      'let nt = import("TEMP-nth");\nnt.perfect?(7)',
      'let nt = import("TEMP-nth");\nnt.perfect?(8)',
      'let nt = import("TEMP-nth");\nnt.perfect?(9)',
    ],
  },
}
