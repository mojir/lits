import type { NumberTheorySequenceReference } from '.'

export const perfectSquareReference: NumberTheorySequenceReference<'perfect-square'> = {
  'TEMP-nth.perfect-square-seq': {
    title: 'TEMP-nth.perfect-square-seq',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers up to a specified length.',
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
      'let nt = import("TEMP-nth");\nnt.perfect-square-seq(5)',
      'let nt = import("TEMP-nth");\nnt.perfect-square-seq(20)',
    ],
  },
  'TEMP-nth.perfect-square-take-while': {
    title: 'TEMP-nth.perfect-square-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.perfect-square-take-while(-> $ <= 100)',
    ],
  },
  'TEMP-nth.perfect-square-nth': {
    title: 'TEMP-nth.perfect-square-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect square numbers.',
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
      'let nt = import("TEMP-nth");\nnt.perfect-square-nth(1)',
      'let nt = import("TEMP-nth");\nnt.perfect-square-nth(5)',
    ],
  },
  'TEMP-nth.perfect-square?': {
    title: 'TEMP-nth.perfect-square?',
    category: 'Number Theory',
    description: 'Checks if a number is a perfect square.',
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
      'let nt = import("TEMP-nth");\nnt.perfect-square?(16)',
      'let nt = import("TEMP-nth");\nnt.perfect-square?(20)',
    ],
  },
}
