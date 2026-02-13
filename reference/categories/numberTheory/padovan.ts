import type { NumberTheorySequenceReference } from '.'

export const padovanReference: NumberTheorySequenceReference<'padovan'> = {
  'TEMP-nth.padovan-seq': {
    title: 'TEMP-nth.padovan-seq',
    category: 'Number Theory',
    description: 'Generates the Padovan sequence up to a specified length.',
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
      'let { padovan-seq } = import("TEMP-nth");\npadovan-seq(5)',
      'let { padovan-seq } = import("TEMP-nth");\npadovan-seq(10)',
      'let { padovan-seq } = import("TEMP-nth");\npadovan-seq(20)',
    ],
  },
  'TEMP-nth.padovan-take-while': {
    title: 'TEMP-nth.padovan-take-while',
    category: 'Number Theory',
    description: 'Generates the Padovan sequence while a condition is met.',
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
      'let { padovan-take-while } = import("TEMP-nth");\npadovan-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.padovan-nth': {
    title: 'TEMP-nth.padovan-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Padovan sequence.',
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
      'let { padovan-nth } = import("TEMP-nth");\npadovan-nth(5)',
      'let { padovan-nth } = import("TEMP-nth");\npadovan-nth(10)',
      'let { padovan-nth } = import("TEMP-nth");\npadovan-nth(20)',
    ],
  },
  'TEMP-nth.padovan?': {
    title: 'TEMP-nth.padovan?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Padovan sequence.',
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
      'let { padovan? } = import("TEMP-nth");\npadovan?(1)',
      'let { padovan? } = import("TEMP-nth");\npadovan?(265)',
      'let { padovan? } = import("TEMP-nth");\npadovan?(6)',
    ],
  },
}
