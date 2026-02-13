import type { NumberTheorySequenceReference } from '.'

export const padovanReference: NumberTheorySequenceReference<'padovan'> = {
  'Number-Theory.padovan-seq': {
    title: 'Number-Theory.padovan-seq',
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
      'let { padovan-seq } = import("Number-Theory");\npadovan-seq(5)',
      'let { padovan-seq } = import("Number-Theory");\npadovan-seq(10)',
      'let { padovan-seq } = import("Number-Theory");\npadovan-seq(20)',
    ],
  },
  'Number-Theory.padovan-take-while': {
    title: 'Number-Theory.padovan-take-while',
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
      'let { padovan-take-while } = import("Number-Theory");\npadovan-take-while(-> $ < 1000)',
    ],
  },
  'Number-Theory.padovan-nth': {
    title: 'Number-Theory.padovan-nth',
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
      'let { padovan-nth } = import("Number-Theory");\npadovan-nth(5)',
      'let { padovan-nth } = import("Number-Theory");\npadovan-nth(10)',
      'let { padovan-nth } = import("Number-Theory");\npadovan-nth(20)',
    ],
  },
  'Number-Theory.padovan?': {
    title: 'Number-Theory.padovan?',
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
      'let { padovan? } = import("Number-Theory");\npadovan?(1)',
      'let { padovan? } = import("Number-Theory");\npadovan?(265)',
      'let { padovan? } = import("Number-Theory");\npadovan?(6)',
    ],
  },
}
