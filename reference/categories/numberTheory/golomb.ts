import type { NumberTheorySequenceReference } from '.'

export const golombReference: NumberTheorySequenceReference<'golomb'> = {
  'TEMP-nth.golomb-seq': {
    title: 'TEMP-nth.golomb-seq',
    category: 'Number Theory',
    description: 'Generates the Golomb sequence up to a specified length.',
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
      'let { golomb-seq } = import("TEMP-nth");\ngolomb-seq(5)',
      'let { golomb-seq } = import("TEMP-nth");\ngolomb-seq(20)',
    ],
  },
  'TEMP-nth.golomb-take-while': {
    title: 'TEMP-nth.golomb-take-while',
    category: 'Number Theory',
    description: 'Generates the Golomb sequence while a condition is met.',
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
      'let { golomb-take-while } = import("TEMP-nth");\ngolomb-take-while(-> $ <= 10)',
    ],
  },
  'TEMP-nth.golomb-nth': {
    title: 'TEMP-nth.golomb-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Golomb sequence.',
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
      'let { golomb-nth } = import("TEMP-nth");\ngolomb-nth(5)',
      'let { golomb-nth } = import("TEMP-nth");\ngolomb-nth(1000)',
    ],
  },
  'TEMP-nth.golomb?': {
    title: 'TEMP-nth.golomb?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Golomb sequence.',
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
      'let { golomb? } = import("TEMP-nth");\ngolomb?(1)',
      'let { golomb? } = import("TEMP-nth");\ngolomb?(2)',
      'let { golomb? } = import("TEMP-nth");\ngolomb?(3345)',
      'let { golomb? } = import("TEMP-nth");\ngolomb?(67867864)',
    ],
  },
}
