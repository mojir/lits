import type { NumberTheorySequenceReference } from '.'

export const golombReference: NumberTheorySequenceReference<'golomb'> = {
  'Number-Theory.golomb-seq': {
    title: 'Number-Theory.golomb-seq',
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
      'let { golomb-seq } = import("Number-Theory");\ngolomb-seq(5)',
      'let { golomb-seq } = import("Number-Theory");\ngolomb-seq(20)',
    ],
  },
  'Number-Theory.golomb-take-while': {
    title: 'Number-Theory.golomb-take-while',
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
      'let { golomb-take-while } = import("Number-Theory");\ngolomb-take-while(-> $ <= 10)',
    ],
  },
  'Number-Theory.golomb-nth': {
    title: 'Number-Theory.golomb-nth',
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
      'let { golomb-nth } = import("Number-Theory");\ngolomb-nth(5)',
      'let { golomb-nth } = import("Number-Theory");\ngolomb-nth(1000)',
    ],
  },
  'Number-Theory.golomb?': {
    title: 'Number-Theory.golomb?',
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
      'let { golomb? } = import("Number-Theory");\ngolomb?(1)',
      'let { golomb? } = import("Number-Theory");\ngolomb?(2)',
      'let { golomb? } = import("Number-Theory");\ngolomb?(3345)',
      'let { golomb? } = import("Number-Theory");\ngolomb?(67867864)',
    ],
  },
}
