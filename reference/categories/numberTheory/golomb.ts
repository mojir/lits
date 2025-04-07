import type { NumberTheorySequenceReference } from '.'

export const golombReference: NumberTheorySequenceReference<'golomb'> = {
  'n:golomb-seq': {
    title: 'n:golomb-seq',
    category: 'Number Theory',
    description: 'Generates the Golomb sequence up to a specified length.',
    linkName: 'c-colon-golomb-seq',
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
      'n:golomb-seq(5)',
      'n:golomb-seq(20)',
    ],
  },
  'n:golomb-take-while': {
    title: 'n:golomb-take-while',
    category: 'Number Theory',
    description: 'Generates the Golomb sequence while a condition is met.',
    linkName: 'c-colon-golomb-take-while',
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
      'n:golomb-take-while(-> $ <= 10)',
    ],
  },
  'n:golomb-nth': {
    title: 'n:golomb-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Golomb sequence.',
    linkName: 'c-colon-golomb-nth',
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
      'n:golomb-nth(5)',
      'n:golomb-nth(1000)',
    ],
  },
  'n:golomb?': {
    title: 'n:golomb?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Golomb sequence.',
    linkName: 'c-colon-golomb',
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
      'n:golomb?(1)',
      'n:golomb?(2)',
      'n:golomb?(3345)',
      'n:golomb?(67867864)',
    ],
  },
}
