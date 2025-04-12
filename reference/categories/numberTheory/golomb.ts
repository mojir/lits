import type { NumberTheorySequenceReference } from '.'

export const golombReference: NumberTheorySequenceReference<'golomb'> = {
  'nth:golomb-seq': {
    title: 'nth:golomb-seq',
    category: 'Number Theory',
    description: 'Generates the Golomb sequence up to a specified length.',
    linkName: 'nth-colon-golomb-seq',
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
      'nth:golomb-seq(5)',
      'nth:golomb-seq(20)',
    ],
  },
  'nth:golomb-take-while': {
    title: 'nth:golomb-take-while',
    category: 'Number Theory',
    description: 'Generates the Golomb sequence while a condition is met.',
    linkName: 'nth-colon-golomb-take-while',
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
      'nth:golomb-take-while(-> $ <= 10)',
    ],
  },
  'nth:golomb-nth': {
    title: 'nth:golomb-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Golomb sequence.',
    linkName: 'nth-colon-golomb-nth',
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
      'nth:golomb-nth(5)',
      'nth:golomb-nth(1000)',
    ],
  },
  'nth:golomb?': {
    title: 'nth:golomb?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Golomb sequence.',
    linkName: 'nth-colon-golomb-question',
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
      'nth:golomb?(1)',
      'nth:golomb?(2)',
      'nth:golomb?(3345)',
      'nth:golomb?(67867864)',
    ],
  },
}
