import type { CombinatorialSequenceReference } from '.'

export const golombReference: CombinatorialSequenceReference<'golomb'> = {
  'c:golomb-seq': {
    title: 'c:golomb-seq',
    category: 'Combinatorial',
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
      'c:golomb-seq(5)',
      'c:golomb-seq(20)',
    ],
  },
  'c:golomb-take-while': {
    title: 'c:golomb-take-while',
    category: 'Combinatorial',
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
      'c:golomb-take-while(-> $ <= 10)',
    ],
  },
  'c:golomb-nth': {
    title: 'c:golomb-nth',
    category: 'Combinatorial',
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
      'c:golomb-nth(5)',
      'c:golomb-nth(1000)',
    ],
  },
  'c:golomb?': {
    title: 'c:golomb?',
    category: 'Combinatorial',
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
      'c:golomb?(1)',
      'c:golomb?(2)',
      'c:golomb?(3345)',
      'c:golomb?(67867864)',
    ],
  },
}
