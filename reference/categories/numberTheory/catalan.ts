import type { NumberTheorySequenceReference } from '.'

export const catalanReference: NumberTheorySequenceReference<'catalan'> = {
  'nth.catalan-seq': {
    title: 'nth.catalan-seq',
    category: 'Number Theory',
    description: 'Generates the Catalan sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description:
          'The length of the sequence to generate. If not provided, the default is 30 (the maximum length of the pre-calculated catalan numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'nth.catalan-seq(5)',
      'nth.catalan-seq(10)',
      'nth.catalan-seq()',
    ],
  },
  'nth.catalan-take-while': {
    title: 'nth.catalan-take-while',
    category: 'Number Theory',
    description: 'Generates the Catalan sequence while a condition is met.',
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
      'nth.catalan-take-while(-> $ < 1000)',
    ],
  },
  'nth.catalan-nth': {
    title: 'nth.catalan-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Catalan sequence.',
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
      'nth.catalan-nth(5)',
      'nth.catalan-nth(10)',
    ],
  },
  'nth.catalan?': {
    title: 'nth.catalan?',
    category: 'Number Theory',
    description: 'Determines if a number is in the Catalan sequence.',
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
      'nth.catalan?(5)',
      'nth.catalan?(10)',
    ],
  },
}
