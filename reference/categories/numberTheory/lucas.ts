import type { NumberTheorySequenceReference } from '.'

export const lucasReference: NumberTheorySequenceReference<'lucas'> = {
  'n:lucas-seq': {
    title: 'n:lucas-seq',
    category: 'Number Theory',
    description: 'Generates the lucas sequence up to a specified length.',
    linkName: 'c-colon-lucas-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 77 (the maximum length of the pre-calculated Lucas numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'n:lucas-seq(1)',
      'n:lucas-seq(2)',
      'n:lucas-seq()',
    ],
  },
  'n:lucas-take-while': {
    title: 'n:lucas-take-while',
    category: 'Number Theory',
    description: 'Generates the lucas sequence while a condition is met.',
    linkName: 'c-colon-lucas-take-while',
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
      'n:lucas-take-while(-> $ < 100)',
    ],
  },
  'n:lucas-nth': {
    title: 'n:lucas-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the lucas sequence.',
    linkName: 'c-colon-lucas-nth',
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
      'n:lucas-nth(1)',
      'n:lucas-nth(2)',
      'n:lucas-nth(10)',
    ],
  },
  'n:lucas?': {
    title: 'n:lucas?',
    category: 'Number Theory',
    description: 'Determines if a number is in the lucas sequence.',
    linkName: 'c-colon-lucas-question-mark',
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
      'n:lucas?(1)',
      'n:lucas?(2)',
      'n:lucas?(10)',
    ],
  },
}
