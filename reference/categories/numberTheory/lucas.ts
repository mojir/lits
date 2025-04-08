import type { NumberTheorySequenceReference } from '.'

export const lucasReference: NumberTheorySequenceReference<'lucas'> = {
  'nth:lucas-seq': {
    title: 'nth:lucas-seq',
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
      'nth:lucas-seq(1)',
      'nth:lucas-seq(2)',
      'nth:lucas-seq()',
    ],
  },
  'nth:lucas-take-while': {
    title: 'nth:lucas-take-while',
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
      'nth:lucas-take-while(-> $ < 100)',
    ],
  },
  'nth:lucas-nth': {
    title: 'nth:lucas-nth',
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
      'nth:lucas-nth(1)',
      'nth:lucas-nth(2)',
      'nth:lucas-nth(10)',
    ],
  },
  'nth:lucas?': {
    title: 'nth:lucas?',
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
      'nth:lucas?(1)',
      'nth:lucas?(2)',
      'nth:lucas?(10)',
    ],
  },
}
