import type { NumberTheorySequenceReference } from '.'

export const compositeReference: NumberTheorySequenceReference<'composite'> = {
  'nth.composite-seq': {
    title: 'nth.composite-seq',
    category: 'Number Theory',
    description: 'Generates the composite sequence up to a specified length.',
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
      'nth.composite-seq(1)',
      'nth.composite-seq(2)',
      'nth.composite-seq(10)',
    ],
  },
  'nth.composite-take-while': {
    title: 'nth.composite-take-while',
    category: 'Number Theory',
    description: 'Generates the composite sequence while a condition is met.',
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
      'nth.composite-take-while(-> $ < 50)',
    ],
  },
  'nth.composite-nth': {
    title: 'nth.composite-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the composite sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the composite number to retrieve.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'nth.composite-nth(1)',
      'nth.composite-nth(2)',
      'nth.composite-nth(10)',
    ],
  },
  'nth.composite?': {
    title: 'nth.composite?',
    category: 'Number Theory',
    description: 'Determines if a number is composite.',
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
      'nth.composite?(4)',
      'nth.composite?(5)',
      'nth.composite?(11)',
    ],
  },
}
