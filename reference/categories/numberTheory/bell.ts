import type { NumberTheorySequenceReference } from '.'

export const bellReference: NumberTheorySequenceReference<'bell'> = {
  'nth.bell-seq': {
    title: 'nth.bell-seq',
    category: 'Number Theory',
    description: 'Generates the Bell sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 22 (the maximum length of the pre-calculated bell numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'nth.bell-seq(5)',
      'nth.bell-seq(10)',
      'nth.bell-seq()',
    ],
  },
  'nth.bell-take-while': {
    title: 'nth.bell-take-while',
    category: 'Number Theory',
    description: 'Generates the Bell sequence while a condition is met.',
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
      'nth.bell-take-while(-> $ < 1000)',
    ],
  },
  'nth.bell-nth': {
    title: 'nth.bell-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Bell sequence.',
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
      'nth.bell-nth(5)',
      'nth.bell-nth(10)',
    ],
  },
  'nth.bell?': {
    title: 'nth.bell?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Bell sequence.',
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
      'nth.bell?(1)',
      'nth.bell?(27644437)',
      'nth.bell?(27644436)',
    ],
  },
}
