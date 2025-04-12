import type { NumberTheorySequenceReference } from '.'

export const padovanReference: NumberTheorySequenceReference<'padovan'> = {
  'nth:padovan-seq': {
    title: 'nth:padovan-seq',
    category: 'Number Theory',
    description: 'Generates the Padovan sequence up to a specified length.',
    linkName: 'nth-colon-padovan-seq',
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
      'nth:padovan-seq(5)',
      'nth:padovan-seq(10)',
      'nth:padovan-seq(20)',
    ],
  },
  'nth:padovan-take-while': {
    title: 'nth:padovan-take-while',
    category: 'Number Theory',
    description: 'Generates the Padovan sequence while a condition is met.',
    linkName: 'nth-colon-padovan-take-while',
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
      'nth:padovan-take-while(-> $ < 1000)',
    ],
  },
  'nth:padovan-nth': {
    title: 'nth:padovan-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Padovan sequence.',
    linkName: 'nth-colon-padovan-nth',
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
      'nth:padovan-nth(5)',
      'nth:padovan-nth(10)',
      'nth:padovan-nth(20)',
    ],
  },
  'nth:padovan?': {
    title: 'nth:padovan?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Padovan sequence.',
    linkName: 'nth-colon-padovan-question',
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
      'nth:padovan?(1)',
      'nth:padovan?(265)',
      'nth:padovan?(6)',
    ],
  },
}
