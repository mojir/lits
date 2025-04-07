import type { NumberTheorySequenceReference } from '.'

export const padovanReference: NumberTheorySequenceReference<'padovan'> = {
  'n:padovan-seq': {
    title: 'n:padovan-seq',
    category: 'Number Theory',
    description: 'Generates the Padovan sequence up to a specified length.',
    linkName: 'c-colon-padovan-seq',
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
      'n:padovan-seq(5)',
      'n:padovan-seq(10)',
      'n:padovan-seq(20)',
    ],
  },
  'n:padovan-take-while': {
    title: 'n:padovan-take-while',
    category: 'Number Theory',
    description: 'Generates the Padovan sequence while a condition is met.',
    linkName: 'c-colon-padovan-take-while',
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
      'n:padovan-take-while(-> $ < 1000)',
    ],
  },
  'n:padovan-nth': {
    title: 'n:padovan-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Padovan sequence.',
    linkName: 'c-colon-padovan-nth',
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
      'n:padovan-nth(5)',
      'n:padovan-nth(10)',
      'n:padovan-nth(20)',
    ],
  },
  'n:padovan?': {
    title: 'n:padovan?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Padovan sequence.',
    linkName: 'c-colon-padovan-question-mark',
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
      'n:padovan?(1)',
      'n:padovan?(265)',
      'n:padovan?(6)',
    ],
  },
}
