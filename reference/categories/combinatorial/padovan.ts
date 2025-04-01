import type { CombinatorialSequenceReference } from '.'

export const padovanReference: CombinatorialSequenceReference<'padovan'> = {
  'c:padovan-seq': {
    title: 'c:padovan-seq',
    category: 'Combinatorial',
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
      'c:padovan-seq(5)',
      'c:padovan-seq(10)',
      'c:padovan-seq(20)',
    ],
  },
  'c:padovan-take-while': {
    title: 'c:padovan-take-while',
    category: 'Combinatorial',
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
      'c:padovan-take-while(-> $ < 1000)',
    ],
  },
  'c:padovan-nth': {
    title: 'c:padovan-nth',
    category: 'Combinatorial',
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
      'c:padovan-nth(5)',
      'c:padovan-nth(10)',
      'c:padovan-nth(20)',
    ],
  },
  'c:padovan?': {
    title: 'c:padovan?',
    category: 'Combinatorial',
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
      'c:padovan?(1)',
      'c:padovan?(265)',
      'c:padovan?(6)',
    ],
  },
}
