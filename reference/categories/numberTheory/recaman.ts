import type { NumberTheorySequenceReference } from '.'

export const recamanReference: NumberTheorySequenceReference<'recaman'> = {
  'nth:recaman-seq': {
    title: 'nth:recaman-seq',
    category: 'Number Theory',
    description: 'Generates the Recaman sequence up to a specified length.',
    linkName: 'c-colon-recaman-seq',
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
      'nth:recaman-seq(5)',
      'nth:recaman-seq(10)',
      'nth:recaman-seq(20)',
    ],
  },
  'nth:recaman-take-while': {
    title: 'nth:recaman-take-while',
    category: 'Number Theory',
    description: 'Generates the Recaman sequence while a condition is met.',
    linkName: 'c-colon-recaman-take-while',
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
      'nth:recaman-take-while(-> $ < 10)',
    ],
  },
  'nth:recaman-nth': {
    title: 'nth:recaman-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Recaman sequence.',
    linkName: 'c-colon-recaman-nth',
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
      'nth:recaman-nth(5)',
      'nth:recaman-nth(10)',
      'nth:recaman-nth(20)',
    ],
  },
  'nth:recaman?': {
    title: 'nth:recaman?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Recaman sequence.',
    linkName: 'c-colon-recaman-question-mark',
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
      'nth:recaman?(5)',
      'nth:recaman?(10)',
      'nth:recaman?(20)',
    ],
  },
}
