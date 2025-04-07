import type { NumberTheorySequenceReference } from '.'

export const recamanReference: NumberTheorySequenceReference<'recaman'> = {
  'n:recaman-seq': {
    title: 'n:recaman-seq',
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
      'n:recaman-seq(5)',
      'n:recaman-seq(10)',
      'n:recaman-seq(20)',
    ],
  },
  'n:recaman-take-while': {
    title: 'n:recaman-take-while',
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
      'n:recaman-take-while(-> $ < 10)',
    ],
  },
  'n:recaman-nth': {
    title: 'n:recaman-nth',
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
      'n:recaman-nth(5)',
      'n:recaman-nth(10)',
      'n:recaman-nth(20)',
    ],
  },
  'n:recaman?': {
    title: 'n:recaman?',
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
      'n:recaman?(5)',
      'n:recaman?(10)',
      'n:recaman?(20)',
    ],
  },
}
