import type { NumberTheorySequenceReference } from '.'

export const catalanReference: NumberTheorySequenceReference<'catalan'> = {
  'n:catalan-seq': {
    title: 'n:catalan-seq',
    category: 'Number Theory',
    description: 'Generates the Catalan sequence up to a specified length.',
    linkName: 'c-colon-catalan-seq',
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
      'n:catalan-seq(5)',
      'n:catalan-seq(10)',
      'n:catalan-seq()',
    ],
  },
  'n:catalan-take-while': {
    title: 'n:catalan-take-while',
    category: 'Number Theory',
    description: 'Generates the Catalan sequence while a condition is met.',
    linkName: 'c-colon-catalan-take-while',
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
      'n:catalan-take-while(-> $ < 1000)',
    ],
  },
  'n:catalan-nth': {
    title: 'n:catalan-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Catalan sequence.',
    linkName: 'c-colon-catalan-nth',
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
      'n:catalan-nth(5)',
      'n:catalan-nth(10)',
    ],
  },
  'n:catalan?': {
    title: 'n:catalan?',
    category: 'Number Theory',
    description: 'Determines if a number is in the Catalan sequence.',
    linkName: 'c-colon-catalan-question-mark',
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
      'n:catalan?(5)',
      'n:catalan?(10)',
    ],
  },
}
