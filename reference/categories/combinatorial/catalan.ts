import type { CombinatorialSequenceReference } from '.'

export const catalanReference: CombinatorialSequenceReference<'catalan'> = {
  'c:catalan-seq': {
    title: 'c:catalan-seq',
    category: 'Combinatorial',
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
      'c:catalan-seq(5)',
      'c:catalan-seq(10)',
      'c:catalan-seq()',
    ],
  },
  'c:catalan-take-while': {
    title: 'c:catalan-take-while',
    category: 'Combinatorial',
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
      'c:catalan-take-while(-> $ < 1000)',
    ],
  },
  'c:catalan-nth': {
    title: 'c:catalan-nth',
    category: 'Combinatorial',
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
      'c:catalan-nth(5)',
      'c:catalan-nth(10)',
    ],
  },
  'c:catalan?': {
    title: 'c:catalan?',
    category: 'Combinatorial',
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
      'c:catalan?(5)',
      'c:catalan?(10)',
    ],
  },
}
