import type { NumberTheorySequenceReference } from '.'

export const catalanReference: NumberTheorySequenceReference<'catalan'> = {
  'Number-Theory.catalan-seq': {
    title: 'Number-Theory.catalan-seq',
    category: 'Number Theory',
    description: 'Generates the Catalan sequence up to a specified length.',
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
      'let { catalan-seq } = import("Number-Theory");\ncatalan-seq(5)',
      'let { catalan-seq } = import("Number-Theory");\ncatalan-seq(10)',
      'let { catalan-seq } = import("Number-Theory");\ncatalan-seq()',
    ],
  },
  'Number-Theory.catalan-take-while': {
    title: 'Number-Theory.catalan-take-while',
    category: 'Number Theory',
    description: 'Generates the Catalan sequence while a condition is met.',
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
      'let { catalan-take-while } = import("Number-Theory");\ncatalan-take-while(-> $ < 1000)',
    ],
  },
  'Number-Theory.catalan-nth': {
    title: 'Number-Theory.catalan-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Catalan sequence.',
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
      'let { catalan-nth } = import("Number-Theory");\ncatalan-nth(5)',
      'let { catalan-nth } = import("Number-Theory");\ncatalan-nth(10)',
    ],
  },
  'Number-Theory.catalan?': {
    title: 'Number-Theory.catalan?',
    category: 'Number Theory',
    description: 'Determines if a number is in the Catalan sequence.',
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
      'let { catalan? } = import("Number-Theory");\ncatalan?(5)',
      'let { catalan? } = import("Number-Theory");\ncatalan?(10)',
    ],
  },
}
