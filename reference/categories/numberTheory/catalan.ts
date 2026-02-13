import type { NumberTheorySequenceReference } from '.'

export const catalanReference: NumberTheorySequenceReference<'catalan'> = {
  'TEMP-nth.catalan-seq': {
    title: 'TEMP-nth.catalan-seq',
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
      'let { catalan-seq } = import("TEMP-nth");\ncatalan-seq(5)',
      'let { catalan-seq } = import("TEMP-nth");\ncatalan-seq(10)',
      'let { catalan-seq } = import("TEMP-nth");\ncatalan-seq()',
    ],
  },
  'TEMP-nth.catalan-take-while': {
    title: 'TEMP-nth.catalan-take-while',
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
      'let { catalan-take-while } = import("TEMP-nth");\ncatalan-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.catalan-nth': {
    title: 'TEMP-nth.catalan-nth',
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
      'let { catalan-nth } = import("TEMP-nth");\ncatalan-nth(5)',
      'let { catalan-nth } = import("TEMP-nth");\ncatalan-nth(10)',
    ],
  },
  'TEMP-nth.catalan?': {
    title: 'TEMP-nth.catalan?',
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
      'let { catalan? } = import("TEMP-nth");\ncatalan?(5)',
      'let { catalan? } = import("TEMP-nth");\ncatalan?(10)',
    ],
  },
}
