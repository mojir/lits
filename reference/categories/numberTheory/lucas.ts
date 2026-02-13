import type { NumberTheorySequenceReference } from '.'

export const lucasReference: NumberTheorySequenceReference<'lucas'> = {
  'TEMP-nth.lucas-seq': {
    title: 'TEMP-nth.lucas-seq',
    category: 'Number Theory',
    description: 'Generates the lucas sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 77 (the maximum length of the pre-calculated Lucas numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let { lucas-seq } = import("TEMP-nth");\nlucas-seq(1)',
      'let { lucas-seq } = import("TEMP-nth");\nlucas-seq(2)',
      'let { lucas-seq } = import("TEMP-nth");\nlucas-seq()',
    ],
  },
  'TEMP-nth.lucas-take-while': {
    title: 'TEMP-nth.lucas-take-while',
    category: 'Number Theory',
    description: 'Generates the lucas sequence while a condition is met.',
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
      'let { lucas-take-while } = import("TEMP-nth");\nlucas-take-while(-> $ < 100)',
    ],
  },
  'TEMP-nth.lucas-nth': {
    title: 'TEMP-nth.lucas-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the lucas sequence.',
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
      'let { lucas-nth } = import("TEMP-nth");\nlucas-nth(1)',
      'let { lucas-nth } = import("TEMP-nth");\nlucas-nth(2)',
      'let { lucas-nth } = import("TEMP-nth");\nlucas-nth(10)',
    ],
  },
  'TEMP-nth.lucas?': {
    title: 'TEMP-nth.lucas?',
    category: 'Number Theory',
    description: 'Determines if a number is in the lucas sequence.',
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
      'let { lucas? } = import("TEMP-nth");\nlucas?(1)',
      'let { lucas? } = import("TEMP-nth");\nlucas?(2)',
      'let { lucas? } = import("TEMP-nth");\nlucas?(10)',
    ],
  },
}
