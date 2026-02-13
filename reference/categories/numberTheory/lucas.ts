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
      'let nt = import("TEMP-nth");\nnt.lucas-seq(1)',
      'let nt = import("TEMP-nth");\nnt.lucas-seq(2)',
      'let nt = import("TEMP-nth");\nnt.lucas-seq()',
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
      'let nt = import("TEMP-nth");\nnt.lucas-take-while(-> $ < 100)',
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
      'let nt = import("TEMP-nth");\nnt.lucas-nth(1)',
      'let nt = import("TEMP-nth");\nnt.lucas-nth(2)',
      'let nt = import("TEMP-nth");\nnt.lucas-nth(10)',
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
      'let nt = import("TEMP-nth");\nnt.lucas?(1)',
      'let nt = import("TEMP-nth");\nnt.lucas?(2)',
      'let nt = import("TEMP-nth");\nnt.lucas?(10)',
    ],
  },
}
