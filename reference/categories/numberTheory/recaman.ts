import type { NumberTheorySequenceReference } from '.'

export const recamanReference: NumberTheorySequenceReference<'recaman'> = {
  'TEMP-nth.recaman-seq': {
    title: 'TEMP-nth.recaman-seq',
    category: 'Number Theory',
    description: 'Generates the Recaman sequence up to a specified length.',
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
      'let nt = import("TEMP-nth");\nnt.recaman-seq(5)',
      'let nt = import("TEMP-nth");\nnt.recaman-seq(10)',
      'let nt = import("TEMP-nth");\nnt.recaman-seq(20)',
    ],
  },
  'TEMP-nth.recaman-take-while': {
    title: 'TEMP-nth.recaman-take-while',
    category: 'Number Theory',
    description: 'Generates the Recaman sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.recaman-take-while(-> $ < 10)',
    ],
  },
  'TEMP-nth.recaman-nth': {
    title: 'TEMP-nth.recaman-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Recaman sequence.',
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
      'let nt = import("TEMP-nth");\nnt.recaman-nth(5)',
      'let nt = import("TEMP-nth");\nnt.recaman-nth(10)',
      'let nt = import("TEMP-nth");\nnt.recaman-nth(20)',
    ],
  },
  'TEMP-nth.recaman?': {
    title: 'TEMP-nth.recaman?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Recaman sequence.',
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
      'let nt = import("TEMP-nth");\nnt.recaman?(5)',
      'let nt = import("TEMP-nth");\nnt.recaman?(10)',
      'let nt = import("TEMP-nth");\nnt.recaman?(20)',
    ],
  },
}
