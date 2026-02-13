import type { NumberTheorySequenceReference } from '.'

export const primeReference: NumberTheorySequenceReference<'prime'> = {
  'TEMP-nth.prime-seq': {
    title: 'TEMP-nth.prime-seq',
    category: 'Number Theory',
    description: 'Generates the prime sequence up to a specified length.',
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
      'let nt = import("TEMP-nth");\nnt.prime-seq(1)',
      'let nt = import("TEMP-nth");\nnt.prime-seq(2)',
      'let nt = import("TEMP-nth");\nnt.prime-seq(10)',
    ],
  },
  'TEMP-nth.prime-take-while': {
    title: 'TEMP-nth.prime-take-while',
    category: 'Number Theory',
    description: 'Generates the prime sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.prime-take-while(-> $ < 50)',
    ],
  },
  'TEMP-nth.prime-nth': {
    title: 'TEMP-nth.prime-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the prime sequence.',
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
      'let nt = import("TEMP-nth");\nnt.prime-nth(1)',
      'let nt = import("TEMP-nth");\nnt.prime-nth(2)',
      'let nt = import("TEMP-nth");\nnt.prime-nth(10)',
    ],
  },
  'TEMP-nth.prime?': {
    title: 'TEMP-nth.prime?',
    category: 'Number Theory',
    description: 'Determines if a number is prime.',
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
      'let nt = import("TEMP-nth");\nnt.prime?(1)',
      'let nt = import("TEMP-nth");\nnt.prime?(2)',
      'let nt = import("TEMP-nth");\nnt.prime?(3)',
      'let nt = import("TEMP-nth");\nnt.prime?(4)',
      'let nt = import("TEMP-nth");\nnt.prime?(997)',
      'let nt = import("TEMP-nth");\nnt.prime?(1001)',
    ],
  },
}
