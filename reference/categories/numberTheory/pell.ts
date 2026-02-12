import type { NumberTheorySequenceReference } from '.'

export const pellReference: NumberTheorySequenceReference<'pell'> = {
  'nth.pell-seq': {
    title: 'nth.pell-seq',
    category: 'Number Theory',
    description: 'Generates the Pell sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 42 (the maximum length of the pre-calculated Pell numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let nt = import("nth");\nnt.pell-seq(5)',
      'let nt = import("nth");\nnt.pell-seq(10)',
      'let nt = import("nth");\nnt.pell-seq()',
    ],
  },
  'nth.pell-take-while': {
    title: 'nth.pell-take-while',
    category: 'Number Theory',
    description: 'Generates the Pell sequence while a condition is met.',
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
      'let nt = import("nth");\nnt.pell-take-while(-> $ < 1000)',
    ],
  },
  'nth.pell-nth': {
    title: 'nth.pell-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Pell sequence.',
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
      'let nt = import("nth");\nnt.pell-nth(5)',
      'let nt = import("nth");\nnt.pell-nth(10)',
      'let nt = import("nth");\nnt.pell-nth(20)',
    ],
  },
  'nth.pell?': {
    title: 'nth.pell?',
    category: 'Number Theory',
    description: 'Checks if a number is a Pell number.',
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
      'let nt = import("nth");\nnt.pell?(1)',
      'let nt = import("nth");\nnt.pell?(470832)',
      'let nt = import("nth");\nnt.pell?(10)',
    ],
  },
}
