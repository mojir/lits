import type { NumberTheorySequenceReference } from '.'

export const pellReference: NumberTheorySequenceReference<'pell'> = {
  'TEMP-nth.pell-seq': {
    title: 'TEMP-nth.pell-seq',
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
      'let { pell-seq } = import("TEMP-nth");\npell-seq(5)',
      'let { pell-seq } = import("TEMP-nth");\npell-seq(10)',
      'let { pell-seq } = import("TEMP-nth");\npell-seq()',
    ],
  },
  'TEMP-nth.pell-take-while': {
    title: 'TEMP-nth.pell-take-while',
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
      'let { pell-take-while } = import("TEMP-nth");\npell-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.pell-nth': {
    title: 'TEMP-nth.pell-nth',
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
      'let { pell-nth } = import("TEMP-nth");\npell-nth(5)',
      'let { pell-nth } = import("TEMP-nth");\npell-nth(10)',
      'let { pell-nth } = import("TEMP-nth");\npell-nth(20)',
    ],
  },
  'TEMP-nth.pell?': {
    title: 'TEMP-nth.pell?',
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
      'let { pell? } = import("TEMP-nth");\npell?(1)',
      'let { pell? } = import("TEMP-nth");\npell?(470832)',
      'let { pell? } = import("TEMP-nth");\npell?(10)',
    ],
  },
}
