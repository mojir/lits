import type { NumberTheorySequenceReference } from '.'

export const pellReference: NumberTheorySequenceReference<'pell'> = {
  'Number-Theory.pell-seq': {
    title: 'Number-Theory.pell-seq',
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
      'let { pell-seq } = import("Number-Theory");\npell-seq(5)',
      'let { pell-seq } = import("Number-Theory");\npell-seq(10)',
      'let { pell-seq } = import("Number-Theory");\npell-seq()',
    ],
  },
  'Number-Theory.pell-take-while': {
    title: 'Number-Theory.pell-take-while',
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
      'let { pell-take-while } = import("Number-Theory");\npell-take-while(-> $ < 1000)',
    ],
  },
  'Number-Theory.pell-nth': {
    title: 'Number-Theory.pell-nth',
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
      'let { pell-nth } = import("Number-Theory");\npell-nth(5)',
      'let { pell-nth } = import("Number-Theory");\npell-nth(10)',
      'let { pell-nth } = import("Number-Theory");\npell-nth(20)',
    ],
  },
  'Number-Theory.pell?': {
    title: 'Number-Theory.pell?',
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
      'let { pell? } = import("Number-Theory");\npell?(1)',
      'let { pell? } = import("Number-Theory");\npell?(470832)',
      'let { pell? } = import("Number-Theory");\npell?(10)',
    ],
  },
}
