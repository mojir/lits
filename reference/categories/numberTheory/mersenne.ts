import type { NumberTheorySequenceReference } from '.'

export const mersenneReference: NumberTheorySequenceReference<'mersenne'> = {
  'TEMP-nth.mersenne-seq': {
    title: 'TEMP-nth.mersenne-seq',
    category: 'Number Theory',
    description: 'Generates the Mersenne sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 9 (the maximum length of the pre-calculated mersenne numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let { mersenne-seq } = import("TEMP-nth");\nmersenne-seq(1)',
      'let { mersenne-seq } = import("TEMP-nth");\nmersenne-seq(5)',
      'let { mersenne-seq } = import("TEMP-nth");\nmersenne-seq()',
    ],
  },
  'TEMP-nth.mersenne-take-while': {
    title: 'TEMP-nth.mersenne-take-while',
    category: 'Number Theory',
    description: 'Generates the Mersenne sequence while a condition is met.',
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
      'let { mersenne-take-while } = import("TEMP-nth");\nmersenne-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.mersenne-nth': {
    title: 'TEMP-nth.mersenne-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Mersenne sequence.',
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
      'let { mersenne-nth } = import("TEMP-nth");\nmersenne-nth(1)',
      'let { mersenne-nth } = import("TEMP-nth");\nmersenne-nth(5)',
    ],
  },
  'TEMP-nth.mersenne?': {
    title: 'TEMP-nth.mersenne?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Mersenne sequence.',
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
      'let { mersenne? } = import("TEMP-nth");\nmersenne?(3)',
      'let { mersenne? } = import("TEMP-nth");\nmersenne?(4)',
      'let { mersenne? } = import("TEMP-nth");\nmersenne?(7)',
    ],
  },
}
