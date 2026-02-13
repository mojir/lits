import type { NumberTheorySequenceReference } from '.'

export const mersenneReference: NumberTheorySequenceReference<'mersenne'> = {
  'Number-Theory.mersenne-seq': {
    title: 'Number-Theory.mersenne-seq',
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
      'let { mersenne-seq } = import("Number-Theory");\nmersenne-seq(1)',
      'let { mersenne-seq } = import("Number-Theory");\nmersenne-seq(5)',
      'let { mersenne-seq } = import("Number-Theory");\nmersenne-seq()',
    ],
  },
  'Number-Theory.mersenne-take-while': {
    title: 'Number-Theory.mersenne-take-while',
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
      'let { mersenne-take-while } = import("Number-Theory");\nmersenne-take-while(-> $ < 1000)',
    ],
  },
  'Number-Theory.mersenne-nth': {
    title: 'Number-Theory.mersenne-nth',
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
      'let { mersenne-nth } = import("Number-Theory");\nmersenne-nth(1)',
      'let { mersenne-nth } = import("Number-Theory");\nmersenne-nth(5)',
    ],
  },
  'Number-Theory.mersenne?': {
    title: 'Number-Theory.mersenne?',
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
      'let { mersenne? } = import("Number-Theory");\nmersenne?(3)',
      'let { mersenne? } = import("Number-Theory");\nmersenne?(4)',
      'let { mersenne? } = import("Number-Theory");\nmersenne?(7)',
    ],
  },
}
