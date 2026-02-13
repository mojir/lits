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
      'let nt = import("TEMP-nth");\nnt.mersenne-seq(1)',
      'let nt = import("TEMP-nth");\nnt.mersenne-seq(5)',
      'let nt = import("TEMP-nth");\nnt.mersenne-seq()',
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
      'let nt = import("TEMP-nth");\nnt.mersenne-take-while(-> $ < 1000)',
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
      'let nt = import("TEMP-nth");\nnt.mersenne-nth(1)',
      'let nt = import("TEMP-nth");\nnt.mersenne-nth(5)',
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
      'let nt = import("TEMP-nth");\nnt.mersenne?(3)',
      'let nt = import("TEMP-nth");\nnt.mersenne?(4)',
      'let nt = import("TEMP-nth");\nnt.mersenne?(7)',
    ],
  },
}
