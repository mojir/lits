import type { NumberTheorySequenceReference } from '.'

export const factorialReference: NumberTheorySequenceReference<'factorial'> = {
  'TEMP-nth.factorial-seq': {
    title: 'TEMP-nth.factorial-seq',
    category: 'Number Theory',
    description: 'Generates the factorial sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 19 (the maximum length of the pre-calculated factorial numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let { factorial-seq } = import("TEMP-nth");\nfactorial-seq(1)',
      'let { factorial-seq } = import("TEMP-nth");\nfactorial-seq(2)',
      'let { factorial-seq } = import("TEMP-nth");\nfactorial-seq(3)',
      'let { factorial-seq } = import("TEMP-nth");\nfactorial-seq(4)',
      'let { factorial-seq } = import("TEMP-nth");\nfactorial-seq(5)',
      'let { factorial-seq } = import("TEMP-nth");\nfactorial-seq(10)',
    ],
  },
  'TEMP-nth.factorial-take-while': {
    title: 'TEMP-nth.factorial-take-while',
    category: 'Number Theory',
    description: 'Generates the factorial sequence while a condition is met.',
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
      'let { factorial-take-while } = import("TEMP-nth");\nfactorial-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.factorial-nth': {
    title: 'TEMP-nth.factorial-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the factorial sequence.',
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
      'let { factorial-nth } = import("TEMP-nth");\nfactorial-nth(1)',
      'let { factorial-nth } = import("TEMP-nth");\nfactorial-nth(2)',
      'let { factorial-nth } = import("TEMP-nth");\nfactorial-nth(3)',
      'let { factorial-nth } = import("TEMP-nth");\nfactorial-nth(4)',
      'let { factorial-nth } = import("TEMP-nth");\nfactorial-nth(5)',
      'let { factorial-nth } = import("TEMP-nth");\nfactorial-nth(10)',
    ],
  },
  'TEMP-nth.factorial?': {
    title: 'TEMP-nth.factorial?',
    category: 'Number Theory',
    description: 'Checks if a number is in the factorial sequence.',
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
      'let { factorial? } = import("TEMP-nth");\nfactorial?(1)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(2)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(3)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(4)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(5)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(6)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(7)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(8)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(9)',
      'let { factorial? } = import("TEMP-nth");\nfactorial?(3628800)',
    ],
  },
}
