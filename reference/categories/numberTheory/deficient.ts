import type { NumberTheorySequenceReference } from '.'

export const deficientReference: NumberTheorySequenceReference<'deficient'> = {
  'TEMP-nth.deficient-seq': {
    title: 'TEMP-nth.deficient-seq',
    category: 'Number Theory',
    description: 'Generates the deficient numbers up to a specified length.',
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
      'let { deficient-seq } = import("TEMP-nth");\ndeficient-seq(1)',
      'let { deficient-seq } = import("TEMP-nth");\ndeficient-seq(5)',
    ],
  },
  'TEMP-nth.deficient-take-while': {
    title: 'TEMP-nth.deficient-take-while',
    category: 'Number Theory',
    description: 'Generates the deficient numbers while a condition is met.',
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
      'let { deficient-take-while } = import("TEMP-nth");\ndeficient-take-while(-> $ < 100)',
    ],
  },
  'TEMP-nth.deficient-nth': {
    title: 'TEMP-nth.deficient-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the deficient numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the number in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'let { deficient-nth } = import("TEMP-nth");\ndeficient-nth(5)',
      'let { deficient-nth } = import("TEMP-nth");\ndeficient-nth(12)',
    ],
  },
  'TEMP-nth.deficient?': {
    title: 'TEMP-nth.deficient?',
    category: 'Number Theory',
    description: 'Checks if a number is deficient.',
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
      'let { deficient? } = import("TEMP-nth");\ndeficient?(12)',
      'let { deficient? } = import("TEMP-nth");\ndeficient?(15)',
    ],
  },
}
