import type { NumberTheorySequenceReference } from '.'

export const deficientReference: NumberTheorySequenceReference<'deficient'> = {
  'Number-Theory.deficient-seq': {
    title: 'Number-Theory.deficient-seq',
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
      'let { deficient-seq } = import("Number-Theory");\ndeficient-seq(1)',
      'let { deficient-seq } = import("Number-Theory");\ndeficient-seq(5)',
    ],
  },
  'Number-Theory.deficient-take-while': {
    title: 'Number-Theory.deficient-take-while',
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
      'let { deficient-take-while } = import("Number-Theory");\ndeficient-take-while(-> $ < 100)',
    ],
  },
  'Number-Theory.deficient-nth': {
    title: 'Number-Theory.deficient-nth',
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
      'let { deficient-nth } = import("Number-Theory");\ndeficient-nth(5)',
      'let { deficient-nth } = import("Number-Theory");\ndeficient-nth(12)',
    ],
  },
  'Number-Theory.deficient?': {
    title: 'Number-Theory.deficient?',
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
      'let { deficient? } = import("Number-Theory");\ndeficient?(12)',
      'let { deficient? } = import("Number-Theory");\ndeficient?(15)',
    ],
  },
}
