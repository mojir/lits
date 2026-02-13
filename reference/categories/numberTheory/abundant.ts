import type { NumberTheorySequenceReference } from '.'

export const abundantReference: NumberTheorySequenceReference<'abundant'> = {
  'Number-Theory.abundant-seq': {
    title: 'Number-Theory.abundant-seq',
    category: 'Number Theory',
    description: 'Generates the abundant numbers up to a specified length.',
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
      'let { abundant-seq } = import("Number-Theory");\nabundant-seq(1)',
      'let { abundant-seq } = import("Number-Theory");\nabundant-seq(5)',
    ],
  },
  'Number-Theory.abundant-take-while': {
    title: 'Number-Theory.abundant-take-while',
    category: 'Number Theory',
    description: 'Generates the abundant numbers while a condition is met.',
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
      'let { abundant-take-while } = import("Number-Theory");\nabundant-take-while(-> $ < 100)',
    ],
  },
  'Number-Theory.abundant-nth': {
    title: 'Number-Theory.abundant-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the abundant numbers.',
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
      'let { abundant-nth } = import("Number-Theory");\nabundant-nth(1)',
      'let { abundant-nth } = import("Number-Theory");\nabundant-nth(5)',
    ],
  },
  'Number-Theory.abundant?': {
    title: 'Number-Theory.abundant?',
    category: 'Number Theory',
    description: 'Checks if a number is abundant.',
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
      'let { abundant? } = import("Number-Theory");\nabundant?(12)',
      'let { abundant? } = import("Number-Theory");\nabundant?(15)',
    ],
  },
}
