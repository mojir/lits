import type { NumberTheorySequenceReference } from '.'

export const bellReference: NumberTheorySequenceReference<'bell'> = {
  'TEMP-nth.bell-seq': {
    title: 'TEMP-nth.bell-seq',
    category: 'Number Theory',
    description: 'Generates the Bell sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 22 (the maximum length of the pre-calculated bell numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let { bell-seq } = import("TEMP-nth");\nbell-seq(5)',
      'let { bell-seq } = import("TEMP-nth");\nbell-seq(10)',
      'let { bell-seq } = import("TEMP-nth");\nbell-seq()',
    ],
  },
  'TEMP-nth.bell-take-while': {
    title: 'TEMP-nth.bell-take-while',
    category: 'Number Theory',
    description: 'Generates the Bell sequence while a condition is met.',
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
      'let { bell-take-while } = import("TEMP-nth");\nbell-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.bell-nth': {
    title: 'TEMP-nth.bell-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Bell sequence.',
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
      'let { bell-nth } = import("TEMP-nth");\nbell-nth(5)',
      'let { bell-nth } = import("TEMP-nth");\nbell-nth(10)',
    ],
  },
  'TEMP-nth.bell?': {
    title: 'TEMP-nth.bell?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Bell sequence.',
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
      'let { bell? } = import("TEMP-nth");\nbell?(1)',
      'let { bell? } = import("TEMP-nth");\nbell?(27644437)',
      'let { bell? } = import("TEMP-nth");\nbell?(27644436)',
    ],
  },
}
