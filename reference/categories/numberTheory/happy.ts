import type { NumberTheorySequenceReference } from '.'

export const happyReference: NumberTheorySequenceReference<'happy'> = {
  'TEMP-nth.happy-seq': {
    title: 'TEMP-nth.happy-seq',
    category: 'Number Theory',
    description: 'Generates the happy sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 20 (the maximum length of the pre-calculated happy numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let { happy-seq } = import("TEMP-nth");\nhappy-seq(1)',
      'let { happy-seq } = import("TEMP-nth");\nhappy-seq(2)',
      'let { happy-seq } = import("TEMP-nth");\nhappy-seq(20)',
    ],
  },
  'TEMP-nth.happy-take-while': {
    title: 'TEMP-nth.happy-take-while',
    category: 'Number Theory',
    description: 'Generates the happy sequence while a condition is met.',
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
      'let { happy-take-while } = import("TEMP-nth");\nhappy-take-while(-> $ < 100)',
    ],
  },
  'TEMP-nth.happy-nth': {
    title: 'TEMP-nth.happy-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the happy sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the happy number to return.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'let { happy-nth } = import("TEMP-nth");\nhappy-nth(1)',
      'let { happy-nth } = import("TEMP-nth");\nhappy-nth(2)',
      'let { happy-nth } = import("TEMP-nth");\nhappy-nth(20)',
    ],
  },
  'TEMP-nth.happy?': {
    title: 'TEMP-nth.happy?',
    category: 'Number Theory',
    description: 'Determines if a number is a happy number.',
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
      'let { happy? } = import("TEMP-nth");\nhappy?(1)',
      'let { happy? } = import("TEMP-nth");\nhappy?(2)',
      'let { happy? } = import("TEMP-nth");\nhappy?(100)',
    ],
  },
}
