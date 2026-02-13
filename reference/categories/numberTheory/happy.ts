import type { NumberTheorySequenceReference } from '.'

export const happyReference: NumberTheorySequenceReference<'happy'> = {
  'Number-Theory.happy-seq': {
    title: 'Number-Theory.happy-seq',
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
      'let { happy-seq } = import("Number-Theory");\nhappy-seq(1)',
      'let { happy-seq } = import("Number-Theory");\nhappy-seq(2)',
      'let { happy-seq } = import("Number-Theory");\nhappy-seq(20)',
    ],
  },
  'Number-Theory.happy-take-while': {
    title: 'Number-Theory.happy-take-while',
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
      'let { happy-take-while } = import("Number-Theory");\nhappy-take-while(-> $ < 100)',
    ],
  },
  'Number-Theory.happy-nth': {
    title: 'Number-Theory.happy-nth',
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
      'let { happy-nth } = import("Number-Theory");\nhappy-nth(1)',
      'let { happy-nth } = import("Number-Theory");\nhappy-nth(2)',
      'let { happy-nth } = import("Number-Theory");\nhappy-nth(20)',
    ],
  },
  'Number-Theory.happy?': {
    title: 'Number-Theory.happy?',
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
      'let { happy? } = import("Number-Theory");\nhappy?(1)',
      'let { happy? } = import("Number-Theory");\nhappy?(2)',
      'let { happy? } = import("Number-Theory");\nhappy?(100)',
    ],
  },
}
