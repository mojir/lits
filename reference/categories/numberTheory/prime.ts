import type { NumberTheorySequenceReference } from '.'

export const primeReference: NumberTheorySequenceReference<'prime'> = {
  'Number-Theory.prime-seq': {
    title: 'Number-Theory.prime-seq',
    category: 'Number Theory',
    description: 'Generates the prime sequence up to a specified length.',
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
      'let { prime-seq } = import("Number-Theory");\nprime-seq(1)',
      'let { prime-seq } = import("Number-Theory");\nprime-seq(2)',
      'let { prime-seq } = import("Number-Theory");\nprime-seq(10)',
    ],
  },
  'Number-Theory.prime-take-while': {
    title: 'Number-Theory.prime-take-while',
    category: 'Number Theory',
    description: 'Generates the prime sequence while a condition is met.',
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
      'let { prime-take-while } = import("Number-Theory");\nprime-take-while(-> $ < 50)',
    ],
  },
  'Number-Theory.prime-nth': {
    title: 'Number-Theory.prime-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the prime sequence.',
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
      'let { prime-nth } = import("Number-Theory");\nprime-nth(1)',
      'let { prime-nth } = import("Number-Theory");\nprime-nth(2)',
      'let { prime-nth } = import("Number-Theory");\nprime-nth(10)',
    ],
  },
  'Number-Theory.prime?': {
    title: 'Number-Theory.prime?',
    category: 'Number Theory',
    description: 'Determines if a number is prime.',
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
      'let { prime? } = import("Number-Theory");\nprime?(1)',
      'let { prime? } = import("Number-Theory");\nprime?(2)',
      'let { prime? } = import("Number-Theory");\nprime?(3)',
      'let { prime? } = import("Number-Theory");\nprime?(4)',
      'let { prime? } = import("Number-Theory");\nprime?(997)',
      'let { prime? } = import("Number-Theory");\nprime?(1001)',
    ],
  },
}
