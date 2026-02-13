import type { NumberTheorySequenceReference } from '.'

export const fibonacciReference: NumberTheorySequenceReference<'fibonacci'> = {
  'TEMP-nth.fibonacci-seq': {
    title: 'TEMP-nth.fibonacci-seq',
    category: 'Number Theory',
    description: 'Generates the fibonacci sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 79 (the maximum length of the pre-calculated Fibonacci numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.fibonacci-seq(1)',
      'let nt = import("TEMP-nth");\nnt.fibonacci-seq(2)',
      'let nt = import("TEMP-nth");\nnt.fibonacci-seq()',
    ],
  },
  'TEMP-nth.fibonacci-take-while': {
    title: 'TEMP-nth.fibonacci-take-while',
    category: 'Number Theory',
    description: 'Generates the fibonacci sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.fibonacci-take-while(-> $ < 100)',
    ],
  },
  'TEMP-nth.fibonacci-nth': {
    title: 'TEMP-nth.fibonacci-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the fibonacci sequence.',
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
      'let nt = import("TEMP-nth");\nnt.fibonacci-nth(5)',
      'let nt = import("TEMP-nth");\nnt.fibonacci-nth(50)',
    ],
  },
  'TEMP-nth.fibonacci?': {
    title: 'TEMP-nth.fibonacci?',
    category: 'Number Theory',
    description: 'Determines if a number is in the fibonacci sequence.',
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
      'let nt = import("TEMP-nth");\nnt.fibonacci?(0)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(1)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(2)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(3)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(4)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(5)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(6)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(7)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(8)',
      'let nt = import("TEMP-nth");\nnt.fibonacci?(9)',
    ],
  },
}
