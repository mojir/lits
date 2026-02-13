import type { NumberTheorySequenceReference } from '.'

export const fibonacciReference: NumberTheorySequenceReference<'fibonacci'> = {
  'Number-Theory.fibonacci-seq': {
    title: 'Number-Theory.fibonacci-seq',
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
      'let { fibonacci-seq } = import("Number-Theory");\nfibonacci-seq(1)',
      'let { fibonacci-seq } = import("Number-Theory");\nfibonacci-seq(2)',
      'let { fibonacci-seq } = import("Number-Theory");\nfibonacci-seq()',
    ],
  },
  'Number-Theory.fibonacci-take-while': {
    title: 'Number-Theory.fibonacci-take-while',
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
      'let { fibonacci-take-while } = import("Number-Theory");\nfibonacci-take-while(-> $ < 100)',
    ],
  },
  'Number-Theory.fibonacci-nth': {
    title: 'Number-Theory.fibonacci-nth',
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
      'let { fibonacci-nth } = import("Number-Theory");\nfibonacci-nth(5)',
      'let { fibonacci-nth } = import("Number-Theory");\nfibonacci-nth(50)',
    ],
  },
  'Number-Theory.fibonacci?': {
    title: 'Number-Theory.fibonacci?',
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
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(0)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(1)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(2)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(3)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(4)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(5)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(6)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(7)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(8)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(9)',
    ],
  },
}
