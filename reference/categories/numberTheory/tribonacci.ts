import type { NumberTheorySequenceReference } from '.'

export const tribonacciReference: NumberTheorySequenceReference<'tribonacci'> = {
  'Number-Theory.tribonacci-seq': {
    title: 'Number-Theory.tribonacci-seq',
    category: 'Number Theory',
    description: 'Generates the tribonacci sequence up to a specified length.',
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
      'let { tribonacci-seq } = import("Number-Theory");\ntribonacci-seq(1)',
      'let { tribonacci-seq } = import("Number-Theory");\ntribonacci-seq(2)',
      'let { tribonacci-seq } = import("Number-Theory");\ntribonacci-seq(10)',
    ],
  },
  'Number-Theory.tribonacci-take-while': {
    title: 'Number-Theory.tribonacci-take-while',
    category: 'Number Theory',
    description: 'Generates the tribonacci sequence while a condition is met.',
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
      'let { tribonacci-take-while } = import("Number-Theory");\ntribonacci-take-while(-> $ < 100)',
    ],
  },
  'Number-Theory.tribonacci-nth': {
    title: 'Number-Theory.tribonacci-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the tribonacci sequence.',
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
      'let { tribonacci-nth } = import("Number-Theory");\ntribonacci-nth(1)',
      'let { tribonacci-nth } = import("Number-Theory");\ntribonacci-nth(2)',
      'let { tribonacci-nth } = import("Number-Theory");\ntribonacci-nth(10)',
    ],
  },
  'Number-Theory.tribonacci?': {
    title: 'Number-Theory.tribonacci?',
    category: 'Number Theory',
    description: 'Determines if a number is in the tribonacci sequence.',
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
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(0)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(1)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(2)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(3)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(4)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(5)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(6)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(7)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(8)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(9)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(10)',
    ],
  },
}
