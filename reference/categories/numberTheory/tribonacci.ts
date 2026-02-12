import type { NumberTheorySequenceReference } from '.'

export const tribonacciReference: NumberTheorySequenceReference<'tribonacci'> = {
  'nth.tribonacci-seq': {
    title: 'nth.tribonacci-seq',
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
      'let nt = import("nth");\nnt.tribonacci-seq(1)',
      'let nt = import("nth");\nnt.tribonacci-seq(2)',
      'let nt = import("nth");\nnt.tribonacci-seq(10)',
    ],
  },
  'nth.tribonacci-take-while': {
    title: 'nth.tribonacci-take-while',
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
      'let nt = import("nth");\nnt.tribonacci-take-while(-> $ < 100)',
    ],
  },
  'nth.tribonacci-nth': {
    title: 'nth.tribonacci-nth',
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
      'let nt = import("nth");\nnt.tribonacci-nth(1)',
      'let nt = import("nth");\nnt.tribonacci-nth(2)',
      'let nt = import("nth");\nnt.tribonacci-nth(10)',
    ],
  },
  'nth.tribonacci?': {
    title: 'nth.tribonacci?',
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
      'let nt = import("nth");\nnt.tribonacci?(0)',
      'let nt = import("nth");\nnt.tribonacci?(1)',
      'let nt = import("nth");\nnt.tribonacci?(2)',
      'let nt = import("nth");\nnt.tribonacci?(3)',
      'let nt = import("nth");\nnt.tribonacci?(4)',
      'let nt = import("nth");\nnt.tribonacci?(5)',
      'let nt = import("nth");\nnt.tribonacci?(6)',
      'let nt = import("nth");\nnt.tribonacci?(7)',
      'let nt = import("nth");\nnt.tribonacci?(8)',
      'let nt = import("nth");\nnt.tribonacci?(9)',
      'let nt = import("nth");\nnt.tribonacci?(10)',
    ],
  },
}
