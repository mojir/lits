import type { NumberTheorySequenceReference } from '.'

export const tribonacciReference: NumberTheorySequenceReference<'tribonacci'> = {
  'n:tribonacci-seq': {
    title: 'n:tribonacci-seq',
    category: 'Number Theory',
    description: 'Generates the tribonacci sequence up to a specified length.',
    linkName: 'c-colon-tribonacci-seq',
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
      'n:tribonacci-seq(1)',
      'n:tribonacci-seq(2)',
      'n:tribonacci-seq(10)',
    ],
  },
  'n:tribonacci-take-while': {
    title: 'n:tribonacci-take-while',
    category: 'Number Theory',
    description: 'Generates the tribonacci sequence while a condition is met.',
    linkName: 'c-colon-tribonacci-take-while',
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
      'n:tribonacci-take-while(-> $ < 100)',
    ],
  },
  'n:tribonacci-nth': {
    title: 'n:tribonacci-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the tribonacci sequence.',
    linkName: 'c-colon-tribonacci-nth',
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
      'n:tribonacci-nth(1)',
      'n:tribonacci-nth(2)',
      'n:tribonacci-nth(10)',
    ],
  },
  'n:tribonacci?': {
    title: 'n:tribonacci?',
    category: 'Number Theory',
    description: 'Determines if a number is in the tribonacci sequence.',
    linkName: 'c-colon-tribonacci-question',
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
      'n:tribonacci?(0)',
      'n:tribonacci?(1)',
      'n:tribonacci?(2)',
      'n:tribonacci?(3)',
      'n:tribonacci?(4)',
      'n:tribonacci?(5)',
      'n:tribonacci?(6)',
      'n:tribonacci?(7)',
      'n:tribonacci?(8)',
      'n:tribonacci?(9)',
      'n:tribonacci?(10)',
    ],
  },
}
