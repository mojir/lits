import type { NumberTheorySequenceReference } from '.'

export const tribonacciReference: NumberTheorySequenceReference<'tribonacci'> = {
  'nth:tribonacci-seq': {
    title: 'nth:tribonacci-seq',
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
      'nth:tribonacci-seq(1)',
      'nth:tribonacci-seq(2)',
      'nth:tribonacci-seq(10)',
    ],
  },
  'nth:tribonacci-take-while': {
    title: 'nth:tribonacci-take-while',
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
      'nth:tribonacci-take-while(-> $ < 100)',
    ],
  },
  'nth:tribonacci-nth': {
    title: 'nth:tribonacci-nth',
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
      'nth:tribonacci-nth(1)',
      'nth:tribonacci-nth(2)',
      'nth:tribonacci-nth(10)',
    ],
  },
  'nth:tribonacci?': {
    title: 'nth:tribonacci?',
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
      'nth:tribonacci?(0)',
      'nth:tribonacci?(1)',
      'nth:tribonacci?(2)',
      'nth:tribonacci?(3)',
      'nth:tribonacci?(4)',
      'nth:tribonacci?(5)',
      'nth:tribonacci?(6)',
      'nth:tribonacci?(7)',
      'nth:tribonacci?(8)',
      'nth:tribonacci?(9)',
      'nth:tribonacci?(10)',
    ],
  },
}
