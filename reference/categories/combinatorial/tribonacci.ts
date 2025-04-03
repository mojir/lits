import type { CombinatorialSequenceReference } from '.'

export const tribonacciReference: CombinatorialSequenceReference<'tribonacci'> = {
  'c:tribonacci-seq': {
    title: 'c:tribonacci-seq',
    category: 'Combinatorial',
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
      'c:tribonacci-seq(1)',
      'c:tribonacci-seq(2)',
      'c:tribonacci-seq(10)',
    ],
  },
  'c:tribonacci-take-while': {
    title: 'c:tribonacci-take-while',
    category: 'Combinatorial',
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
      'c:tribonacci-take-while(-> $ < 100)',
    ],
  },
  'c:tribonacci-nth': {
    title: 'c:tribonacci-nth',
    category: 'Combinatorial',
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
      'c:tribonacci-nth(1)',
      'c:tribonacci-nth(2)',
      'c:tribonacci-nth(10)',
    ],
  },
  'c:tribonacci?': {
    title: 'c:tribonacci?',
    category: 'Combinatorial',
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
      'c:tribonacci?(0)',
      'c:tribonacci?(1)',
      'c:tribonacci?(2)',
      'c:tribonacci?(3)',
      'c:tribonacci?(4)',
      'c:tribonacci?(5)',
      'c:tribonacci?(6)',
      'c:tribonacci?(7)',
      'c:tribonacci?(8)',
      'c:tribonacci?(9)',
      'c:tribonacci?(10)',
    ],
  },
}
