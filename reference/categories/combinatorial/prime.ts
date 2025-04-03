import type { CombinatorialSequenceReference } from '.'

export const primeReference: CombinatorialSequenceReference<'prime'> = {
  'c:prime-seq': {
    title: 'c:prime-seq',
    category: 'Combinatorial',
    description: 'Generates the prime sequence up to a specified length.',
    linkName: 'c-colon-prime-seq',
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
      'c:prime-seq(1)',
      'c:prime-seq(2)',
      'c:prime-seq(10)',
    ],
  },
  'c:prime-take-while': {
    title: 'c:prime-take-while',
    category: 'Combinatorial',
    description: 'Generates the prime sequence while a condition is met.',
    linkName: 'c-colon-prime-take-while',
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
      'c:prime-take-while(-> $ < 50)',
    ],
  },
  'c:prime-nth': {
    title: 'c:prime-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the prime sequence.',
    linkName: 'c-colon-prime-nth',
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
      'c:prime-nth(1)',
      'c:prime-nth(2)',
      'c:prime-nth(10)',
    ],
  },
  'c:prime?': {
    title: 'c:prime?',
    category: 'Combinatorial',
    description: 'Determines if a number is prime.',
    linkName: 'c-colon-prime-question-mark',
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
      'c:prime?(1)',
      'c:prime?(2)',
      'c:prime?(3)',
      'c:prime?(4)',
      'c:prime?(997)',
      'c:prime?(1001)',
    ],
  },
}
