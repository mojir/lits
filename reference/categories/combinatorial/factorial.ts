import type { CombinatorialSequenceReference } from '.'

export const factorialReference: CombinatorialSequenceReference<'factorial'> = {
  'c:factorial-seq': {
    title: 'c:factorial-seq',
    category: 'Combinatorial',
    description: 'Generates the factorial sequence up to a specified length.',
    linkName: 'c-colon-factorial-seq',
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
      'c:factorial-seq(1)',
      'c:factorial-seq(2)',
      'c:factorial-seq(3)',
      'c:factorial-seq(4)',
      'c:factorial-seq(5)',
      'c:factorial-seq(10)',
    ],
  },
  'c:factorial-take-while': {
    title: 'c:factorial-take-while',
    category: 'Combinatorial',
    description: 'Generates the factorial sequence while a condition is met.',
    linkName: 'c-colon-factorial-take-while',
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
      'c:factorial-take-while(-> $ < 1000)',
    ],
  },
  'c:factorial-nth': {
    title: 'c:factorial-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the factorial sequence.',
    linkName: 'c-colon-factorial-nth',
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
      'c:factorial-nth(1)',
      'c:factorial-nth(2)',
      'c:factorial-nth(3)',
      'c:factorial-nth(4)',
      'c:factorial-nth(5)',
      'c:factorial-nth(10)',
    ],
  },
  'c:factorial?': {
    title: 'c:factorial?',
    category: 'Combinatorial',
    description: 'Checks if a number is in the factorial sequence.',
    linkName: 'c-colon-factorial-question-mark',
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
      'c:factorial?(1)',
      'c:factorial?(2)',
      'c:factorial?(3)',
      'c:factorial?(4)',
      'c:factorial?(5)',
      'c:factorial?(6)',
      'c:factorial?(7)',
      'c:factorial?(8)',
      'c:factorial?(9)',
      'c:factorial?(3628800)',
    ],
  },
}
