import type { CombinatorialSequenceReference } from '.'

export const perfectReference: CombinatorialSequenceReference<'perfect'> = {
  'c:perfect-seq': {
    title: 'c:perfect-seq',
    category: 'Combinatorial',
    description: 'Generates the perfect numbers up to a specified length.',
    linkName: 'c-colon-perfect-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If no length is provided, it defaults to 7 (the maximum length of the pre-calculated perfect numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'c:perfect-seq(1)',
      'c:perfect-seq(5)',
      'c:perfect-seq()',
    ],
  },
  'c:perfect-take-while': {
    title: 'c:perfect-take-while',
    category: 'Combinatorial',
    description: 'Generates the perfect numbers while a condition is met.',
    linkName: 'c-colon-perfect-take-while',
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
      'c:perfect-take-while(-> $ < 1000)',
    ],
  },
  'c:perfect-nth': {
    title: 'c:perfect-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the perfect numbers.',
    linkName: 'c-colon-perfect-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the perfect number to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'c:perfect-nth(1)',
      'c:perfect-nth(5)',
    ],
  },
  'c:perfect?': {
    title: 'c:perfect?',
    category: 'Combinatorial',
    description: 'Checks if a number is in the perfect numbers.',
    linkName: 'c-colon-perfect-question-mark',
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
      'c:perfect?(0)',
      'c:perfect?(1)',
      'c:perfect?(2)',
      'c:perfect?(3)',
      'c:perfect?(4)',
      'c:perfect?(5)',
      'c:perfect?(6)',
      'c:perfect?(7)',
      'c:perfect?(8)',
      'c:perfect?(9)',
    ],
  },
}
