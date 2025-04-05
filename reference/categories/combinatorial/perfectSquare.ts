import type { CombinatorialSequenceReference } from '.'

export const perfectSquareReference: CombinatorialSequenceReference<'perfect-square'> = {
  'c:perfect-square-seq': {
    title: 'c:perfect-square-seq',
    category: 'Combinatorial',
    description: 'Generates the perfect square numbers up to a specified length.',
    linkName: 'c-colon-perfect-square-seq',
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
      'c:perfect-square-seq(5)',
      'c:perfect-square-seq(20)',
    ],
  },
  'c:perfect-square-take-while': {
    title: 'c:perfect-square-take-while',
    category: 'Combinatorial',
    description: 'Generates the perfect square numbers while a condition is met.',
    linkName: 'c-colon-perfect-square-take-while',
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
      'c:perfect-square-take-while(-> $ <= 100)',
    ],
  },
  'c:perfect-square-nth': {
    title: 'c:perfect-square-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the perfect square numbers.',
    linkName: 'c-colon-perfect-square-nth',
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
      'c:perfect-square-nth(1)',
      'c:perfect-square-nth(5)',
    ],
  },
  'c:perfect-square?': {
    title: 'c:perfect-square?',
    category: 'Combinatorial',
    description: 'Checks if a number is a perfect square.',
    linkName: 'c-colon-perfect-square',
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
      'c:perfect-square?(16)',
      'c:perfect-square?(20)',
    ],
  },
}
