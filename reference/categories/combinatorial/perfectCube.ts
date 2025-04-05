import type { CombinatorialSequenceReference } from '.'

export const perfectCubeReference: CombinatorialSequenceReference<'perfect-cube'> = {
  'c:perfect-cube-seq': {
    title: 'c:perfect-cube-seq',
    category: 'Combinatorial',
    description: 'Generates the perfect cube numbers up to a specified length.',
    linkName: 'c-colon-perfect-cube-seq',
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
      'c:perfect-cube-seq(5)',
      'c:perfect-cube-seq(20)',
    ],
  },
  'c:perfect-cube-take-while': {
    title: 'c:perfect-cube-take-while',
    category: 'Combinatorial',
    description: 'Generates the perfect cube numbers while a condition is met.',
    linkName: 'c-colon-perfect-cube-take-while',
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
      'c:perfect-cube-take-while(-> $ <= 100)',
    ],
  },
  'c:perfect-cube-nth': {
    title: 'c:perfect-cube-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the perfect cube numbers.',
    linkName: 'c-colon-perfect-cube-nth',
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
      'c:perfect-cube-nth(1)',
      'c:perfect-cube-nth(5)',
    ],
  },
  'c:perfect-cube?': {
    title: 'c:perfect-cube?',
    category: 'Combinatorial',
    description: 'Checks if a number is in the perfect cube numbers.',
    linkName: 'c-colon-perfect-cube?',
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
      'c:perfect-cube?(7)',
      'c:perfect-cube?(8)',
      'c:perfect-cube?(9)',
    ],
  },
}
