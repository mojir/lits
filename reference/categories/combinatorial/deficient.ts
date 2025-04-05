import type { CombinatorialSequenceReference } from '.'

export const deficientReference: CombinatorialSequenceReference<'deficient'> = {
  'c:deficient-seq': {
    title: 'c:deficient-seq',
    category: 'Combinatorial',
    description: 'Generates the deficient numbers up to a specified length.',
    linkName: 'c-colon-deficient-seq',
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
      'c:deficient-seq(1)',
      'c:deficient-seq(5)',
    ],
  },
  'c:deficient-take-while': {
    title: 'c:deficient-take-while',
    category: 'Combinatorial',
    description: 'Generates the deficient numbers while a condition is met.',
    linkName: 'c-colon-deficient-take-while',
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
      'c:deficient-take-while(-> $ < 100)',
    ],
  },
  'c:deficient-nth': {
    title: 'c:deficient-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the deficient numbers.',
    linkName: 'c-colon-deficient-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the number in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'c:deficient-nth(5)',
      'c:deficient-nth(12)',
    ],
  },
  'c:deficient?': {
    title: 'c:deficient?',
    category: 'Combinatorial',
    description: 'Checks if a number is deficient.',
    linkName: 'c-colon-deficient',
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
      'c:deficient?(12)',
      'c:deficient?(15)',
    ],
  },
}
