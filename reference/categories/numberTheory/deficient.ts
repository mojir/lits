import type { NumberTheorySequenceReference } from '.'

export const deficientReference: NumberTheorySequenceReference<'deficient'> = {
  'nth:deficient-seq': {
    title: 'nth:deficient-seq',
    category: 'Number Theory',
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
      'nth:deficient-seq(1)',
      'nth:deficient-seq(5)',
    ],
  },
  'nth:deficient-take-while': {
    title: 'nth:deficient-take-while',
    category: 'Number Theory',
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
      'nth:deficient-take-while(-> $ < 100)',
    ],
  },
  'nth:deficient-nth': {
    title: 'nth:deficient-nth',
    category: 'Number Theory',
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
      'nth:deficient-nth(5)',
      'nth:deficient-nth(12)',
    ],
  },
  'nth:deficient?': {
    title: 'nth:deficient?',
    category: 'Number Theory',
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
      'nth:deficient?(12)',
      'nth:deficient?(15)',
    ],
  },
}
