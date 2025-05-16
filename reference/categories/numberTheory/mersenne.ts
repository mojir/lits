import type { NumberTheorySequenceReference } from '.'

export const mersenneReference: NumberTheorySequenceReference<'mersenne'> = {
  'nth:mersenne-seq': {
    title: 'nth:mersenne-seq',
    category: 'Number Theory',
    description: 'Generates the Mersenne sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 9 (the maximum length of the pre-calculated mersenne numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'nth:mersenne-seq(1)',
      'nth:mersenne-seq(5)',
      'nth:mersenne-seq()',
    ],
  },
  'nth:mersenne-take-while': {
    title: 'nth:mersenne-take-while',
    category: 'Number Theory',
    description: 'Generates the Mersenne sequence while a condition is met.',
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
      'nth:mersenne-take-while(-> $ < 1000)',
    ],
  },
  'nth:mersenne-nth': {
    title: 'nth:mersenne-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Mersenne sequence.',
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
      'nth:mersenne-nth(1)',
      'nth:mersenne-nth(5)',
    ],
  },
  'nth:mersenne?': {
    title: 'nth:mersenne?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Mersenne sequence.',
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
      'nth:mersenne?(3)',
      'nth:mersenne?(4)',
      'nth:mersenne?(7)',
    ],
  },
}
