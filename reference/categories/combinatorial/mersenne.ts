import type { CombinatorialSequenceReference } from '.'

export const mersenneReference: CombinatorialSequenceReference<'mersenne'> = {
  'c:mersenne-seq': {
    title: 'c:mersenne-seq',
    category: 'Combinatorial',
    description: 'Generates the Mersenne sequence up to a specified length.',
    linkName: 'c-colon-mersenne-seq',
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
      'c:mersenne-seq(1)',
      'c:mersenne-seq(5)',
      'c:mersenne-seq()',
    ],
  },
  'c:mersenne-take-while': {
    title: 'c:mersenne-take-while',
    category: 'Combinatorial',
    description: 'Generates the Mersenne sequence while a condition is met.',
    linkName: 'c-colon-mersenne-take-while',
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
      'c:mersenne-take-while(-> $ < 1000)',
    ],
  },
  'c:mersenne-nth': {
    title: 'c:mersenne-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the Mersenne sequence.',
    linkName: 'c-colon-mersenne-nth',
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
      'c:mersenne-nth(1)',
      'c:mersenne-nth(5)',
    ],
  },
  'c:mersenne?': {
    title: 'c:mersenne?',
    category: 'Combinatorial',
    description: 'Checks if a number is in the Mersenne sequence.',
    linkName: 'c-colon-mersenne-question-mark',
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
      'c:mersenne?(3)',
      'c:mersenne?(4)',
      'c:mersenne?(7)',
    ],
  },
}
