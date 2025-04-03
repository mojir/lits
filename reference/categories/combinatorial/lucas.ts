import type { CombinatorialSequenceReference } from '.'

export const lucasReference: CombinatorialSequenceReference<'lucas'> = {
  'c:lucas-seq': {
    title: 'c:lucas-seq',
    category: 'Combinatorial',
    description: 'Generates the lucas sequence up to a specified length.',
    linkName: 'c-colon-lucas-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 77 (the maximum length of the pre-calculated Lucas numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'c:lucas-seq(1)',
      'c:lucas-seq(2)',
      'c:lucas-seq()',
    ],
  },
  'c:lucas-take-while': {
    title: 'c:lucas-take-while',
    category: 'Combinatorial',
    description: 'Generates the lucas sequence while a condition is met.',
    linkName: 'c-colon-lucas-take-while',
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
      'c:lucas-take-while(-> $ < 100)',
    ],
  },
  'c:lucas-nth': {
    title: 'c:lucas-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the lucas sequence.',
    linkName: 'c-colon-lucas-nth',
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
      'c:lucas-nth(1)',
      'c:lucas-nth(2)',
      'c:lucas-nth(10)',
    ],
  },
  'c:lucas?': {
    title: 'c:lucas?',
    category: 'Combinatorial',
    description: 'Determines if a number is in the lucas sequence.',
    linkName: 'c-colon-lucas-question-mark',
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
      'c:lucas?(1)',
      'c:lucas?(2)',
      'c:lucas?(10)',
    ],
  },
}
