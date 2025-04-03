import type { CombinatorialSequenceReference } from '.'

export const happyReference: CombinatorialSequenceReference<'happy'> = {
  'c:happy-seq': {
    title: 'c:happy-seq',
    category: 'Combinatorial',
    description: 'Generates the happy sequence up to a specified length.',
    linkName: 'c-colon-happy-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 77 (the maximum length of the pre-calculated happy numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'c:happy-seq(1)',
      'c:happy-seq(2)',
      'c:happy-seq(20)',
    ],
  },
  'c:happy-take-while': {
    title: 'c:happy-take-while',
    category: 'Combinatorial',
    description: 'Generates the happy sequence while a condition is met.',
    linkName: 'c-colon-happy-take-while',
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
      'c:happy-take-while(-> $ < 100)',
    ],
  },
  'c:happy-nth': {
    title: 'c:happy-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the happy sequence.',
    linkName: 'c-colon-happy-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the happy number to return.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'c:happy-nth(1)',
      'c:happy-nth(2)',
      'c:happy-nth(20)',
    ],
  },
  'c:happy?': {
    title: 'c:happy?',
    category: 'Combinatorial',
    description: 'Determines if a number is a happy number.',
    linkName: 'c-colon-happy-question',
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
      'c:happy?(1)',
      'c:happy?(2)',
      'c:happy?(100)',
    ],
  },
}
