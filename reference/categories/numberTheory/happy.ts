import type { NumberTheorySequenceReference } from '.'

export const happyReference: NumberTheorySequenceReference<'happy'> = {
  'nth:happy-seq': {
    title: 'nth:happy-seq',
    category: 'Number Theory',
    description: 'Generates the happy sequence up to a specified length.',
    linkName: 'c-colon-happy-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 20 (the maximum length of the pre-calculated happy numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'nth:happy-seq(1)',
      'nth:happy-seq(2)',
      'nth:happy-seq(20)',
    ],
  },
  'nth:happy-take-while': {
    title: 'nth:happy-take-while',
    category: 'Number Theory',
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
      'nth:happy-take-while(-> $ < 100)',
    ],
  },
  'nth:happy-nth': {
    title: 'nth:happy-nth',
    category: 'Number Theory',
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
      'nth:happy-nth(1)',
      'nth:happy-nth(2)',
      'nth:happy-nth(20)',
    ],
  },
  'nth:happy?': {
    title: 'nth:happy?',
    category: 'Number Theory',
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
      'nth:happy?(1)',
      'nth:happy?(2)',
      'nth:happy?(100)',
    ],
  },
}
