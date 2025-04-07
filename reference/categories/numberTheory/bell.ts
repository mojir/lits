import type { NumberTheorySequenceReference } from '.'

export const bellReference: NumberTheorySequenceReference<'bell'> = {
  'n:bell-seq': {
    title: 'n:bell-seq',
    category: 'Number Theory',
    description: 'Generates the Bell sequence up to a specified length.',
    linkName: 'c-colon-bell-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 22 (the maximum length of the pre-calculated bell numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'n:bell-seq(5)',
      'n:bell-seq(10)',
      'n:bell-seq()',
    ],
  },
  'n:bell-take-while': {
    title: 'n:bell-take-while',
    category: 'Number Theory',
    description: 'Generates the Bell sequence while a condition is met.',
    linkName: 'c-colon-bell-take-while',
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
      'n:bell-take-while(-> $ < 1000)',
    ],
  },
  'n:bell-nth': {
    title: 'n:bell-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Bell sequence.',
    linkName: 'c-colon-bell-nth',
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
      'n:bell-nth(5)',
      'n:bell-nth(10)',
      'n:bell-nth(23)',
    ],
  },
  'n:bell?': {
    title: 'n:bell?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Bell sequence.',
    linkName: 'c-colon-bell-question-mark',
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
      'n:bell?(1)',
      'n:bell?(27644437)',
      'n:bell?(27644436)',
    ],
  },
}
