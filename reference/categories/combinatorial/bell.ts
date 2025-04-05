import type { CombinatorialSequenceReference } from '.'

export const bellReference: CombinatorialSequenceReference<'bell'> = {
  'c:bell-seq': {
    title: 'c:bell-seq',
    category: 'Combinatorial',
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
      'c:bell-seq(5)',
      'c:bell-seq(10)',
      'c:bell-seq()',
    ],
  },
  'c:bell-take-while': {
    title: 'c:bell-take-while',
    category: 'Combinatorial',
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
      'c:bell-take-while(-> $ < 1000)',
    ],
  },
  'c:bell-nth': {
    title: 'c:bell-nth',
    category: 'Combinatorial',
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
      'c:bell-nth(5)',
      'c:bell-nth(10)',
      'c:bell-nth(23)',
    ],
  },
  'c:bell?': {
    title: 'c:bell?',
    category: 'Combinatorial',
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
      'c:bell?(1)',
      'c:bell?(27644437)',
      'c:bell?(27644436)',
    ],
  },
}
