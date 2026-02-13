import type { NumberTheorySequenceReference } from '.'

export const sylvesterReference: NumberTheorySequenceReference<'sylvester'> = {
  'TEMP-nth.sylvester-seq': {
    title: 'TEMP-nth.sylvester-seq',
    category: 'Number Theory',
    description: 'Generates the Sylvester sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 6 (the maximum length of the pre-calculated Sylvester numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.sylvester-seq(5)',
      'let nt = import("TEMP-nth");\nnt.sylvester-seq()',
      'let nt = import("TEMP-nth");\nnt.sylvester-seq()',
    ],
  },
  'TEMP-nth.sylvester-take-while': {
    title: 'TEMP-nth.sylvester-take-while',
    category: 'Number Theory',
    description: 'Generates the Sylvester sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.sylvester-take-while(-> $ < 100000)',
    ],
  },
  'TEMP-nth.sylvester-nth': {
    title: 'TEMP-nth.sylvester-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Sylvester sequence.',
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
      'let nt = import("TEMP-nth");\nnt.sylvester-nth(1)',
      'let nt = import("TEMP-nth");\nnt.sylvester-nth(5)',
    ],
  },
  'TEMP-nth.sylvester?': {
    title: 'TEMP-nth.sylvester?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Sylvester sequence.',
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
      'let nt = import("TEMP-nth");\nnt.sylvester?(2)',
      'let nt = import("TEMP-nth");\nnt.sylvester?(3)',
      'let nt = import("TEMP-nth");\nnt.sylvester?(6)',
    ],
  },
}
