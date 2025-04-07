import type { NumberTheorySequenceReference } from '.'

export const sylvesterReference: NumberTheorySequenceReference<'sylvester'> = {
  'n:sylvester-seq': {
    title: 'n:sylvester-seq',
    category: 'Number Theory',
    description: 'Generates the Sylvester sequence up to a specified length.',
    linkName: 'c-colon-sylvester-seq',
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
      'n:sylvester-seq(5)',
      'n:sylvester-seq()',
      'n:sylvester-seq()',
    ],
  },
  'n:sylvester-take-while': {
    title: 'n:sylvester-take-while',
    category: 'Number Theory',
    description: 'Generates the Sylvester sequence while a condition is met.',
    linkName: 'c-colon-sylvester-take-while',
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
      'n:sylvester-take-while(-> $ < 100000)',
    ],
  },
  'n:sylvester-nth': {
    title: 'n:sylvester-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Sylvester sequence.',
    linkName: 'c-colon-sylvester-nth',
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
      'n:sylvester-nth(1)',
      'n:sylvester-nth(5)',
    ],
  },
  'n:sylvester?': {
    title: 'n:sylvester?',
    category: 'Number Theory',
    description: 'Checks if a number is in the Sylvester sequence.',
    linkName: 'c-colon-sylvester-question-mark',
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
      'n:sylvester?(2)',
      'n:sylvester?(3)',
      'n:sylvester?(6)',
    ],
  },
}
