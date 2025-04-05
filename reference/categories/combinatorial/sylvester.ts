import type { CombinatorialSequenceReference } from '.'

export const sylvesterReference: CombinatorialSequenceReference<'sylvester'> = {
  'c:sylvester-seq': {
    title: 'c:sylvester-seq',
    category: 'Combinatorial',
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
      'c:sylvester-seq(5)',
      'c:sylvester-seq()',
      'c:sylvester-seq()',
    ],
  },
  'c:sylvester-take-while': {
    title: 'c:sylvester-take-while',
    category: 'Combinatorial',
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
      'c:sylvester-take-while(-> $ < 100000)',
    ],
  },
  'c:sylvester-nth': {
    title: 'c:sylvester-nth',
    category: 'Combinatorial',
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
      'c:sylvester-nth(1)',
      'c:sylvester-nth(5)',
    ],
  },
  'c:sylvester?': {
    title: 'c:sylvester?',
    category: 'Combinatorial',
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
      'c:sylvester?(2)',
      'c:sylvester?(3)',
      'c:sylvester?(6)',
    ],
  },
}
