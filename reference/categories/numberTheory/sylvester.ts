import type { NumberTheorySequenceReference } from '.'

export const sylvesterReference: NumberTheorySequenceReference<'sylvester'> = {
  'Number-Theory.sylvester-seq': {
    title: 'Number-Theory.sylvester-seq',
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
      'let { sylvester-seq } = import("Number-Theory");\nsylvester-seq(5)',
      'let { sylvester-seq } = import("Number-Theory");\nsylvester-seq()',
      'let { sylvester-seq } = import("Number-Theory");\nsylvester-seq()',
    ],
  },
  'Number-Theory.sylvester-take-while': {
    title: 'Number-Theory.sylvester-take-while',
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
      'let { sylvester-take-while } = import("Number-Theory");\nsylvester-take-while(-> $ < 100000)',
    ],
  },
  'Number-Theory.sylvester-nth': {
    title: 'Number-Theory.sylvester-nth',
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
      'let { sylvester-nth } = import("Number-Theory");\nsylvester-nth(1)',
      'let { sylvester-nth } = import("Number-Theory");\nsylvester-nth(5)',
    ],
  },
  'Number-Theory.sylvester?': {
    title: 'Number-Theory.sylvester?',
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
      'let { sylvester? } = import("Number-Theory");\nsylvester?(2)',
      'let { sylvester? } = import("Number-Theory");\nsylvester?(3)',
      'let { sylvester? } = import("Number-Theory");\nsylvester?(6)',
    ],
  },
}
