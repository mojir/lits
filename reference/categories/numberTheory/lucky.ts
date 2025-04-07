import type { NumberTheorySequenceReference } from '.'

export const luckyReference: NumberTheorySequenceReference<'lucky'> = {
  'n:lucky-seq': {
    title: 'n:lucky-seq',
    category: 'Number Theory',
    description: 'Generates the lucky sequence up to a specified length.',
    linkName: 'c-colon-lucky-seq',
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
      'n:lucky-seq(1)',
      'n:lucky-seq(2)',
      'n:lucky-seq(20)',
    ],
  },
  'n:lucky-take-while': {
    title: 'n:lucky-take-while',
    category: 'Number Theory',
    description: 'Generates the lucky sequence while a condition is met.',
    linkName: 'c-colon-lucky-take-while',
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
      'n:lucky-take-while(-> $ < 100)',
    ],
  },
  'n:lucky-nth': {
    title: 'n:lucky-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the lucky sequence.',
    linkName: 'c-colon-lucky-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The position in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:lucky-nth(1)',
      'n:lucky-nth(2)',
      'n:lucky-nth(20)',
    ],
  },
  'n:lucky?': {
    title: 'n:lucky?',
    category: 'Number Theory',
    description: 'Checks if a number is a lucky number.',
    linkName: 'c-colon-lucky-question-mark',
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
      'n:lucky?(4)',
      'n:lucky?(7)',
      'n:lucky?(33)',
    ],
  },
}
