import type { CombinatorialSequenceReference } from '.'

export const luckyReference: CombinatorialSequenceReference<'lucky'> = {
  'c:lucky-seq': {
    title: 'c:lucky-seq',
    category: 'Combinatorial',
    description: 'Generates the lucky sequence up to a specified length.',
    linkName: 'c-colon-lucky-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 77 (the maximum length of the pre-calculated lucky numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'c:lucky-seq(1)',
      'c:lucky-seq(2)',
      'c:lucky-seq(20)',
    ],
  },
  'c:lucky-take-while': {
    title: 'c:lucky-take-while',
    category: 'Combinatorial',
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
      'c:lucky-take-while(-> $ < 100)',
    ],
  },
  'c:lucky-nth': {
    title: 'c:lucky-nth',
    category: 'Combinatorial',
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
      'c:lucky-nth(1)',
      'c:lucky-nth(2)',
      'c:lucky-nth(20)',
    ],
  },
  'c:lucky?': {
    title: 'c:lucky?',
    category: 'Combinatorial',
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
      'c:lucky?(4)',
      'c:lucky?(7)',
      'c:lucky?(33)',
    ],
  },
}
