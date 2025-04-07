import type { NumberTheorySequenceReference } from '.'

export const pellReference: NumberTheorySequenceReference<'pell'> = {
  'n:pell-seq': {
    title: 'n:pell-seq',
    category: 'Number Theory',
    description: 'Generates the Pell sequence up to a specified length.',
    linkName: 'c-colon-pell-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 42 (the maximum length of the pre-calculated Pell numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'n:pell-seq(5)',
      'n:pell-seq(10)',
      'n:pell-seq()',
    ],
  },
  'n:pell-take-while': {
    title: 'n:pell-take-while',
    category: 'Number Theory',
    description: 'Generates the Pell sequence while a condition is met.',
    linkName: 'c-colon-pell-take-while',
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
      'n:pell-take-while(-> $ < 1000)',
    ],
  },
  'n:pell-nth': {
    title: 'n:pell-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Pell sequence.',
    linkName: 'c-colon-pell-nth',
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
      'n:pell-nth(5)',
      'n:pell-nth(10)',
      'n:pell-nth(20)',
    ],
  },
  'n:pell?': {
    title: 'n:pell?',
    category: 'Number Theory',
    description: 'Checks if a number is a Pell number.',
    linkName: 'c-colon-pell-question-mark',
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
      'n:pell?(1)',
      'n:pell?(470832)',
      'n:pell?(10)',
    ],
  },
}
