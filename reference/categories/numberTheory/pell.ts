import type { NumberTheorySequenceReference } from '.'

export const pellReference: NumberTheorySequenceReference<'pell'> = {
  'nth:pell-seq': {
    title: 'nth:pell-seq',
    category: 'Number Theory',
    description: 'Generates the Pell sequence up to a specified length.',
    linkName: 'nth-colon-pell-seq',
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
      'nth:pell-seq(5)',
      'nth:pell-seq(10)',
      'nth:pell-seq()',
    ],
  },
  'nth:pell-take-while': {
    title: 'nth:pell-take-while',
    category: 'Number Theory',
    description: 'Generates the Pell sequence while a condition is met.',
    linkName: 'nth-colon-pell-take-while',
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
      'nth:pell-take-while(-> $ < 1000)',
    ],
  },
  'nth:pell-nth': {
    title: 'nth:pell-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Pell sequence.',
    linkName: 'nth-colon-pell-nth',
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
      'nth:pell-nth(5)',
      'nth:pell-nth(10)',
      'nth:pell-nth(20)',
    ],
  },
  'nth:pell?': {
    title: 'nth:pell?',
    category: 'Number Theory',
    description: 'Checks if a number is a Pell number.',
    linkName: 'nth-colon-pell-question',
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
      'nth:pell?(1)',
      'nth:pell?(470832)',
      'nth:pell?(10)',
    ],
  },
}
