import type { CombinatorialSequenceReference } from '.'

export const pellReference: CombinatorialSequenceReference<'pell'> = {
  'c:pell-seq': {
    title: 'c:pell-seq',
    category: 'Combinatorial',
    description: 'Generates the Pell sequence up to a specified length.',
    linkName: 'c-colon-pell-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 43 (the maximum length of the pre-calculated Pell numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'c:pell-seq(5)',
      'c:pell-seq(10)',
      'c:pell-seq()',
    ],
  },
  'c:pell-take-while': {
    title: 'c:pell-take-while',
    category: 'Combinatorial',
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
      'c:pell-take-while(-> $ < 1000)',
    ],
  },
  'c:pell-nth': {
    title: 'c:pell-nth',
    category: 'Combinatorial',
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
      'c:pell-nth(5)',
      'c:pell-nth(10)',
      'c:pell-nth(20)',
    ],
  },
  'c:pell?': {
    title: 'c:pell?',
    category: 'Combinatorial',
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
      'c:pell?(1)',
      'c:pell?(470832)',
      'c:pell?(10)',
    ],
  },
}
