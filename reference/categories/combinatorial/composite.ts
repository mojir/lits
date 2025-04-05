import type { CombinatorialSequenceReference } from '.'

export const compositeReference: CombinatorialSequenceReference<'composite'> = {
  'c:composite-seq': {
    title: 'c:composite-seq',
    category: 'Combinatorial',
    description: 'Generates the composite sequence up to a specified length.',
    linkName: 'c-colon-composite-seq',
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
      'c:composite-seq(1)',
      'c:composite-seq(2)',
      'c:composite-seq(10)',
    ],
  },
  'c:composite-take-while': {
    title: 'c:composite-take-while',
    category: 'Combinatorial',
    description: 'Generates the composite sequence while a condition is met.',
    linkName: 'c-colon-composite-take-while',
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
      'c:composite-take-while(-> $ < 50)',
    ],
  },
  'c:composite-nth': {
    title: 'c:composite-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the composite sequence.',
    linkName: 'c-colon-composite-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the composite number to retrieve.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'c:composite-nth(1)',
      'c:composite-nth(2)',
      'c:composite-nth(10)',
    ],
  },
  'c:composite?': {
    title: 'c:composite?',
    category: 'Combinatorial',
    description: 'Determines if a number is composite.',
    linkName: 'c-colon-composite-question-mark',
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
      'c:composite?(4)',
      'c:composite?(5)',
      'c:composite?(11)',
    ],
  },
}
