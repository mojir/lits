import type { CombinatorialSequenceReference } from '.'

export const recamanReference: CombinatorialSequenceReference<'recaman'> = {
  'c:recaman-seq': {
    title: 'c:recaman-seq',
    category: 'Combinatorial',
    description: 'Generates the Recaman sequence up to a specified length.',
    linkName: 'c-colon-recaman-seq',
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
      'c:recaman-seq(5)',
      'c:recaman-seq(10)',
      'c:recaman-seq(20)',
    ],
  },
  'c:recaman-take-while': {
    title: 'c:recaman-take-while',
    category: 'Combinatorial',
    description: 'Generates the Recaman sequence while a condition is met.',
    linkName: 'c-colon-recaman-take-while',
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
      'c:recaman-take-while(-> $ < 10)',
    ],
  },
  'c:recaman-nth': {
    title: 'c:recaman-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the Recaman sequence.',
    linkName: 'c-colon-recaman-nth',
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
      'c:recaman-nth(5)',
      'c:recaman-nth(10)',
      'c:recaman-nth(20)',
    ],
  },
  'c:recaman?': {
    title: 'c:recaman?',
    category: 'Combinatorial',
    description: 'Checks if a number is in the Recaman sequence.',
    linkName: 'c-colon-recaman-question-mark',
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
      'c:recaman?(5)',
      'c:recaman?(10)',
      'c:recaman?(20)',
    ],
  },
}
