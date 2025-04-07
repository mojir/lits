import type { NumberTheorySequenceReference } from '.'

export const thueMorseReference: NumberTheorySequenceReference<'thue-morse'> = {
  'n:thue-morse-seq': {
    title: 'n:thue-morse-seq',
    category: 'Number Theory',
    description: 'Generates the Thue-Morse sequence up to a specified length.',
    linkName: 'c-colon-thue-morse-seq',
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
      'n:thue-morse-seq(5)',
      'n:thue-morse-seq(10)',
      'n:thue-morse-seq(20)',
    ],
  },
  'n:thue-morse-take-while': {
    title: 'n:thue-morse-take-while',
    category: 'Number Theory',
    description: 'Generates the Thue-Morse sequence while a condition is met.',
    linkName: 'c-colon-thue-morse-take-while',
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
      'n:thue-morse-take-while(-> $2 < 10)',
    ],
  },
  'n:thue-morse-nth': {
    title: 'n:thue-morse-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Thue-Morse sequence.',
    linkName: 'c-colon-thue-morse-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:thue-morse-nth(5)',
      'n:thue-morse-nth(10)',
      'n:thue-morse-nth(20)',
    ],
  },
  'n:thue-morse?': {
    title: 'n:thue-morse?',
    category: 'Number Theory',
    description: 'Checks if a number is part of the Thue-Morse sequence.',
    linkName: 'c-colon-thue-morse-question',
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
      'n:thue-morse?(1)',
      'n:thue-morse?(2)',
    ],
  },
}
