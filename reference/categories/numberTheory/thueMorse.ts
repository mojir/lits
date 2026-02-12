import type { NumberTheorySequenceReference } from '.'

export const thueMorseReference: NumberTheorySequenceReference<'thue-morse'> = {
  'nth.thue-morse-seq': {
    title: 'nth.thue-morse-seq',
    category: 'Number Theory',
    description: 'Generates the Thue-Morse sequence up to a specified length.',
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
      'nth.thue-morse-seq(5)',
      'nth.thue-morse-seq(10)',
      'nth.thue-morse-seq(20)',
    ],
  },
  'nth.thue-morse-take-while': {
    title: 'nth.thue-morse-take-while',
    category: 'Number Theory',
    description: 'Generates the Thue-Morse sequence while a condition is met.',
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
      'nth.thue-morse-take-while(-> $2 < 10)',
    ],
  },
  'nth.thue-morse-nth': {
    title: 'nth.thue-morse-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Thue-Morse sequence.',
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
      'nth.thue-morse-nth(5)',
      'nth.thue-morse-nth(10)',
      'nth.thue-morse-nth(20)',
    ],
  },
  'nth.thue-morse?': {
    title: 'nth.thue-morse?',
    category: 'Number Theory',
    description: 'Checks if a number is part of the Thue-Morse sequence.',
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
      'nth.thue-morse?(1)',
      'nth.thue-morse?(2)',
    ],
  },
}
