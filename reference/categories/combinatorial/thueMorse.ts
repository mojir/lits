import type { CombinatorialSequenceReference } from '.'

export const thueMorseReference: CombinatorialSequenceReference<'thue-morse'> = {
  'c:thue-morse-seq': {
    title: 'c:thue-morse-seq',
    category: 'Combinatorial',
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
      'c:thue-morse-seq(5)',
      'c:thue-morse-seq(10)',
      'c:thue-morse-seq(20)',
    ],
  },
  'c:thue-morse-take-while': {
    title: 'c:thue-morse-take-while',
    category: 'Combinatorial',
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
      'c:thue-morse-take-while(-> $2 < 10)',
    ],
  },
  'c:thue-morse-nth': {
    title: 'c:thue-morse-nth',
    category: 'Combinatorial',
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
      'c:thue-morse-nth(5)',
      'c:thue-morse-nth(10)',
      'c:thue-morse-nth(20)',
    ],
  },
  'c:thue-morse?': {
    title: 'c:thue-morse?',
    category: 'Combinatorial',
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
      'c:thue-morse?(1)',
      'c:thue-morse?(2)',
    ],
  },
}
