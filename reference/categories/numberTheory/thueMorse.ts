import type { NumberTheorySequenceReference } from '.'

export const thueMorseReference: NumberTheorySequenceReference<'thue-morse'> = {
  'TEMP-nth.thue-morse-seq': {
    title: 'TEMP-nth.thue-morse-seq',
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
      'let { thue-morse-seq } = import("TEMP-nth");\nthue-morse-seq(5)',
      'let { thue-morse-seq } = import("TEMP-nth");\nthue-morse-seq(10)',
      'let { thue-morse-seq } = import("TEMP-nth");\nthue-morse-seq(20)',
    ],
  },
  'TEMP-nth.thue-morse-take-while': {
    title: 'TEMP-nth.thue-morse-take-while',
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
      'let { thue-morse-take-while } = import("TEMP-nth");\nthue-morse-take-while(-> $2 < 10)',
    ],
  },
  'TEMP-nth.thue-morse-nth': {
    title: 'TEMP-nth.thue-morse-nth',
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
      'let { thue-morse-nth } = import("TEMP-nth");\nthue-morse-nth(5)',
      'let { thue-morse-nth } = import("TEMP-nth");\nthue-morse-nth(10)',
      'let { thue-morse-nth } = import("TEMP-nth");\nthue-morse-nth(20)',
    ],
  },
  'TEMP-nth.thue-morse?': {
    title: 'TEMP-nth.thue-morse?',
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
      'let { thue-morse? } = import("TEMP-nth");\nthue-morse?(1)',
      'let { thue-morse? } = import("TEMP-nth");\nthue-morse?(2)',
    ],
  },
}
