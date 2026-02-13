import type { NumberTheorySequenceReference } from '.'

export const compositeReference: NumberTheorySequenceReference<'composite'> = {
  'TEMP-nth.composite-seq': {
    title: 'TEMP-nth.composite-seq',
    category: 'Number Theory',
    description: 'Generates the composite sequence up to a specified length.',
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
      'let { composite-seq } = import("TEMP-nth");\ncomposite-seq(1)',
      'let { composite-seq } = import("TEMP-nth");\ncomposite-seq(2)',
      'let { composite-seq } = import("TEMP-nth");\ncomposite-seq(10)',
    ],
  },
  'TEMP-nth.composite-take-while': {
    title: 'TEMP-nth.composite-take-while',
    category: 'Number Theory',
    description: 'Generates the composite sequence while a condition is met.',
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
      'let { composite-take-while } = import("TEMP-nth");\ncomposite-take-while(-> $ < 50)',
    ],
  },
  'TEMP-nth.composite-nth': {
    title: 'TEMP-nth.composite-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the composite sequence.',
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
      'let { composite-nth } = import("TEMP-nth");\ncomposite-nth(1)',
      'let { composite-nth } = import("TEMP-nth");\ncomposite-nth(2)',
      'let { composite-nth } = import("TEMP-nth");\ncomposite-nth(10)',
    ],
  },
  'TEMP-nth.composite?': {
    title: 'TEMP-nth.composite?',
    category: 'Number Theory',
    description: 'Determines if a number is composite.',
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
      'let { composite? } = import("TEMP-nth");\ncomposite?(4)',
      'let { composite? } = import("TEMP-nth");\ncomposite?(5)',
      'let { composite? } = import("TEMP-nth");\ncomposite?(11)',
    ],
  },
}
