import type { NumberTheorySequenceReference } from '.'

export const luckyReference: NumberTheorySequenceReference<'lucky'> = {
  'TEMP-nth.lucky-seq': {
    title: 'TEMP-nth.lucky-seq',
    category: 'Number Theory',
    description: 'Generates the lucky sequence up to a specified length.',
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
      'let nt = import("TEMP-nth");\nnt.lucky-seq(1)',
      'let nt = import("TEMP-nth");\nnt.lucky-seq(2)',
      'let nt = import("TEMP-nth");\nnt.lucky-seq(20)',
    ],
  },
  'TEMP-nth.lucky-take-while': {
    title: 'TEMP-nth.lucky-take-while',
    category: 'Number Theory',
    description: 'Generates the lucky sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.lucky-take-while(-> $ < 100)',
    ],
  },
  'TEMP-nth.lucky-nth': {
    title: 'TEMP-nth.lucky-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the lucky sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The position in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.lucky-nth(1)',
      'let nt = import("TEMP-nth");\nnt.lucky-nth(2)',
      'let nt = import("TEMP-nth");\nnt.lucky-nth(20)',
    ],
  },
  'TEMP-nth.lucky?': {
    title: 'TEMP-nth.lucky?',
    category: 'Number Theory',
    description: 'Checks if a number is a lucky number.',
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
      'let nt = import("TEMP-nth");\nnt.lucky?(4)',
      'let nt = import("TEMP-nth");\nnt.lucky?(7)',
      'let nt = import("TEMP-nth");\nnt.lucky?(33)',
    ],
  },
}
