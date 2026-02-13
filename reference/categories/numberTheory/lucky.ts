import type { NumberTheorySequenceReference } from '.'

export const luckyReference: NumberTheorySequenceReference<'lucky'> = {
  'Number-Theory.lucky-seq': {
    title: 'Number-Theory.lucky-seq',
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
      'let { lucky-seq } = import("Number-Theory");\nlucky-seq(1)',
      'let { lucky-seq } = import("Number-Theory");\nlucky-seq(2)',
      'let { lucky-seq } = import("Number-Theory");\nlucky-seq(20)',
    ],
  },
  'Number-Theory.lucky-take-while': {
    title: 'Number-Theory.lucky-take-while',
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
      'let { lucky-take-while } = import("Number-Theory");\nlucky-take-while(-> $ < 100)',
    ],
  },
  'Number-Theory.lucky-nth': {
    title: 'Number-Theory.lucky-nth',
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
      'let { lucky-nth } = import("Number-Theory");\nlucky-nth(1)',
      'let { lucky-nth } = import("Number-Theory");\nlucky-nth(2)',
      'let { lucky-nth } = import("Number-Theory");\nlucky-nth(20)',
    ],
  },
  'Number-Theory.lucky?': {
    title: 'Number-Theory.lucky?',
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
      'let { lucky? } = import("Number-Theory");\nlucky?(4)',
      'let { lucky? } = import("Number-Theory");\nlucky?(7)',
      'let { lucky? } = import("Number-Theory");\nlucky?(33)',
    ],
  },
}
