import type { NumberTheorySequenceReference } from '.'

export const perfectPowerReference: NumberTheorySequenceReference<'perfect-power'> = {
  'Number-Theory.perfect-power-seq': {
    title: 'Number-Theory.perfect-power-seq',
    category: 'Number Theory',
    description: 'Generates the perfect power numbers up to a specified length.',
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
      'let { perfect-power-seq } = import("Number-Theory");\nperfect-power-seq(5)',
      'let { perfect-power-seq } = import("Number-Theory");\nperfect-power-seq(20)',
    ],
  },
  'Number-Theory.perfect-power-take-while': {
    title: 'Number-Theory.perfect-power-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect power numbers while a condition is met.',
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
      'let { perfect-power-take-while } = import("Number-Theory");\nperfect-power-take-while(-> $ <= 100)',
    ],
  },
  'Number-Theory.perfect-power-nth': {
    title: 'Number-Theory.perfect-power-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect power numbers.',
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
      'let { perfect-power-nth } = import("Number-Theory");\nperfect-power-nth(3)',
      'let { perfect-power-nth } = import("Number-Theory");\nperfect-power-nth(15)',
    ],
  },
  'Number-Theory.perfect-power?': {
    title: 'Number-Theory.perfect-power?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect power numbers.',
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
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(7)',
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(8)',
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(9)',
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(10)',
    ],
  },
}
