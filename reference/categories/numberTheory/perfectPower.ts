import type { NumberTheorySequenceReference } from '.'

export const perfectPowerReference: NumberTheorySequenceReference<'perfect-power'> = {
  'TEMP-nth.perfect-power-seq': {
    title: 'TEMP-nth.perfect-power-seq',
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
      'let { perfect-power-seq } = import("TEMP-nth");\nperfect-power-seq(5)',
      'let { perfect-power-seq } = import("TEMP-nth");\nperfect-power-seq(20)',
    ],
  },
  'TEMP-nth.perfect-power-take-while': {
    title: 'TEMP-nth.perfect-power-take-while',
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
      'let { perfect-power-take-while } = import("TEMP-nth");\nperfect-power-take-while(-> $ <= 100)',
    ],
  },
  'TEMP-nth.perfect-power-nth': {
    title: 'TEMP-nth.perfect-power-nth',
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
      'let { perfect-power-nth } = import("TEMP-nth");\nperfect-power-nth(3)',
      'let { perfect-power-nth } = import("TEMP-nth");\nperfect-power-nth(15)',
    ],
  },
  'TEMP-nth.perfect-power?': {
    title: 'TEMP-nth.perfect-power?',
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
      'let { perfect-power? } = import("TEMP-nth");\nperfect-power?(7)',
      'let { perfect-power? } = import("TEMP-nth");\nperfect-power?(8)',
      'let { perfect-power? } = import("TEMP-nth");\nperfect-power?(9)',
      'let { perfect-power? } = import("TEMP-nth");\nperfect-power?(10)',
    ],
  },
}
