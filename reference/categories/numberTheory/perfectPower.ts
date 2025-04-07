import type { NumberTheorySequenceReference } from '.'

export const perfectPowerReference: NumberTheorySequenceReference<'perfect-power'> = {
  'n:perfect-power-seq': {
    title: 'n:perfect-power-seq',
    category: 'Number Theory',
    description: 'Generates the perfect power numbers up to a specified length.',
    linkName: 'c-colon-perfect-power-seq',
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
      'n:perfect-power-seq(5)',
      'n:perfect-power-seq(20)',
    ],
  },
  'n:perfect-power-take-while': {
    title: 'n:perfect-power-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect power numbers while a condition is met.',
    linkName: 'c-colon-perfect-power-take-while',
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
      'n:perfect-power-take-while(-> $ <= 100)',
    ],
  },
  'n:perfect-power-nth': {
    title: 'n:perfect-power-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect power numbers.',
    linkName: 'c-colon-perfect-power-nth',
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
      'n:perfect-power-nth(3)',
      'n:perfect-power-nth(15)',
    ],
  },
  'n:perfect-power?': {
    title: 'n:perfect-power?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect power numbers.',
    linkName: 'c-colon-perfect-power?',
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
      'n:perfect-power?(7)',
      'n:perfect-power?(8)',
      'n:perfect-power?(9)',
      'n:perfect-power?(10)',
    ],
  },
}
