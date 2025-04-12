import type { NumberTheorySequenceReference } from '.'

export const perfectPowerReference: NumberTheorySequenceReference<'perfect-power'> = {
  'nth:perfect-power-seq': {
    title: 'nth:perfect-power-seq',
    category: 'Number Theory',
    description: 'Generates the perfect power numbers up to a specified length.',
    linkName: 'nth-colon-perfect-power-seq',
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
      'nth:perfect-power-seq(5)',
      'nth:perfect-power-seq(20)',
    ],
  },
  'nth:perfect-power-take-while': {
    title: 'nth:perfect-power-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect power numbers while a condition is met.',
    linkName: 'nth-colon-perfect-power-take-while',
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
      'nth:perfect-power-take-while(-> $ <= 100)',
    ],
  },
  'nth:perfect-power-nth': {
    title: 'nth:perfect-power-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect power numbers.',
    linkName: 'nth-colon-perfect-power-nth',
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
      'nth:perfect-power-nth(3)',
      'nth:perfect-power-nth(15)',
    ],
  },
  'nth:perfect-power?': {
    title: 'nth:perfect-power?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect power numbers.',
    linkName: 'nth-colon-perfect-power-question',
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
      'nth:perfect-power?(7)',
      'nth:perfect-power?(8)',
      'nth:perfect-power?(9)',
      'nth:perfect-power?(10)',
    ],
  },
}
