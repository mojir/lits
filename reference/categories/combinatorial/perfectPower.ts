import type { CombinatorialSequenceReference } from '.'

export const perfectPowerReference: CombinatorialSequenceReference<'perfect-power'> = {
  'c:perfect-power-seq': {
    title: 'c:perfect-power-seq',
    category: 'Combinatorial',
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
      'c:perfect-power-seq(5)',
      'c:perfect-power-seq(20)',
    ],
  },
  'c:perfect-power-take-while': {
    title: 'c:perfect-power-take-while',
    category: 'Combinatorial',
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
      'c:perfect-power-take-while(-> $ <= 100)',
    ],
  },
  'c:perfect-power-nth': {
    title: 'c:perfect-power-nth',
    category: 'Combinatorial',
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
      'c:perfect-power-nth(3)',
      'c:perfect-power-nth(15)',
    ],
  },
  'c:perfect-power?': {
    title: 'c:perfect-power?',
    category: 'Combinatorial',
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
      'c:perfect-power?(7)',
      'c:perfect-power?(8)',
      'c:perfect-power?(9)',
      'c:perfect-power?(10)',
    ],
  },
}
