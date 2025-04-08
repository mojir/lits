import type { NumberTheorySequenceReference } from '.'

export const geometricReference: NumberTheorySequenceReference<'geometric'> = {
  'nth:geometric-seq': {
    title: 'nth:geometric-seq',
    category: 'Number Theory',
    description: 'Generates the geometric sequence for a given $start, $ratio, and $length.',
    linkName: 'c-colon-geometric-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      { argumentNames: ['start', 'ratio', 'length'] },
    ],
    examples: [
      'nth:geometric-seq(3, 2, 2)',
      'nth:geometric-seq(2, 3, 2)',
      'nth:geometric-seq(1, 2, 2)',
      'nth:geometric-seq(1, 1.5, 12)',
    ],
  },
  'nth:geometric-take-while': {
    title: 'nth:geometric-take-while',
    category: 'Number Theory',
    description: 'Generates the geometric sequence while a condition is met.',
    linkName: 'c-colon-geometric-take-while',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      takeWhile: {
        type: 'function',
        description: 'A function that takes a number and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['start', 'ratio', 'takeWhile'] },
    ],
    examples: [
      'nth:geometric-take-while(1, 1.5, -> $ < 10)',
    ],
  },
  'nth:geometric-nth': {
    title: 'nth:geometric-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the geometric sequence.',
    linkName: 'c-colon-geometric-nth',
    returns: {
      type: 'integer',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      { argumentNames: ['start', 'ratio', 'n'] },
    ],
    examples: [
      'nth:geometric-nth(3, 2, 2)',
      'nth:geometric-nth(2, 3, 2)',
      'nth:geometric-nth(1, 2, 2)',
      'nth:geometric-nth(1, 1.5, 4)',
    ],
  },
  'nth:geometric?': {
    title: 'nth:geometric?',
    category: 'Number Theory',
    description: 'Checks if a number is in the geometric sequence.',
    linkName: 'c-colon-geometric',
    returns: {
      type: 'boolean',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      number: {
        type: 'number',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['start', 'ratio', 'number'] },
    ],
    examples: [
      'nth:geometric?(1, 2, 1)',
      'nth:geometric?(2, 3, 2)',
      'nth:geometric?(3, 2, 2)',
      'nth:geometric?(1, 1.5, 2.25)',
      'nth:geometric?(1, 1.5, -4)',
    ],
  },
}
