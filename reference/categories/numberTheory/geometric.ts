import type { NumberTheorySequenceReference } from '.'

export const geometricReference: NumberTheorySequenceReference<'geometric'> = {
  'n:geometric-seq': {
    title: 'n:geometric-seq',
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
      'n:geometric-seq(3, 2, 2)',
      'n:geometric-seq(2, 3, 2)',
      'n:geometric-seq(1, 2, 2)',
      'n:geometric-seq(1, 1.5, 12)',
    ],
  },
  'n:geometric-take-while': {
    title: 'n:geometric-take-while',
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
      'n:geometric-take-while(1, 1.5, -> $ < 10)',
    ],
  },
  'n:geometric-nth': {
    title: 'n:geometric-nth',
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
      'n:geometric-nth(3, 2, 2)',
      'n:geometric-nth(2, 3, 2)',
      'n:geometric-nth(1, 2, 2)',
      'n:geometric-nth(1, 1.5, 4)',
    ],
  },
  'n:geometric?': {
    title: 'n:geometric?',
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
      'n:geometric?(1, 2, 1)',
      'n:geometric?(2, 3, 2)',
      'n:geometric?(3, 2, 2)',
      'n:geometric?(1, 1.5, 2.25)',
      'n:geometric?(1, 1.5, -4)',
    ],
  },
}
