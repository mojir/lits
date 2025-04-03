import type { CombinatorialSequenceReference } from '.'

export const geometricReference: CombinatorialSequenceReference<'geometric'> = {
  'c:geometric-seq': {
    title: 'c:geometric-seq',
    category: 'Combinatorial',
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
      'c:geometric-seq(3, 2, 2)',
      'c:geometric-seq(2, 3, 2)',
      'c:geometric-seq(1, 2, 2)',
      'c:geometric-seq(1, 1.5, 12)',
    ],
  },
  'c:geometric-take-while': {
    title: 'c:geometric-take-while',
    category: 'Combinatorial',
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
      'c:geometric-take-while(1, 1.5, -> $ < 10)',
    ],
  },
  'c:geometric-nth': {
    title: 'c:geometric-nth',
    category: 'Combinatorial',
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
      'c:geometric-nth(3, 2, 2)',
      'c:geometric-nth(2, 3, 2)',
      'c:geometric-nth(1, 2, 2)',
      'c:geometric-nth(1, 1.5, 4)',
    ],
  },
  'c:geometric?': {
    title: 'c:geometric?',
    category: 'Combinatorial',
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
      'c:geometric?(1, 2, 1)',
      'c:geometric?(2, 3, 2)',
      'c:geometric?(3, 2, 2)',
      'c:geometric?(1, 1.5, 2.25)',
      'c:geometric?(1, 1.5, -4)',
    ],
  },
}
