import type { NumberTheorySequenceReference } from '.'

export const geometricReference: NumberTheorySequenceReference<'geometric'> = {
  'TEMP-nth.geometric-seq': {
    title: 'TEMP-nth.geometric-seq',
    category: 'Number Theory',
    description: 'Generates the geometric sequence for a given $start, $ratio, and $length.',
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
      'let nt = import("TEMP-nth");\nnt.geometric-seq(3, 2, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric-seq(2, 3, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric-seq(1, 2, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric-seq(1, 1.5, 12)',
    ],
  },
  'TEMP-nth.geometric-take-while': {
    title: 'TEMP-nth.geometric-take-while',
    category: 'Number Theory',
    description: 'Generates the geometric sequence while a condition is met.',
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
      'let nt = import("TEMP-nth");\nnt.geometric-take-while(1, 1.5, -> $ < 10)',
    ],
  },
  'TEMP-nth.geometric-nth': {
    title: 'TEMP-nth.geometric-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the geometric sequence.',
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
      'let nt = import("TEMP-nth");\nnt.geometric-nth(3, 2, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric-nth(2, 3, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric-nth(1, 2, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric-nth(1, 1.5, 4)',
    ],
  },
  'TEMP-nth.geometric?': {
    title: 'TEMP-nth.geometric?',
    category: 'Number Theory',
    description: 'Checks if a number is in the geometric sequence.',
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
      n: {
        type: 'number',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['start', 'ratio', 'n'] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.geometric?(1, 2, 1)',
      'let nt = import("TEMP-nth");\nnt.geometric?(2, 3, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric?(3, 2, 2)',
      'let nt = import("TEMP-nth");\nnt.geometric?(1, 1.5, 2.25)',
      'let nt = import("TEMP-nth");\nnt.geometric?(1, 1.5, -4)',
    ],
  },
}
