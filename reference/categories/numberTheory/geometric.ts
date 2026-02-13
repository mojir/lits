import type { NumberTheorySequenceReference } from '.'

export const geometricReference: NumberTheorySequenceReference<'geometric'> = {
  'Number-Theory.geometric-seq': {
    title: 'Number-Theory.geometric-seq',
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
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(3, 2, 2)',
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(2, 3, 2)',
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(1, 2, 2)',
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(1, 1.5, 12)',
    ],
  },
  'Number-Theory.geometric-take-while': {
    title: 'Number-Theory.geometric-take-while',
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
      'let { geometric-take-while } = import("Number-Theory");\ngeometric-take-while(1, 1.5, -> $ < 10)',
    ],
  },
  'Number-Theory.geometric-nth': {
    title: 'Number-Theory.geometric-nth',
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
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(3, 2, 2)',
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(2, 3, 2)',
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(1, 2, 2)',
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(1, 1.5, 4)',
    ],
  },
  'Number-Theory.geometric?': {
    title: 'Number-Theory.geometric?',
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
      'let { geometric? } = import("Number-Theory");\ngeometric?(1, 2, 1)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(2, 3, 2)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(3, 2, 2)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(1, 1.5, 2.25)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(1, 1.5, -4)',
    ],
  },
}
