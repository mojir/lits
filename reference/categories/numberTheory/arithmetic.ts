import type { NumberTheorySequenceReference } from '.'

export const arithmeticReference: NumberTheorySequenceReference<'arithmetic'> = {
  'TEMP-nth.arithmetic-seq': {
    title: 'TEMP-nth.arithmetic-seq',
    category: 'Number Theory',
    description: 'Generates the arithmetic sequence for a given $start, $step, and $length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      { argumentNames: ['start', 'step', 'length'] },
    ],
    examples: [
      'let { arithmetic-seq } = import("TEMP-nth");\narithmetic-seq(3, 2, 2)',
      'let { arithmetic-seq } = import("TEMP-nth");\narithmetic-seq(2, 3, 2)',
      'let { arithmetic-seq } = import("TEMP-nth");\narithmetic-seq(1, 2, 2)',
      'let { arithmetic-seq } = import("TEMP-nth");\narithmetic-seq(1, 1.5, 12)',
    ],
  },
  'TEMP-nth.arithmetic-take-while': {
    title: 'TEMP-nth.arithmetic-take-while',
    category: 'Number Theory',
    description: 'Generates the arithmetic sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      takeWhile: {
        type: 'function',
        description: 'A function that takes a number and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['start', 'step', 'takeWhile'] },
    ],
    examples: [
      'let { arithmetic-take-while } = import("TEMP-nth");\narithmetic-take-while(1, 0.25, -> $ < 3)',
    ],
  },
  'TEMP-nth.arithmetic-nth': {
    title: 'TEMP-nth.arithmetic-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the arithmetic sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      { argumentNames: ['start', 'step', 'n'] },
    ],
    examples: [
      'let { arithmetic-nth } = import("TEMP-nth");\narithmetic-nth(3, 2, 2)',
      'let { arithmetic-nth } = import("TEMP-nth");\narithmetic-nth(2, 3, 2)',
      'let { arithmetic-nth } = import("TEMP-nth");\narithmetic-nth(1, 2, 2)',
      'let { arithmetic-nth } = import("TEMP-nth");\narithmetic-nth(1, 1.5, 12)',
    ],
  },
  'TEMP-nth.arithmetic?': {
    title: 'TEMP-nth.arithmetic?',
    category: 'Number Theory',
    description: 'Checks if a number is part of the arithmetic sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['start', 'step', 'n'] },
    ],
    examples: [
      'let { arithmetic? } = import("TEMP-nth");\narithmetic?(3, 2, 2)',
      'let { arithmetic? } = import("TEMP-nth");\narithmetic?(2, 3, 2)',
      'let { arithmetic? } = import("TEMP-nth");\narithmetic?(1, 2, 2)',
      'let { arithmetic? } = import("TEMP-nth");\narithmetic?(1, 1.5, 12)',
    ],
  },
}
