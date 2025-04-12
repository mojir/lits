import type { NumberTheorySequenceReference } from '.'

export const arithmeticReference: NumberTheorySequenceReference<'arithmetic'> = {
  'nth:arithmetic-seq': {
    title: 'nth:arithmetic-seq',
    category: 'Number Theory',
    description: 'Generates the arithmetic sequence for a given $start, $step, and $length.',
    linkName: 'nth-colon-arithmetic-seq',
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
      'nth:arithmetic-seq(3, 2, 2)',
      'nth:arithmetic-seq(2, 3, 2)',
      'nth:arithmetic-seq(1, 2, 2)',
      'nth:arithmetic-seq(1, 1.5, 12)',
    ],
  },
  'nth:arithmetic-take-while': {
    title: 'nth:arithmetic-take-while',
    category: 'Number Theory',
    description: 'Generates the arithmetic sequence while a condition is met.',
    linkName: 'nth-colon-arithmetic-take-while',
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
      'nth:arithmetic-take-while(1, 0.25, -> $ < 3)',
    ],
  },
  'nth:arithmetic-nth': {
    title: 'nth:arithmetic-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the arithmetic sequence.',
    linkName: 'nth-colon-arithmetic-nth',
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
      'nth:arithmetic-nth(3, 2, 2)',
      'nth:arithmetic-nth(2, 3, 2)',
      'nth:arithmetic-nth(1, 2, 2)',
      'nth:arithmetic-nth(1, 1.5, 12)',
    ],
  },
  'nth:arithmetic?': {
    title: 'nth:arithmetic?',
    category: 'Number Theory',
    description: 'Checks if a number is part of the arithmetic sequence.',
    linkName: 'nth-colon-arithmetic-question',
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
      'nth:arithmetic?(3, 2, 2)',
      'nth:arithmetic?(2, 3, 2)',
      'nth:arithmetic?(1, 2, 2)',
      'nth:arithmetic?(1, 1.5, 12)',
    ],
  },
}
