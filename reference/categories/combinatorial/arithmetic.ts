import type { CombinatorialSequenceReference } from '.'

export const arithmeticReference: CombinatorialSequenceReference<'arithmetic'> = {
  'c:arithmetic-seq': {
    title: 'c:arithmetic-seq',
    category: 'Combinatorial',
    description: 'Generates the arithmetic sequence for a given $start, $step, and $length.',
    linkName: 'c-colon-arithmetic-seq',
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
      'c:arithmetic-seq(3, 2, 2)',
      'c:arithmetic-seq(2, 3, 2)',
      'c:arithmetic-seq(1, 2, 2)',
      'c:arithmetic-seq(1, 1.5, 12)',
    ],
  },
  'c:arithmetic-take-while': {
    title: 'c:arithmetic-take-while',
    category: 'Combinatorial',
    description: 'Generates the arithmetic sequence while a condition is met.',
    linkName: 'c-colon-arithmetic-take-while',
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
      'c:arithmetic-take-while(1, 0.25, -> $ < 3)',
    ],
  },
  'c:arithmetic-nth': {
    title: 'c:arithmetic-nth',
    category: 'Combinatorial',
    description: 'Generates the nth term of the arithmetic sequence.',
    linkName: 'c-colon-arithmetic-nth',
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
      'c:arithmetic-nth(3, 2, 2)',
      'c:arithmetic-nth(2, 3, 2)',
      'c:arithmetic-nth(1, 2, 2)',
      'c:arithmetic-nth(1, 1.5, 12)',
    ],
  },
  'c:arithmetic?': {
    title: 'c:arithmetic?',
    category: 'Combinatorial',
    description: 'Checks if a number is part of the arithmetic sequence.',
    linkName: 'c-colon-arithmetic?',
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
      'c:arithmetic?(3, 2, 2)',
      'c:arithmetic?(2, 3, 2)',
      'c:arithmetic?(1, 2, 2)',
      'c:arithmetic?(1, 1.5, 12)',
    ],
  },
}
