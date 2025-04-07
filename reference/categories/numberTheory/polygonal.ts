import type { NumberTheorySequenceReference } from '.'

export const polygonalReference: NumberTheorySequenceReference<'polygonal'> = {
  'n:polygonal-seq': {
    title: 'n:polygonal-seq',
    category: 'Number Theory',
    description: 'Generates the polygonal sequence for a given number of sides and length.',
    linkName: 'c-colon-polygonal-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      sides: {
        type: 'integer',
        description: 'The number of sides of the polygon.',
      },
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      { argumentNames: ['sides', 'length'] },
    ],
    examples: [
      'n:polygonal-seq(3, 2)',
      'n:polygonal-seq(4, 2)',
      'n:polygonal-seq(5, 3)',
      'n:polygonal-seq(6, 5)',
      'n:polygonal-seq(100, 10)',
    ],
  },
  'n:polygonal-take-while': {
    title: 'n:polygonal-take-while',
    category: 'Number Theory',
    description: 'Generates the polygonal sequence while a condition is met.',
    linkName: 'c-colon-polygonal-take-while',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      sides: {
        type: 'integer',
        description: 'The number of sides of the polygon.',
      },
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['sides', 'takeWhile'] },
    ],
    examples: [
      'n:polygonal-take-while(15, -> $ < 1000)',
    ],
  },
  'n:polygonal-nth': {
    title: 'n:polygonal-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the polygonal sequence.',
    linkName: 'c-colon-polygonal-nth',
    returns: {
      type: 'integer',
    },
    args: {
      sides: {
        type: 'integer',
        description: 'The number of sides of the polygon.',
      },
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      { argumentNames: ['sides', 'n'] },
    ],
    examples: [
      'n:polygonal-nth(3, 9)',
      'n:polygonal-nth(4, 5)',
      'n:polygonal-nth(5, 5)',
    ],
  },
  'n:polygonal?': {
    title: 'n:polygonal?',
    category: 'Number Theory',
    description: 'Checks if a number is in the polygonal sequence.',
    linkName: 'c-colon-polygonal',
    returns: {
      type: 'boolean',
    },
    args: {
      sides: {
        type: 'integer',
        description: 'The number of sides of the polygon.',
      },
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['sides', 'n'] },
    ],
    examples: [
      'n:polygonal?(3, 10)',
      'n:polygonal?(3, 9)',
      'n:polygonal?(4, 10000)',
      'n:polygonal?(4, 1000)',
      'n:polygonal?(6, 45)',
    ],
  },
}
