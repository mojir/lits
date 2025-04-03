import type { CombinatorialSequenceReference } from '.'

export const polygonalReference: CombinatorialSequenceReference<'polygonal'> = {
  'c:polygonal-seq': {
    title: 'c:polygonal-seq',
    category: 'Combinatorial',
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
      'c:polygonal-seq(3, 2)',
      'c:polygonal-seq(4, 2)',
      'c:polygonal-seq(5, 3)',
      'c:polygonal-seq(6, 5)',
      'c:polygonal-seq(100, 10)',
    ],
  },
  'c:polygonal-take-while': {
    title: 'c:polygonal-take-while',
    category: 'Combinatorial',
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
      'c:polygonal-take-while(15, -> $ < 1000)',
    ],
  },
  'c:polygonal-nth': {
    title: 'c:polygonal-nth',
    category: 'Combinatorial',
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
      'c:polygonal-nth(3, 9)',
      'c:polygonal-nth(4, 5)',
      'c:polygonal-nth(5, 5)',
    ],
  },
  'c:polygonal?': {
    title: 'c:polygonal?',
    category: 'Combinatorial',
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
      'c:polygonal?(3, 10)',
      'c:polygonal?(3, 9)',
      'c:polygonal?(4, 10000)',
      'c:polygonal?(4, 1000)',
      'c:polygonal?(6, 45)',
    ],
  },
}
