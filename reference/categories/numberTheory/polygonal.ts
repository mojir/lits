import { getOperatorArgs } from '../../api'
import type { NumberTheorySequenceReference } from '.'

export const polygonalReference: NumberTheorySequenceReference<'polygonal'> = {
  'nth.polygonal-seq': {
    title: 'nth.polygonal-seq',
    category: 'Number Theory',
    description: 'Generates the polygonal sequence for a given number of sides and length.',
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
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['sides', 'length'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.polygonal-seq(3, 2)',
      'let nt = import("nth");\nnt.polygonal-seq(4, 2)',
      'let nt = import("nth");\nnt.polygonal-seq(5, 3)',
      'let nt = import("nth");\nnt.polygonal-seq(6, 5)',
      'let nt = import("nth");\nnt.polygonal-seq(100, 10)',
    ],
  },
  'nth.polygonal-take-while': {
    title: 'nth.polygonal-take-while',
    category: 'Number Theory',
    description: 'Generates the polygonal sequence while a condition is met.',
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
      ...getOperatorArgs('integer', 'function'),
    },
    variants: [
      { argumentNames: ['sides', 'takeWhile'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.polygonal-take-while(15, -> $ < 1000)',
    ],
  },
  'nth.polygonal-nth': {
    title: 'nth.polygonal-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the polygonal sequence.',
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
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['sides', 'n'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.polygonal-nth(3, 9)',
      'let nt = import("nth");\nnt.polygonal-nth(4, 5)',
      'let nt = import("nth");\nnt.polygonal-nth(5, 5)',
    ],
  },
  'nth.polygonal?': {
    title: 'nth.polygonal?',
    category: 'Number Theory',
    description: 'Checks if a number is in the polygonal sequence.',
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
      ...getOperatorArgs('integer', 'integer'),
    },
    variants: [
      { argumentNames: ['sides', 'n'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.polygonal?(3, 10)',
      'let nt = import("nth");\nnt.polygonal?(3, 9)',
      'let nt = import("nth");\nnt.polygonal?(4, 10000)',
      'let nt = import("nth");\nnt.polygonal?(4, 1000)',
      'let nt = import("nth");\nnt.polygonal?(6, 45)',
    ],
  },
}
