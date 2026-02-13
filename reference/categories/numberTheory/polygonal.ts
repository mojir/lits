import { getOperatorArgs } from '../../api'
import type { NumberTheorySequenceReference } from '.'

export const polygonalReference: NumberTheorySequenceReference<'polygonal'> = {
  'TEMP-nth.polygonal-seq': {
    title: 'TEMP-nth.polygonal-seq',
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
      'let { polygonal-seq } = import("TEMP-nth");\npolygonal-seq(3, 2)',
      'let { polygonal-seq } = import("TEMP-nth");\npolygonal-seq(4, 2)',
      'let { polygonal-seq } = import("TEMP-nth");\npolygonal-seq(5, 3)',
      'let { polygonal-seq } = import("TEMP-nth");\npolygonal-seq(6, 5)',
      'let { polygonal-seq } = import("TEMP-nth");\npolygonal-seq(100, 10)',
    ],
  },
  'TEMP-nth.polygonal-take-while': {
    title: 'TEMP-nth.polygonal-take-while',
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
      'let { polygonal-take-while } = import("TEMP-nth");\npolygonal-take-while(15, -> $ < 1000)',
    ],
  },
  'TEMP-nth.polygonal-nth': {
    title: 'TEMP-nth.polygonal-nth',
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
      'let { polygonal-nth } = import("TEMP-nth");\npolygonal-nth(3, 9)',
      'let { polygonal-nth } = import("TEMP-nth");\npolygonal-nth(4, 5)',
      'let { polygonal-nth } = import("TEMP-nth");\npolygonal-nth(5, 5)',
    ],
  },
  'TEMP-nth.polygonal?': {
    title: 'TEMP-nth.polygonal?',
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
      'let { polygonal? } = import("TEMP-nth");\npolygonal?(3, 10)',
      'let { polygonal? } = import("TEMP-nth");\npolygonal?(3, 9)',
      'let { polygonal? } = import("TEMP-nth");\npolygonal?(4, 10000)',
      'let { polygonal? } = import("TEMP-nth");\npolygonal?(4, 1000)',
      'let { polygonal? } = import("TEMP-nth");\npolygonal?(6, 45)',
    ],
  },
}
