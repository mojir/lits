import { getOperatorArgs } from '../../api'
import type { NumberTheorySequenceReference } from '.'

export const polygonalReference: NumberTheorySequenceReference<'polygonal'> = {
  'Number-Theory.polygonal-seq': {
    title: 'Number-Theory.polygonal-seq',
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
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(3, 2)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(4, 2)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(5, 3)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(6, 5)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(100, 10)',
    ],
  },
  'Number-Theory.polygonal-take-while': {
    title: 'Number-Theory.polygonal-take-while',
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
      'let { polygonal-take-while } = import("Number-Theory");\npolygonal-take-while(15, -> $ < 1000)',
    ],
  },
  'Number-Theory.polygonal-nth': {
    title: 'Number-Theory.polygonal-nth',
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
      'let { polygonal-nth } = import("Number-Theory");\npolygonal-nth(3, 9)',
      'let { polygonal-nth } = import("Number-Theory");\npolygonal-nth(4, 5)',
      'let { polygonal-nth } = import("Number-Theory");\npolygonal-nth(5, 5)',
    ],
  },
  'Number-Theory.polygonal?': {
    title: 'Number-Theory.polygonal?',
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
      'let { polygonal? } = import("Number-Theory");\npolygonal?(3, 10)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(3, 9)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(4, 10000)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(4, 1000)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(6, 45)',
    ],
  },
}
