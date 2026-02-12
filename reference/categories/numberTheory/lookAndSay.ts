import type { NumberTheorySequenceReference } from '.'

export const lookAndSayReference: NumberTheorySequenceReference<'look-and-say'> = {
  'nth.look-and-say-seq': {
    title: 'nth.look-and-say-seq',
    category: 'Number Theory',
    description: 'Generates the Look-and-Say sequence up to a specified length.',
    returns: {
      type: 'string',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.look-and-say-seq(5)',
    ],
  },
  'nth.look-and-say-take-while': {
    title: 'nth.look-and-say-take-while',
    category: 'Number Theory',
    description: 'Generates the Look-and-Say sequence while a condition is met.',
    returns: {
      type: 'string',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes a string and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['takeWhile'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.look-and-say-take-while((term, index) -> count(term) < 10)',
      'let nt = import("nth");\nnt.look-and-say-take-while(-> $2 <= 10)',
    ],
  },
  'nth.look-and-say-nth': {
    title: 'nth.look-and-say-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Look-and-Say sequence.',
    returns: {
      type: 'string',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term in the sequence.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.look-and-say-nth(5)',
    ],
  },
  'nth.look-and-say?': {
    title: 'nth.look-and-say?',
    category: 'Number Theory',
    description: 'Checks if a string is a valid Look-and-Say term.',
    returns: {
      type: 'boolean',
    },
    args: {
      term: {
        type: 'string',
        description: 'The term to check.',
      },
    },
    variants: [
      { argumentNames: ['term'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.look-and-say?("111221")',
      'let nt = import("nth");\nnt.look-and-say?("123")',
    ],
  },
}
