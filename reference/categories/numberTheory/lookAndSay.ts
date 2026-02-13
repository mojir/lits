import type { NumberTheorySequenceReference } from '.'

export const lookAndSayReference: NumberTheorySequenceReference<'look-and-say'> = {
  'Number-Theory.look-and-say-seq': {
    title: 'Number-Theory.look-and-say-seq',
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
      'let { look-and-say-seq } = import("Number-Theory");\nlook-and-say-seq(5)',
    ],
  },
  'Number-Theory.look-and-say-take-while': {
    title: 'Number-Theory.look-and-say-take-while',
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
      'let { look-and-say-take-while } = import("Number-Theory");\nlook-and-say-take-while((term, index) -> count(term) < 10)',
      'let { look-and-say-take-while } = import("Number-Theory");\nlook-and-say-take-while(-> $2 <= 10)',
    ],
  },
  'Number-Theory.look-and-say-nth': {
    title: 'Number-Theory.look-and-say-nth',
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
      'let { look-and-say-nth } = import("Number-Theory");\nlook-and-say-nth(5)',
    ],
  },
  'Number-Theory.look-and-say?': {
    title: 'Number-Theory.look-and-say?',
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
      'let { look-and-say? } = import("Number-Theory");\nlook-and-say?("111221")',
      'let { look-and-say? } = import("Number-Theory");\nlook-and-say?("123")',
    ],
  },
}
