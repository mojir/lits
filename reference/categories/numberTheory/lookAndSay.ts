import type { NumberTheorySequenceReference } from '.'

export const lookAndSayReference: NumberTheorySequenceReference<'look-and-say'> = {
  'n:look-and-say-seq': {
    title: 'n:look-and-say-seq',
    category: 'Number Theory',
    description: 'Generates the Look-and-Say sequence up to a specified length.',
    linkName: 'c-colon-look-and-say-seq',
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
      'n:look-and-say-seq(5)',
    ],
  },
  'n:look-and-say-take-while': {
    title: 'n:look-and-say-take-while',
    category: 'Number Theory',
    description: 'Generates the Look-and-Say sequence while a condition is met.',
    linkName: 'c-colon-look-and-say-take-while',
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
      'n:look-and-say-take-while((term, index) -> term.length < 10)',
      'n:look-and-say-take-while(-> $2 <= 10)',
    ],
  },
  'n:look-and-say-nth': {
    title: 'n:look-and-say-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Look-and-Say sequence.',
    linkName: 'c-colon-look-and-say-nth',
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
      'n:look-and-say-nth(5)',
    ],
  },
  'n:look-and-say?': {
    title: 'n:look-and-say?',
    category: 'Number Theory',
    description: 'Checks if a string is a valid Look-and-Say term.',
    linkName: 'c-colon-look-and-say-question',
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
      'n:look-and-say?("111221")',
      'n:look-and-say?("123")',
    ],
  },
}
