import type { CombinatorialSequenceReference } from '.'

export const lookAndSayReference: CombinatorialSequenceReference<'look-and-say'> = {
  'c:look-and-say-seq': {
    title: 'c:look-and-say-seq',
    category: 'Combinatorial',
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
      'c:look-and-say-seq(5)',
    ],
  },
  'c:look-and-say-take-while': {
    title: 'c:look-and-say-take-while',
    category: 'Combinatorial',
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
      'c:look-and-say-take-while((term, index) -> term.length < 10)',
      'c:look-and-say-take-while(-> $2 <= 10)',
    ],
  },
  'c:look-and-say-nth': {
    title: 'c:look-and-say-nth',
    category: 'Combinatorial',
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
      'c:look-and-say-nth(5)',
    ],
  },
  'c:look-and-say?': {
    title: 'c:look-and-say?',
    category: 'Combinatorial',
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
      'c:look-and-say?("111221")',
      'c:look-and-say?("123")',
    ],
  },
}
