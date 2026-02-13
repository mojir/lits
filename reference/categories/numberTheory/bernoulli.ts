import type { NumberTheorySequenceReference } from '.'

export const bernoulliReference: Omit<NumberTheorySequenceReference<'bernoulli'>, 'TEMP-nth.bernoulli?'> = {
  'TEMP-nth.bernoulli-seq': {
    title: 'TEMP-nth.bernoulli-seq',
    category: 'Number Theory',
    description: 'Generates the Bernoulli sequence up to a specified length.',
    returns: {
      type: 'number',
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
      'let nt = import("TEMP-nth");\nnt.bernoulli-seq(5)',
      'let nt = import("TEMP-nth");\nnt.bernoulli-seq(10)',
    ],
  },
  'TEMP-nth.bernoulli-take-while': {
    title: 'TEMP-nth.bernoulli-take-while',
    category: 'Number Theory',
    description: 'Generates the Bernoulli sequence while a condition is met.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['takeWhile'] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.bernoulli-take-while(-> abs($) < 100)',
    ],
  },
  'TEMP-nth.bernoulli-nth': {
    title: 'TEMP-nth.bernoulli-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Bernoulli sequence.',
    returns: {
      type: 'number',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'let nt = import("TEMP-nth");\nnt.bernoulli-nth(5)',
      'let nt = import("TEMP-nth");\nnt.bernoulli-nth(10)',
      'let nt = import("TEMP-nth");\nnt.bernoulli-nth(23)',
    ],
  },
}
