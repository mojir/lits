import type { NumberTheorySequenceReference } from '.'

export const bernoulliReference: Omit<NumberTheorySequenceReference<'bernoulli'>, 'Number-Theory.bernoulli?'> = {
  'Number-Theory.bernoulli-seq': {
    title: 'Number-Theory.bernoulli-seq',
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
      'let { bernoulli-seq } = import("Number-Theory");\nbernoulli-seq(5)',
      'let { bernoulli-seq } = import("Number-Theory");\nbernoulli-seq(10)',
    ],
  },
  'Number-Theory.bernoulli-take-while': {
    title: 'Number-Theory.bernoulli-take-while',
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
      'let { bernoulli-take-while } = import("Number-Theory");\nbernoulli-take-while(-> abs($) < 100)',
    ],
  },
  'Number-Theory.bernoulli-nth': {
    title: 'Number-Theory.bernoulli-nth',
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
      'let { bernoulli-nth } = import("Number-Theory");\nbernoulli-nth(5)',
      'let { bernoulli-nth } = import("Number-Theory");\nbernoulli-nth(10)',
      'let { bernoulli-nth } = import("Number-Theory");\nbernoulli-nth(23)',
    ],
  },
}
