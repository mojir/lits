import type { NumberTheorySequenceReference } from '.'

export const bernoulliReference: Omit<NumberTheorySequenceReference<'bernoulli'>, 'n:bernoulli?'> = {
  'n:bernoulli-seq': {
    title: 'n:bernoulli-seq',
    category: 'Number Theory',
    description: 'Generates the Bernoulli sequence up to a specified length.',
    linkName: 'c-colon-bernoulli-seq',
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
      'n:bernoulli-seq(5)',
      'n:bernoulli-seq(10)',
    ],
  },
  'n:bernoulli-take-while': {
    title: 'n:bernoulli-take-while',
    category: 'Number Theory',
    description: 'Generates the Bernoulli sequence while a condition is met.',
    linkName: 'c-colon-bernoulli-take-while',
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
      'n:bernoulli-take-while(-> abs($) < 100)',
    ],
  },
  'n:bernoulli-nth': {
    title: 'n:bernoulli-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Bernoulli sequence.',
    linkName: 'c-colon-bernoulli-nth',
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
      'n:bernoulli-nth(5)',
      'n:bernoulli-nth(10)',
      'n:bernoulli-nth(23)',
    ],
  },
}
