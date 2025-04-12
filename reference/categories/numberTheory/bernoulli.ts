import type { NumberTheorySequenceReference } from '.'

export const bernoulliReference: Omit<NumberTheorySequenceReference<'bernoulli'>, 'nth:bernoulli?'> = {
  'nth:bernoulli-seq': {
    title: 'nth:bernoulli-seq',
    category: 'Number Theory',
    description: 'Generates the Bernoulli sequence up to a specified length.',
    linkName: 'nth-colon-bernoulli-seq',
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
      'nth:bernoulli-seq(5)',
      'nth:bernoulli-seq(10)',
    ],
  },
  'nth:bernoulli-take-while': {
    title: 'nth:bernoulli-take-while',
    category: 'Number Theory',
    description: 'Generates the Bernoulli sequence while a condition is met.',
    linkName: 'nth-colon-bernoulli-take-while',
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
      'nth:bernoulli-take-while(-> abs($) < 100)',
    ],
  },
  'nth:bernoulli-nth': {
    title: 'nth:bernoulli-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the Bernoulli sequence.',
    linkName: 'nth-colon-bernoulli-nth',
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
      'nth:bernoulli-nth(5)',
      'nth:bernoulli-nth(10)',
      'nth:bernoulli-nth(23)',
    ],
  },
}
