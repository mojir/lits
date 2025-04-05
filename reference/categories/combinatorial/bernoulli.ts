import type { CombinatorialSequenceReference } from '.'

export const bernoulliReference: Omit<CombinatorialSequenceReference<'bernoulli'>, 'c:bernoulli?'> = {
  'c:bernoulli-seq': {
    title: 'c:bernoulli-seq',
    category: 'Combinatorial',
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
      'c:bernoulli-seq(5)',
      'c:bernoulli-seq(10)',
    ],
  },
  'c:bernoulli-take-while': {
    title: 'c:bernoulli-take-while',
    category: 'Combinatorial',
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
      'c:bernoulli-take-while(-> abs($) < 100)',
    ],
  },
  'c:bernoulli-nth': {
    title: 'c:bernoulli-nth',
    category: 'Combinatorial',
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
      'c:bernoulli-nth(5)',
      'c:bernoulli-nth(10)',
      'c:bernoulli-nth(23)',
    ],
  },
}
