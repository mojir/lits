import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'abundant-seq': {
    category: 'number-theory',
    description: 'Generates the abundant numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { abundant-seq } = import(number-theory);\nabundant-seq(1)',
      'let { abundant-seq } = import(number-theory);\nabundant-seq(5)',
    ],
    seeAlso: ['number-theory.abundant-nth', 'number-theory.abundant-take-while', 'number-theory.abundant?', 'number-theory.deficient-seq', 'number-theory.perfect-seq'],
  },
  'abundant-take-while': {
    category: 'number-theory',
    description: 'Generates the abundant numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { abundant-take-while } = import(number-theory);\nabundant-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.abundant-seq', 'number-theory.abundant-nth', 'number-theory.abundant?'],
  },
  'abundant-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the abundant numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the number in the sequence.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { abundant-nth } = import(number-theory);\nabundant-nth(1)',
      'let { abundant-nth } = import(number-theory);\nabundant-nth(5)',
    ],
    seeAlso: ['number-theory.abundant-seq', 'number-theory.abundant-take-while', 'number-theory.abundant?'],
  },
  'abundant?': {
    category: 'number-theory',
    description: 'Checks if a number is abundant.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { abundant? } = import(number-theory);\nabundant?(12)',
      'let { abundant? } = import(number-theory);\nabundant?(15)',
    ],
    seeAlso: ['number-theory.abundant-seq', 'number-theory.abundant-nth', 'number-theory.deficient?', 'number-theory.perfect?', 'number-theory.sigma', 'number-theory.divisors', 'number-theory.abundant-take-while'],
  },
  'arithmetic-seq': {
    category: 'number-theory',
    description: 'Generates the arithmetic sequence for a given $start, $step, and $length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'step',
          'length',
        ],
      },
    ],
    examples: [
      'let { arithmetic-seq } = import(number-theory);\narithmetic-seq(3, 2, 2)',
      'let { arithmetic-seq } = import(number-theory);\narithmetic-seq(2, 3, 2)',
      'let { arithmetic-seq } = import(number-theory);\narithmetic-seq(1, 2, 2)',
      'let { arithmetic-seq } = import(number-theory);\narithmetic-seq(1, 1.5, 12)',
    ],
    seeAlso: ['number-theory.arithmetic-nth', 'number-theory.arithmetic-take-while', 'number-theory.arithmetic?', 'number-theory.geometric-seq'],
  },
  'arithmetic-take-while': {
    category: 'number-theory',
    description: 'Generates the arithmetic sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      takeWhile: {
        type: 'function',
        description: 'A function that takes a number and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'step',
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { arithmetic-take-while } = import(number-theory);\narithmetic-take-while(1, 0.25, -> $ < 3)',
    ],
    seeAlso: ['number-theory.arithmetic-seq', 'number-theory.arithmetic-nth', 'number-theory.arithmetic?'],
  },
  'arithmetic-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the arithmetic sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'step',
          'n',
        ],
      },
    ],
    examples: [
      'let { arithmetic-nth } = import(number-theory);\narithmetic-nth(3, 2, 2)',
      'let { arithmetic-nth } = import(number-theory);\narithmetic-nth(2, 3, 2)',
      'let { arithmetic-nth } = import(number-theory);\narithmetic-nth(1, 2, 2)',
      'let { arithmetic-nth } = import(number-theory);\narithmetic-nth(1, 1.5, 12)',
    ],
    seeAlso: ['number-theory.arithmetic-seq', 'number-theory.arithmetic-take-while', 'number-theory.arithmetic?'],
  },
  'arithmetic?': {
    category: 'number-theory',
    description: 'Checks if a number is part of the arithmetic sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      step: {
        type: 'number',
        description: 'The common difference of the sequence.',
      },
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'step',
          'n',
        ],
      },
    ],
    examples: [
      'let { arithmetic? } = import(number-theory);\narithmetic?(3, 2, 2)',
      'let { arithmetic? } = import(number-theory);\narithmetic?(2, 3, 2)',
      'let { arithmetic? } = import(number-theory);\narithmetic?(1, 2, 2)',
      'let { arithmetic? } = import(number-theory);\narithmetic?(1, 1.5, 12)',
    ],
    seeAlso: ['number-theory.arithmetic-seq', 'number-theory.arithmetic-nth', 'number-theory.geometric?', 'number-theory.arithmetic-take-while'],
  },
  'bell-seq': {
    category: 'number-theory',
    description: 'Generates the Bell sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 22 (the maximum length of the pre-calculated bell numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { bell-seq } = import(number-theory);\nbell-seq(5)',
      'let { bell-seq } = import(number-theory);\nbell-seq(10)',
      'let { bell-seq } = import(number-theory);\nbell-seq()',
    ],
    seeAlso: ['number-theory.bell-nth', 'number-theory.bell-take-while', 'number-theory.bell?', 'number-theory.catalan-seq', 'number-theory.stirling-second', 'number-theory.stirling-first'],
  },
  'bell-take-while': {
    category: 'number-theory',
    description: 'Generates the Bell sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { bell-take-while } = import(number-theory);\nbell-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.bell-seq', 'number-theory.bell-nth', 'number-theory.bell?'],
  },
  'bell-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Bell sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { bell-nth } = import(number-theory);\nbell-nth(5)',
      'let { bell-nth } = import(number-theory);\nbell-nth(10)',
    ],
    seeAlso: ['number-theory.bell-seq', 'number-theory.bell-take-while', 'number-theory.bell?'],
  },
  'bell?': {
    category: 'number-theory',
    description: 'Checks if a number is in the Bell sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { bell? } = import(number-theory);\nbell?(1)',
      'let { bell? } = import(number-theory);\nbell?(27644437)',
      'let { bell? } = import(number-theory);\nbell?(27644436)',
    ],
    seeAlso: ['number-theory.bell-seq', 'number-theory.bell-nth', 'number-theory.catalan?', 'number-theory.bell-take-while'],
  },
  'bernoulli-seq': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { bernoulli-seq } = import(number-theory);\nbernoulli-seq(5)',
      'let { bernoulli-seq } = import(number-theory);\nbernoulli-seq(10)',
    ],
    seeAlso: ['number-theory.bernoulli-nth', 'number-theory.bernoulli-take-while'],
  },
  'bernoulli-take-while': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { bernoulli-take-while } = import(number-theory);\nbernoulli-take-while(-> abs($) < 100)',
    ],
    seeAlso: ['number-theory.bernoulli-seq', 'number-theory.bernoulli-nth'],
  },
  'bernoulli-nth': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { bernoulli-nth } = import(number-theory);\nbernoulli-nth(5)',
      'let { bernoulli-nth } = import(number-theory);\nbernoulli-nth(10)',
      'let { bernoulli-nth } = import(number-theory);\nbernoulli-nth(23)',
    ],
    seeAlso: ['number-theory.bernoulli-seq', 'number-theory.bernoulli-take-while'],
  },
  'catalan-seq': {
    category: 'number-theory',
    description: 'Generates the Catalan sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 30 (the maximum length of the pre-calculated catalan numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { catalan-seq } = import(number-theory);\ncatalan-seq(5)',
      'let { catalan-seq } = import(number-theory);\ncatalan-seq(10)',
      'let { catalan-seq } = import(number-theory);\ncatalan-seq()',
    ],
    seeAlso: ['number-theory.catalan-nth', 'number-theory.catalan-take-while', 'number-theory.catalan?', 'number-theory.bell-seq'],
  },
  'catalan-take-while': {
    category: 'number-theory',
    description: 'Generates the Catalan sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { catalan-take-while } = import(number-theory);\ncatalan-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.catalan-seq', 'number-theory.catalan-nth', 'number-theory.catalan?'],
  },
  'catalan-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Catalan sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { catalan-nth } = import(number-theory);\ncatalan-nth(5)',
      'let { catalan-nth } = import(number-theory);\ncatalan-nth(10)',
    ],
    seeAlso: ['number-theory.catalan-seq', 'number-theory.catalan-take-while', 'number-theory.catalan?'],
  },
  'catalan?': {
    category: 'number-theory',
    description: 'Determines if a number is in the Catalan sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { catalan? } = import(number-theory);\ncatalan?(5)',
      'let { catalan? } = import(number-theory);\ncatalan?(10)',
    ],
    seeAlso: ['number-theory.catalan-seq', 'number-theory.catalan-nth', 'number-theory.bell?', 'number-theory.catalan-take-while'],
  },
  'collatz-seq': {
    category: 'number-theory',
    description: 'Generates the collatz sequence starting from a given integer.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'integer',
        description: 'The starting integer for the collatz sequence.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
        ],
      },
    ],
    examples: [
      'let { collatz-seq } = import(number-theory);\ncollatz-seq(3)',
      'let { collatz-seq } = import(number-theory);\ncollatz-seq(11)',
    ],
    seeAlso: ['number-theory.juggler-seq'],
  },
  'composite-seq': {
    category: 'number-theory',
    description: 'Generates the composite sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { composite-seq } = import(number-theory);\ncomposite-seq(1)',
      'let { composite-seq } = import(number-theory);\ncomposite-seq(2)',
      'let { composite-seq } = import(number-theory);\ncomposite-seq(10)',
    ],
    seeAlso: ['number-theory.composite-nth', 'number-theory.composite-take-while', 'number-theory.composite?', 'number-theory.prime-seq'],
  },
  'composite-take-while': {
    category: 'number-theory',
    description: 'Generates the composite sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { composite-take-while } = import(number-theory);\ncomposite-take-while(-> $ < 50)',
    ],
    seeAlso: ['number-theory.composite-seq', 'number-theory.composite-nth', 'number-theory.composite?'],
  },
  'composite-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the composite sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the composite number to retrieve.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { composite-nth } = import(number-theory);\ncomposite-nth(1)',
      'let { composite-nth } = import(number-theory);\ncomposite-nth(2)',
      'let { composite-nth } = import(number-theory);\ncomposite-nth(10)',
    ],
    seeAlso: ['number-theory.composite-seq', 'number-theory.composite-take-while', 'number-theory.composite?'],
  },
  'composite?': {
    category: 'number-theory',
    description: 'Determines if a number is composite.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { composite? } = import(number-theory);\ncomposite?(4)',
      'let { composite? } = import(number-theory);\ncomposite?(5)',
      'let { composite? } = import(number-theory);\ncomposite?(11)',
    ],
    seeAlso: ['number-theory.composite-seq', 'number-theory.composite-nth', 'number-theory.prime?', 'number-theory.prime-factors', 'number-theory.composite-take-while'],
  },
  'deficient-seq': {
    category: 'number-theory',
    description: 'Generates the deficient numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { deficient-seq } = import(number-theory);\ndeficient-seq(1)',
      'let { deficient-seq } = import(number-theory);\ndeficient-seq(5)',
    ],
    seeAlso: ['number-theory.deficient-nth', 'number-theory.deficient-take-while', 'number-theory.deficient?', 'number-theory.abundant-seq', 'number-theory.perfect-seq'],
  },
  'deficient-take-while': {
    category: 'number-theory',
    description: 'Generates the deficient numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { deficient-take-while } = import(number-theory);\ndeficient-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.deficient-seq', 'number-theory.deficient-nth', 'number-theory.deficient?'],
  },
  'deficient-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the deficient numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the number in the sequence.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { deficient-nth } = import(number-theory);\ndeficient-nth(5)',
      'let { deficient-nth } = import(number-theory);\ndeficient-nth(12)',
    ],
    seeAlso: ['number-theory.deficient-seq', 'number-theory.deficient-take-while', 'number-theory.deficient?'],
  },
  'deficient?': {
    category: 'number-theory',
    description: 'Checks if a number is deficient.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { deficient? } = import(number-theory);\ndeficient?(12)',
      'let { deficient? } = import(number-theory);\ndeficient?(15)',
    ],
    seeAlso: ['number-theory.deficient-seq', 'number-theory.deficient-nth', 'number-theory.abundant?', 'number-theory.perfect?', 'number-theory.sigma', 'number-theory.divisors', 'number-theory.deficient-take-while'],
  },
  'factorial-seq': {
    category: 'number-theory',
    description: 'Generates the factorial sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 19 (the maximum length of the pre-calculated factorial numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { factorial-seq } = import(number-theory);\nfactorial-seq(1)',
      'let { factorial-seq } = import(number-theory);\nfactorial-seq(2)',
      'let { factorial-seq } = import(number-theory);\nfactorial-seq(3)',
      'let { factorial-seq } = import(number-theory);\nfactorial-seq(4)',
      'let { factorial-seq } = import(number-theory);\nfactorial-seq(5)',
      'let { factorial-seq } = import(number-theory);\nfactorial-seq(10)',
    ],
    seeAlso: ['number-theory.factorial-nth', 'number-theory.factorial-take-while', 'number-theory.factorial?', 'number-theory.factorial'],
  },
  'factorial-take-while': {
    category: 'number-theory',
    description: 'Generates the factorial sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { factorial-take-while } = import(number-theory);\nfactorial-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.factorial-seq', 'number-theory.factorial-nth', 'number-theory.factorial?'],
  },
  'factorial-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the factorial sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { factorial-nth } = import(number-theory);\nfactorial-nth(1)',
      'let { factorial-nth } = import(number-theory);\nfactorial-nth(2)',
      'let { factorial-nth } = import(number-theory);\nfactorial-nth(3)',
      'let { factorial-nth } = import(number-theory);\nfactorial-nth(4)',
      'let { factorial-nth } = import(number-theory);\nfactorial-nth(5)',
      'let { factorial-nth } = import(number-theory);\nfactorial-nth(10)',
    ],
    seeAlso: ['number-theory.factorial-seq', 'number-theory.factorial-take-while', 'number-theory.factorial?', 'number-theory.factorial'],
  },
  'factorial?': {
    category: 'number-theory',
    description: 'Checks if a number is in the factorial sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { factorial? } = import(number-theory);\nfactorial?(1)',
      'let { factorial? } = import(number-theory);\nfactorial?(2)',
      'let { factorial? } = import(number-theory);\nfactorial?(3)',
      'let { factorial? } = import(number-theory);\nfactorial?(4)',
      'let { factorial? } = import(number-theory);\nfactorial?(5)',
      'let { factorial? } = import(number-theory);\nfactorial?(6)',
      'let { factorial? } = import(number-theory);\nfactorial?(7)',
      'let { factorial? } = import(number-theory);\nfactorial?(8)',
      'let { factorial? } = import(number-theory);\nfactorial?(9)',
      'let { factorial? } = import(number-theory);\nfactorial?(3628800)',
    ],
    seeAlso: ['number-theory.factorial-seq', 'number-theory.factorial-nth', 'number-theory.factorial', 'number-theory.factorial-take-while'],
  },
  'fibonacci-seq': {
    category: 'number-theory',
    description: 'Generates the fibonacci sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 79 (the maximum length of the pre-calculated Fibonacci numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { fibonacci-seq } = import(number-theory);\nfibonacci-seq(1)',
      'let { fibonacci-seq } = import(number-theory);\nfibonacci-seq(2)',
      'let { fibonacci-seq } = import(number-theory);\nfibonacci-seq()',
    ],
    seeAlso: ['number-theory.fibonacci-nth', 'number-theory.fibonacci-take-while', 'number-theory.fibonacci?', 'number-theory.lucas-seq', 'number-theory.tribonacci-seq', 'number-theory.pell-seq', 'number-theory.padovan-seq'],
  },
  'fibonacci-take-while': {
    category: 'number-theory',
    description: 'Generates the fibonacci sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { fibonacci-take-while } = import(number-theory);\nfibonacci-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.fibonacci-seq', 'number-theory.fibonacci-nth', 'number-theory.fibonacci?'],
  },
  'fibonacci-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the fibonacci sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { fibonacci-nth } = import(number-theory);\nfibonacci-nth(5)',
      'let { fibonacci-nth } = import(number-theory);\nfibonacci-nth(50)',
    ],
    seeAlso: ['number-theory.fibonacci-seq', 'number-theory.fibonacci-take-while', 'number-theory.fibonacci?'],
  },
  'fibonacci?': {
    category: 'number-theory',
    description: 'Determines if a number is in the fibonacci sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { fibonacci? } = import(number-theory);\nfibonacci?(0)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(1)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(2)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(3)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(4)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(5)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(6)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(7)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(8)',
      'let { fibonacci? } = import(number-theory);\nfibonacci?(9)',
    ],
    seeAlso: ['number-theory.fibonacci-seq', 'number-theory.fibonacci-nth', 'number-theory.lucas?', 'number-theory.fibonacci-take-while', 'number-theory.tribonacci?', 'number-theory.padovan?', 'number-theory.pell?'],
  },
  'geometric-seq': {
    category: 'number-theory',
    description: 'Generates the geometric sequence for a given $start, $ratio, and $length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'ratio',
          'length',
        ],
      },
    ],
    examples: [
      'let { geometric-seq } = import(number-theory);\ngeometric-seq(3, 2, 2)',
      'let { geometric-seq } = import(number-theory);\ngeometric-seq(2, 3, 2)',
      'let { geometric-seq } = import(number-theory);\ngeometric-seq(1, 2, 2)',
      'let { geometric-seq } = import(number-theory);\ngeometric-seq(1, 1.5, 12)',
    ],
    seeAlso: ['number-theory.geometric-nth', 'number-theory.geometric-take-while', 'number-theory.geometric?', 'number-theory.arithmetic-seq'],
  },
  'geometric-take-while': {
    category: 'number-theory',
    description: 'Generates the geometric sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      takeWhile: {
        type: 'function',
        description: 'A function that takes a number and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'ratio',
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { geometric-take-while } = import(number-theory);\ngeometric-take-while(1, 1.5, -> $ < 10)',
    ],
    seeAlso: ['number-theory.geometric-seq', 'number-theory.geometric-nth', 'number-theory.geometric?'],
  },
  'geometric-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the geometric sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'ratio',
          'n',
        ],
      },
    ],
    examples: [
      'let { geometric-nth } = import(number-theory);\ngeometric-nth(3, 2, 2)',
      'let { geometric-nth } = import(number-theory);\ngeometric-nth(2, 3, 2)',
      'let { geometric-nth } = import(number-theory);\ngeometric-nth(1, 2, 2)',
      'let { geometric-nth } = import(number-theory);\ngeometric-nth(1, 1.5, 4)',
    ],
    seeAlso: ['number-theory.geometric-seq', 'number-theory.geometric-take-while', 'number-theory.geometric?'],
  },
  'geometric?': {
    category: 'number-theory',
    description: 'Checks if a number is in the geometric sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting term of the sequence.',
      },
      ratio: {
        type: 'number',
        description: 'The common ratio of the sequence.',
      },
      n: {
        type: 'number',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'ratio',
          'n',
        ],
      },
    ],
    examples: [
      'let { geometric? } = import(number-theory);\ngeometric?(1, 2, 1)',
      'let { geometric? } = import(number-theory);\ngeometric?(2, 3, 2)',
      'let { geometric? } = import(number-theory);\ngeometric?(3, 2, 2)',
      'let { geometric? } = import(number-theory);\ngeometric?(1, 1.5, 2.25)',
      'let { geometric? } = import(number-theory);\ngeometric?(1, 1.5, -4)',
    ],
    seeAlso: ['number-theory.geometric-seq', 'number-theory.geometric-nth', 'number-theory.arithmetic?', 'number-theory.geometric-take-while'],
  },
  'golomb-seq': {
    category: 'number-theory',
    description: 'Generates the Golomb sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { golomb-seq } = import(number-theory);\ngolomb-seq(5)',
      'let { golomb-seq } = import(number-theory);\ngolomb-seq(20)',
    ],
    seeAlso: ['number-theory.golomb-nth', 'number-theory.golomb-take-while', 'number-theory.golomb?', 'number-theory.recaman-seq'],
  },
  'golomb-take-while': {
    category: 'number-theory',
    description: 'Generates the Golomb sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { golomb-take-while } = import(number-theory);\ngolomb-take-while(-> $ <= 10)',
    ],
    seeAlso: ['number-theory.golomb-seq', 'number-theory.golomb-nth', 'number-theory.golomb?'],
  },
  'golomb-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Golomb sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { golomb-nth } = import(number-theory);\ngolomb-nth(5)',
      'let { golomb-nth } = import(number-theory);\ngolomb-nth(1000)',
    ],
    seeAlso: ['number-theory.golomb-seq', 'number-theory.golomb-take-while', 'number-theory.golomb?'],
  },
  'golomb?': {
    category: 'number-theory',
    description: 'Checks if a number is in the Golomb sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { golomb? } = import(number-theory);\ngolomb?(1)',
      'let { golomb? } = import(number-theory);\ngolomb?(2)',
      'let { golomb? } = import(number-theory);\ngolomb?(3345)',
      'let { golomb? } = import(number-theory);\ngolomb?(67867864)',
    ],
    seeAlso: ['number-theory.golomb-seq', 'number-theory.golomb-nth', 'number-theory.golomb-take-while'],
  },
  'happy-seq': {
    category: 'number-theory',
    description: 'Generates the happy sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 20 (the maximum length of the pre-calculated happy numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { happy-seq } = import(number-theory);\nhappy-seq(1)',
      'let { happy-seq } = import(number-theory);\nhappy-seq(2)',
      'let { happy-seq } = import(number-theory);\nhappy-seq(20)',
    ],
    seeAlso: ['number-theory.happy-nth', 'number-theory.happy-take-while', 'number-theory.happy?', 'number-theory.lucky-seq'],
  },
  'happy-take-while': {
    category: 'number-theory',
    description: 'Generates the happy sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { happy-take-while } = import(number-theory);\nhappy-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.happy-seq', 'number-theory.happy-nth', 'number-theory.happy?'],
  },
  'happy-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the happy sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the happy number to return.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { happy-nth } = import(number-theory);\nhappy-nth(1)',
      'let { happy-nth } = import(number-theory);\nhappy-nth(2)',
      'let { happy-nth } = import(number-theory);\nhappy-nth(20)',
    ],
    seeAlso: ['number-theory.happy-seq', 'number-theory.happy-take-while', 'number-theory.happy?'],
  },
  'happy?': {
    category: 'number-theory',
    description: 'Determines if a number is a happy number.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { happy? } = import(number-theory);\nhappy?(1)',
      'let { happy? } = import(number-theory);\nhappy?(2)',
      'let { happy? } = import(number-theory);\nhappy?(100)',
    ],
    seeAlso: ['number-theory.happy-seq', 'number-theory.happy-nth', 'number-theory.happy-take-while'],
  },
  'juggler-seq': {
    category: 'number-theory',
    description: 'Generates the Juggler sequence starting from a given integer.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'integer',
        description: 'The starting integer for the Juggler sequence.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
        ],
      },
    ],
    examples: [
      'let { juggler-seq } = import(number-theory);\njuggler-seq(3)',
      'let { juggler-seq } = import(number-theory);\njuggler-seq(5)',
    ],
    seeAlso: ['number-theory.collatz-seq'],
  },
  'look-and-say-seq': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { look-and-say-seq } = import(number-theory);\nlook-and-say-seq(5)',
    ],
    seeAlso: ['number-theory.look-and-say-nth', 'number-theory.look-and-say-take-while', 'number-theory.look-and-say?'],
  },
  'look-and-say-take-while': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { look-and-say-take-while } = import(number-theory);\nlook-and-say-take-while((term, index) -> count(term) < 10)',
      'let { look-and-say-take-while } = import(number-theory);\nlook-and-say-take-while(-> $2 <= 10)',
    ],
    seeAlso: ['number-theory.look-and-say-seq', 'number-theory.look-and-say-nth', 'number-theory.look-and-say?'],
  },
  'look-and-say-nth': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { look-and-say-nth } = import(number-theory);\nlook-and-say-nth(5)',
    ],
    seeAlso: ['number-theory.look-and-say-seq', 'number-theory.look-and-say-take-while', 'number-theory.look-and-say?'],
  },
  'look-and-say?': {
    category: 'number-theory',
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
      {
        argumentNames: [
          'term',
        ],
      },
    ],
    examples: [
      'let { look-and-say? } = import(number-theory);\nlook-and-say?("111221")',
      'let { look-and-say? } = import(number-theory);\nlook-and-say?("123")',
    ],
    seeAlso: ['number-theory.look-and-say-seq', 'number-theory.look-and-say-nth', 'number-theory.look-and-say-take-while'],
  },
  'lucas-seq': {
    category: 'number-theory',
    description: 'Generates the lucas sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 77 (the maximum length of the pre-calculated Lucas numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { lucas-seq } = import(number-theory);\nlucas-seq(1)',
      'let { lucas-seq } = import(number-theory);\nlucas-seq(2)',
      'let { lucas-seq } = import(number-theory);\nlucas-seq()',
    ],
    seeAlso: ['number-theory.lucas-nth', 'number-theory.lucas-take-while', 'number-theory.lucas?', 'number-theory.fibonacci-seq'],
  },
  'lucas-take-while': {
    category: 'number-theory',
    description: 'Generates the lucas sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { lucas-take-while } = import(number-theory);\nlucas-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.lucas-seq', 'number-theory.lucas-nth', 'number-theory.lucas?'],
  },
  'lucas-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the lucas sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { lucas-nth } = import(number-theory);\nlucas-nth(1)',
      'let { lucas-nth } = import(number-theory);\nlucas-nth(2)',
      'let { lucas-nth } = import(number-theory);\nlucas-nth(10)',
    ],
    seeAlso: ['number-theory.lucas-seq', 'number-theory.lucas-take-while', 'number-theory.lucas?'],
  },
  'lucas?': {
    category: 'number-theory',
    description: 'Determines if a number is in the lucas sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { lucas? } = import(number-theory);\nlucas?(1)',
      'let { lucas? } = import(number-theory);\nlucas?(2)',
      'let { lucas? } = import(number-theory);\nlucas?(10)',
    ],
    seeAlso: ['number-theory.lucas-seq', 'number-theory.lucas-nth', 'number-theory.fibonacci?', 'number-theory.lucas-take-while'],
  },
  'lucky-seq': {
    category: 'number-theory',
    description: 'Generates the lucky sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { lucky-seq } = import(number-theory);\nlucky-seq(1)',
      'let { lucky-seq } = import(number-theory);\nlucky-seq(2)',
      'let { lucky-seq } = import(number-theory);\nlucky-seq(20)',
    ],
    seeAlso: ['number-theory.lucky-nth', 'number-theory.lucky-take-while', 'number-theory.lucky?', 'number-theory.happy-seq', 'number-theory.prime-seq'],
  },
  'lucky-take-while': {
    category: 'number-theory',
    description: 'Generates the lucky sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { lucky-take-while } = import(number-theory);\nlucky-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.lucky-seq', 'number-theory.lucky-nth', 'number-theory.lucky?'],
  },
  'lucky-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the lucky sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The position in the sequence.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { lucky-nth } = import(number-theory);\nlucky-nth(1)',
      'let { lucky-nth } = import(number-theory);\nlucky-nth(2)',
      'let { lucky-nth } = import(number-theory);\nlucky-nth(20)',
    ],
    seeAlso: ['number-theory.lucky-seq', 'number-theory.lucky-take-while', 'number-theory.lucky?'],
  },
  'lucky?': {
    category: 'number-theory',
    description: 'Checks if a number is a lucky number.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { lucky? } = import(number-theory);\nlucky?(4)',
      'let { lucky? } = import(number-theory);\nlucky?(7)',
      'let { lucky? } = import(number-theory);\nlucky?(33)',
    ],
    seeAlso: ['number-theory.lucky-seq', 'number-theory.lucky-nth', 'number-theory.prime?', 'number-theory.lucky-take-while'],
  },
  'mersenne-seq': {
    category: 'number-theory',
    description: 'Generates the Mersenne sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 9 (the maximum length of the pre-calculated mersenne numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { mersenne-seq } = import(number-theory);\nmersenne-seq(1)',
      'let { mersenne-seq } = import(number-theory);\nmersenne-seq(5)',
      'let { mersenne-seq } = import(number-theory);\nmersenne-seq()',
    ],
    seeAlso: ['number-theory.mersenne-nth', 'number-theory.mersenne-take-while', 'number-theory.mersenne?', 'number-theory.prime-seq'],
  },
  'mersenne-take-while': {
    category: 'number-theory',
    description: 'Generates the Mersenne sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { mersenne-take-while } = import(number-theory);\nmersenne-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.mersenne-seq', 'number-theory.mersenne-nth', 'number-theory.mersenne?'],
  },
  'mersenne-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Mersenne sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { mersenne-nth } = import(number-theory);\nmersenne-nth(1)',
      'let { mersenne-nth } = import(number-theory);\nmersenne-nth(5)',
    ],
    seeAlso: ['number-theory.mersenne-seq', 'number-theory.mersenne-take-while', 'number-theory.mersenne?'],
  },
  'mersenne?': {
    category: 'number-theory',
    description: 'Checks if a number is in the Mersenne sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { mersenne? } = import(number-theory);\nmersenne?(3)',
      'let { mersenne? } = import(number-theory);\nmersenne?(4)',
      'let { mersenne? } = import(number-theory);\nmersenne?(7)',
    ],
    seeAlso: ['number-theory.mersenne-seq', 'number-theory.mersenne-nth', 'number-theory.prime?', 'number-theory.mersenne-take-while'],
  },
  'padovan-seq': {
    category: 'number-theory',
    description: 'Generates the Padovan sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { padovan-seq } = import(number-theory);\npadovan-seq(5)',
      'let { padovan-seq } = import(number-theory);\npadovan-seq(10)',
      'let { padovan-seq } = import(number-theory);\npadovan-seq(20)',
    ],
    seeAlso: ['number-theory.padovan-nth', 'number-theory.padovan-take-while', 'number-theory.padovan?', 'number-theory.fibonacci-seq'],
  },
  'padovan-take-while': {
    category: 'number-theory',
    description: 'Generates the Padovan sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { padovan-take-while } = import(number-theory);\npadovan-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.padovan-seq', 'number-theory.padovan-nth', 'number-theory.padovan?'],
  },
  'padovan-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Padovan sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { padovan-nth } = import(number-theory);\npadovan-nth(5)',
      'let { padovan-nth } = import(number-theory);\npadovan-nth(10)',
      'let { padovan-nth } = import(number-theory);\npadovan-nth(20)',
    ],
    seeAlso: ['number-theory.padovan-seq', 'number-theory.padovan-take-while', 'number-theory.padovan?'],
  },
  'padovan?': {
    category: 'number-theory',
    description: 'Checks if a number is in the Padovan sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { padovan? } = import(number-theory);\npadovan?(1)',
      'let { padovan? } = import(number-theory);\npadovan?(265)',
      'let { padovan? } = import(number-theory);\npadovan?(6)',
    ],
    seeAlso: ['number-theory.padovan-seq', 'number-theory.padovan-nth', 'number-theory.fibonacci?', 'number-theory.padovan-take-while'],
  },
  'partition-seq': {
    category: 'number-theory',
    description: 'Generates the partition numbers up to a specified length. If no length is provided, it defaults to 299 (the maximum length of the pre-calculated partition numbers).',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { partition-seq } = import(number-theory);\npartition-seq(1)',
      'let { partition-seq } = import(number-theory);\npartition-seq(10)',
      'let { partition-seq } = import(number-theory);\npartition-seq()',
    ],
    seeAlso: ['number-theory.partition-nth', 'number-theory.partition-take-while', 'number-theory.partition?', 'number-theory.partitions', 'number-theory.count-partitions'],
  },
  'partition-take-while': {
    category: 'number-theory',
    description: 'Generates the partition numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { partition-take-while } = import(number-theory);\npartition-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.partition-seq', 'number-theory.partition-nth', 'number-theory.partition?'],
  },
  'partition-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the partition numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the partition number to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { partition-nth } = import(number-theory);\npartition-nth(1)',
      'let { partition-nth } = import(number-theory);\npartition-nth(5)',
    ],
    seeAlso: ['number-theory.partition-seq', 'number-theory.partition-take-while', 'number-theory.partition?'],
  },
  'partition?': {
    category: 'number-theory',
    description: 'Checks if a number is in the partition numbers.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { partition? } = import(number-theory);\npartition?(0)',
      'let { partition? } = import(number-theory);\npartition?(1)',
      'let { partition? } = import(number-theory);\npartition?(2)',
      'let { partition? } = import(number-theory);\npartition?(3)',
      'let { partition? } = import(number-theory);\npartition?(4)',
      'let { partition? } = import(number-theory);\npartition?(5)',
    ],
    seeAlso: ['number-theory.partition-seq', 'number-theory.partition-nth', 'number-theory.partitions', 'number-theory.partition-take-while'],
  },
  'pell-seq': {
    category: 'number-theory',
    description: 'Generates the Pell sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 42 (the maximum length of the pre-calculated Pell numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { pell-seq } = import(number-theory);\npell-seq(5)',
      'let { pell-seq } = import(number-theory);\npell-seq(10)',
      'let { pell-seq } = import(number-theory);\npell-seq()',
    ],
    seeAlso: ['number-theory.pell-nth', 'number-theory.pell-take-while', 'number-theory.pell?', 'number-theory.fibonacci-seq'],
  },
  'pell-take-while': {
    category: 'number-theory',
    description: 'Generates the Pell sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { pell-take-while } = import(number-theory);\npell-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.pell-seq', 'number-theory.pell-nth', 'number-theory.pell?'],
  },
  'pell-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Pell sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { pell-nth } = import(number-theory);\npell-nth(5)',
      'let { pell-nth } = import(number-theory);\npell-nth(10)',
      'let { pell-nth } = import(number-theory);\npell-nth(20)',
    ],
    seeAlso: ['number-theory.pell-seq', 'number-theory.pell-take-while', 'number-theory.pell?'],
  },
  'pell?': {
    category: 'number-theory',
    description: 'Checks if a number is a Pell number.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { pell? } = import(number-theory);\npell?(1)',
      'let { pell? } = import(number-theory);\npell?(470832)',
      'let { pell? } = import(number-theory);\npell?(10)',
    ],
    seeAlso: ['number-theory.pell-seq', 'number-theory.pell-nth', 'number-theory.fibonacci?', 'number-theory.pell-take-while'],
  },
  'perfect-seq': {
    category: 'number-theory',
    description: 'Generates the perfect numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If no length is provided, it defaults to 7 (the maximum length of the pre-calculated perfect numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { perfect-seq } = import(number-theory);\nperfect-seq(1)',
      'let { perfect-seq } = import(number-theory);\nperfect-seq(5)',
      'let { perfect-seq } = import(number-theory);\nperfect-seq()',
    ],
    seeAlso: ['number-theory.perfect-nth', 'number-theory.perfect-take-while', 'number-theory.perfect?', 'number-theory.abundant-seq', 'number-theory.deficient-seq', 'number-theory.amicable?'],
  },
  'perfect-take-while': {
    category: 'number-theory',
    description: 'Generates the perfect numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { perfect-take-while } = import(number-theory);\nperfect-take-while(-> $ < 1000)',
    ],
    seeAlso: ['number-theory.perfect-seq', 'number-theory.perfect-nth', 'number-theory.perfect?'],
  },
  'perfect-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the perfect numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the perfect number to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-nth } = import(number-theory);\nperfect-nth(1)',
      'let { perfect-nth } = import(number-theory);\nperfect-nth(5)',
    ],
    seeAlso: ['number-theory.perfect-seq', 'number-theory.perfect-take-while', 'number-theory.perfect?'],
  },
  'perfect?': {
    category: 'number-theory',
    description: 'Checks if a number is in the perfect numbers.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect? } = import(number-theory);\nperfect?(0)',
      'let { perfect? } = import(number-theory);\nperfect?(1)',
      'let { perfect? } = import(number-theory);\nperfect?(2)',
      'let { perfect? } = import(number-theory);\nperfect?(3)',
      'let { perfect? } = import(number-theory);\nperfect?(4)',
      'let { perfect? } = import(number-theory);\nperfect?(5)',
      'let { perfect? } = import(number-theory);\nperfect?(6)',
      'let { perfect? } = import(number-theory);\nperfect?(7)',
      'let { perfect? } = import(number-theory);\nperfect?(8)',
      'let { perfect? } = import(number-theory);\nperfect?(9)',
    ],
    seeAlso: ['number-theory.perfect-seq', 'number-theory.perfect-nth', 'number-theory.abundant?', 'number-theory.deficient?', 'number-theory.sigma', 'number-theory.perfect-take-while', 'number-theory.amicable?', 'number-theory.proper-divisors'],
  },
  'perfect-square-seq': {
    category: 'number-theory',
    description: 'Generates the perfect square numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { perfect-square-seq } = import(number-theory);\nperfect-square-seq(5)',
      'let { perfect-square-seq } = import(number-theory);\nperfect-square-seq(20)',
    ],
    seeAlso: ['number-theory.perfect-square-nth', 'number-theory.perfect-square-take-while', 'number-theory.perfect-square?', 'number-theory.perfect-cube-seq', 'number-theory.perfect-power-seq', 'number-theory.polygonal-seq'],
  },
  'perfect-square-take-while': {
    category: 'number-theory',
    description: 'Generates the perfect square numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { perfect-square-take-while } = import(number-theory);\nperfect-square-take-while(-> $ <= 100)',
    ],
    seeAlso: ['number-theory.perfect-square-seq', 'number-theory.perfect-square-nth', 'number-theory.perfect-square?'],
  },
  'perfect-square-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the perfect square numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-square-nth } = import(number-theory);\nperfect-square-nth(1)',
      'let { perfect-square-nth } = import(number-theory);\nperfect-square-nth(5)',
    ],
    seeAlso: ['number-theory.perfect-square-seq', 'number-theory.perfect-square-take-while', 'number-theory.perfect-square?'],
  },
  'perfect-square?': {
    category: 'number-theory',
    description: 'Checks if a number is a perfect square.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-square? } = import(number-theory);\nperfect-square?(16)',
      'let { perfect-square? } = import(number-theory);\nperfect-square?(20)',
    ],
    seeAlso: ['number-theory.perfect-square-seq', 'number-theory.perfect-square-nth', 'number-theory.perfect-cube?', 'number-theory.perfect-power?', 'number-theory.perfect-square-take-while', 'number-theory.perfect-power', 'number-theory.polygonal?'],
  },
  'perfect-cube-seq': {
    category: 'number-theory',
    description: 'Generates the perfect cube numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { perfect-cube-seq } = import(number-theory);\nperfect-cube-seq(5)',
      'let { perfect-cube-seq } = import(number-theory);\nperfect-cube-seq(20)',
    ],
    seeAlso: ['number-theory.perfect-cube-nth', 'number-theory.perfect-cube-take-while', 'number-theory.perfect-cube?', 'number-theory.perfect-square-seq', 'number-theory.perfect-power-seq'],
  },
  'perfect-cube-take-while': {
    category: 'number-theory',
    description: 'Generates the perfect cube numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { perfect-cube-take-while } = import(number-theory);\nperfect-cube-take-while(-> $ <= 100)',
    ],
    seeAlso: ['number-theory.perfect-cube-seq', 'number-theory.perfect-cube-nth', 'number-theory.perfect-cube?'],
  },
  'perfect-cube-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the perfect cube numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-cube-nth } = import(number-theory);\nperfect-cube-nth(1)',
      'let { perfect-cube-nth } = import(number-theory);\nperfect-cube-nth(5)',
    ],
    seeAlso: ['number-theory.perfect-cube-seq', 'number-theory.perfect-cube-take-while', 'number-theory.perfect-cube?'],
  },
  'perfect-cube?': {
    category: 'number-theory',
    description: 'Checks if a number is in the perfect cube numbers.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-cube? } = import(number-theory);\nperfect-cube?(7)',
      'let { perfect-cube? } = import(number-theory);\nperfect-cube?(8)',
      'let { perfect-cube? } = import(number-theory);\nperfect-cube?(9)',
    ],
    seeAlso: ['number-theory.perfect-cube-seq', 'number-theory.perfect-cube-nth', 'number-theory.perfect-square?', 'number-theory.perfect-power?', 'number-theory.perfect-cube-take-while', 'number-theory.perfect-power'],
  },
  'perfect-power-seq': {
    category: 'number-theory',
    description: 'Generates the perfect power numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { perfect-power-seq } = import(number-theory);\nperfect-power-seq(5)',
      'let { perfect-power-seq } = import(number-theory);\nperfect-power-seq(20)',
    ],
    seeAlso: ['number-theory.perfect-power-nth', 'number-theory.perfect-power-take-while', 'number-theory.perfect-power?', 'number-theory.perfect-power', 'number-theory.perfect-square-seq', 'number-theory.perfect-cube-seq'],
  },
  'perfect-power-take-while': {
    category: 'number-theory',
    description: 'Generates the perfect power numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { perfect-power-take-while } = import(number-theory);\nperfect-power-take-while(-> $ <= 100)',
    ],
    seeAlso: ['number-theory.perfect-power-seq', 'number-theory.perfect-power-nth', 'number-theory.perfect-power?'],
  },
  'perfect-power-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the perfect power numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-power-nth } = import(number-theory);\nperfect-power-nth(3)',
      'let { perfect-power-nth } = import(number-theory);\nperfect-power-nth(15)',
    ],
    seeAlso: ['number-theory.perfect-power-seq', 'number-theory.perfect-power-take-while', 'number-theory.perfect-power?'],
  },
  'perfect-power?': {
    category: 'number-theory',
    description: 'Checks if a number is in the perfect power numbers.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-power? } = import(number-theory);\nperfect-power?(7)',
      'let { perfect-power? } = import(number-theory);\nperfect-power?(8)',
      'let { perfect-power? } = import(number-theory);\nperfect-power?(9)',
      'let { perfect-power? } = import(number-theory);\nperfect-power?(10)',
    ],
    seeAlso: ['number-theory.perfect-power-seq', 'number-theory.perfect-power-nth', 'number-theory.perfect-power', 'number-theory.perfect-square?', 'number-theory.perfect-cube?', 'number-theory.perfect-power-take-while'],
  },
  'polygonal-seq': {
    category: 'number-theory',
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
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'sides',
          'length',
        ],
      },
    ],
    examples: [
      'let { polygonal-seq } = import(number-theory);\npolygonal-seq(3, 2)',
      'let { polygonal-seq } = import(number-theory);\npolygonal-seq(4, 2)',
      'let { polygonal-seq } = import(number-theory);\npolygonal-seq(5, 3)',
      'let { polygonal-seq } = import(number-theory);\npolygonal-seq(6, 5)',
      'let { polygonal-seq } = import(number-theory);\npolygonal-seq(100, 10)',
    ],
    seeAlso: ['number-theory.polygonal-nth', 'number-theory.polygonal-take-while', 'number-theory.polygonal?', 'number-theory.perfect-square-seq'],
  },
  'polygonal-take-while': {
    category: 'number-theory',
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
      a: {
        type: 'integer',
      },
      b: {
        type: 'function',
      },
    },
    variants: [
      {
        argumentNames: [
          'sides',
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { polygonal-take-while } = import(number-theory);\npolygonal-take-while(15, -> $ < 1000)',
    ],
    seeAlso: ['number-theory.polygonal-seq', 'number-theory.polygonal-nth', 'number-theory.polygonal?'],
  },
  'polygonal-nth': {
    category: 'number-theory',
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
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'sides',
          'n',
        ],
      },
    ],
    examples: [
      'let { polygonal-nth } = import(number-theory);\npolygonal-nth(3, 9)',
      'let { polygonal-nth } = import(number-theory);\npolygonal-nth(4, 5)',
      'let { polygonal-nth } = import(number-theory);\npolygonal-nth(5, 5)',
    ],
    seeAlso: ['number-theory.polygonal-seq', 'number-theory.polygonal-take-while', 'number-theory.polygonal?'],
  },
  'polygonal?': {
    category: 'number-theory',
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
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'sides',
          'n',
        ],
      },
    ],
    examples: [
      'let { polygonal? } = import(number-theory);\npolygonal?(3, 10)',
      'let { polygonal? } = import(number-theory);\npolygonal?(3, 9)',
      'let { polygonal? } = import(number-theory);\npolygonal?(4, 10000)',
      'let { polygonal? } = import(number-theory);\npolygonal?(4, 1000)',
      'let { polygonal? } = import(number-theory);\npolygonal?(6, 45)',
    ],
    seeAlso: ['number-theory.polygonal-seq', 'number-theory.polygonal-nth', 'number-theory.perfect-square?', 'number-theory.polygonal-take-while'],
  },
  'prime-seq': {
    category: 'number-theory',
    description: 'Generates the prime sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { prime-seq } = import(number-theory);\nprime-seq(1)',
      'let { prime-seq } = import(number-theory);\nprime-seq(2)',
      'let { prime-seq } = import(number-theory);\nprime-seq(10)',
    ],
    seeAlso: ['number-theory.prime-nth', 'number-theory.prime-take-while', 'number-theory.prime?', 'number-theory.composite-seq', 'number-theory.mersenne-seq', 'number-theory.lucky-seq'],
  },
  'prime-take-while': {
    category: 'number-theory',
    description: 'Generates the prime sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { prime-take-while } = import(number-theory);\nprime-take-while(-> $ < 50)',
    ],
    seeAlso: ['number-theory.prime-seq', 'number-theory.prime-nth', 'number-theory.prime?'],
  },
  'prime-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the prime sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { prime-nth } = import(number-theory);\nprime-nth(1)',
      'let { prime-nth } = import(number-theory);\nprime-nth(2)',
      'let { prime-nth } = import(number-theory);\nprime-nth(10)',
    ],
    seeAlso: ['number-theory.prime-seq', 'number-theory.prime-take-while', 'number-theory.prime?'],
  },
  'prime?': {
    category: 'number-theory',
    description: 'Determines if a number is prime.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { prime? } = import(number-theory);\nprime?(1)',
      'let { prime? } = import(number-theory);\nprime?(2)',
      'let { prime? } = import(number-theory);\nprime?(3)',
      'let { prime? } = import(number-theory);\nprime?(4)',
      'let { prime? } = import(number-theory);\nprime?(997)',
      'let { prime? } = import(number-theory);\nprime?(1001)',
    ],
    seeAlso: ['number-theory.prime-seq', 'number-theory.prime-nth', 'number-theory.composite?', 'number-theory.prime-factors', 'number-theory.mersenne?', 'number-theory.prime-take-while', 'number-theory.lucky?'],
  },
  'recaman-seq': {
    category: 'number-theory',
    description: 'Generates the Recaman sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { recaman-seq } = import(number-theory);\nrecaman-seq(5)',
      'let { recaman-seq } = import(number-theory);\nrecaman-seq(10)',
      'let { recaman-seq } = import(number-theory);\nrecaman-seq(20)',
    ],
    seeAlso: ['number-theory.recaman-nth', 'number-theory.recaman-take-while', 'number-theory.recaman?', 'number-theory.golomb-seq'],
  },
  'recaman-take-while': {
    category: 'number-theory',
    description: 'Generates the Recaman sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { recaman-take-while } = import(number-theory);\nrecaman-take-while(-> $ < 10)',
    ],
    seeAlso: ['number-theory.recaman-seq', 'number-theory.recaman-nth', 'number-theory.recaman?'],
  },
  'recaman-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Recaman sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { recaman-nth } = import(number-theory);\nrecaman-nth(5)',
      'let { recaman-nth } = import(number-theory);\nrecaman-nth(10)',
      'let { recaman-nth } = import(number-theory);\nrecaman-nth(20)',
    ],
    seeAlso: ['number-theory.recaman-seq', 'number-theory.recaman-take-while', 'number-theory.recaman?'],
  },
  'recaman?': {
    category: 'number-theory',
    description: 'Checks if a number is in the Recaman sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { recaman? } = import(number-theory);\nrecaman?(5)',
      'let { recaman? } = import(number-theory);\nrecaman?(10)',
      'let { recaman? } = import(number-theory);\nrecaman?(20)',
    ],
    seeAlso: ['number-theory.recaman-seq', 'number-theory.recaman-nth', 'number-theory.recaman-take-while'],
  },
  'sylvester-seq': {
    category: 'number-theory',
    description: 'Generates the Sylvester sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 6 (the maximum length of the pre-calculated Sylvester numbers).',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
      {
        argumentNames: [],
      },
    ],
    examples: [
      'let { sylvester-seq } = import(number-theory);\nsylvester-seq(5)',
      'let { sylvester-seq } = import(number-theory);\nsylvester-seq()',
      'let { sylvester-seq } = import(number-theory);\nsylvester-seq()',
    ],
    seeAlso: ['number-theory.sylvester-nth', 'number-theory.sylvester-take-while', 'number-theory.sylvester?'],
  },
  'sylvester-take-while': {
    category: 'number-theory',
    description: 'Generates the Sylvester sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { sylvester-take-while } = import(number-theory);\nsylvester-take-while(-> $ < 100000)',
    ],
    seeAlso: ['number-theory.sylvester-seq', 'number-theory.sylvester-nth', 'number-theory.sylvester?'],
  },
  'sylvester-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Sylvester sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { sylvester-nth } = import(number-theory);\nsylvester-nth(1)',
      'let { sylvester-nth } = import(number-theory);\nsylvester-nth(5)',
    ],
    seeAlso: ['number-theory.sylvester-seq', 'number-theory.sylvester-take-while', 'number-theory.sylvester?'],
  },
  'sylvester?': {
    category: 'number-theory',
    description: 'Checks if a number is in the Sylvester sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { sylvester? } = import(number-theory);\nsylvester?(2)',
      'let { sylvester? } = import(number-theory);\nsylvester?(3)',
      'let { sylvester? } = import(number-theory);\nsylvester?(6)',
    ],
    seeAlso: ['number-theory.sylvester-seq', 'number-theory.sylvester-nth', 'number-theory.sylvester-take-while'],
  },
  'thue-morse-seq': {
    category: 'number-theory',
    description: 'Generates the Thue-Morse sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { thue-morse-seq } = import(number-theory);\nthue-morse-seq(5)',
      'let { thue-morse-seq } = import(number-theory);\nthue-morse-seq(10)',
      'let { thue-morse-seq } = import(number-theory);\nthue-morse-seq(20)',
    ],
    seeAlso: ['number-theory.thue-morse-nth', 'number-theory.thue-morse-take-while', 'number-theory.thue-morse?'],
  },
  'thue-morse-take-while': {
    category: 'number-theory',
    description: 'Generates the Thue-Morse sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { thue-morse-take-while } = import(number-theory);\nthue-morse-take-while(-> $2 < 10)',
    ],
    seeAlso: ['number-theory.thue-morse-seq', 'number-theory.thue-morse-nth', 'number-theory.thue-morse?'],
  },
  'thue-morse-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the Thue-Morse sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term in the sequence.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { thue-morse-nth } = import(number-theory);\nthue-morse-nth(5)',
      'let { thue-morse-nth } = import(number-theory);\nthue-morse-nth(10)',
      'let { thue-morse-nth } = import(number-theory);\nthue-morse-nth(20)',
    ],
    seeAlso: ['number-theory.thue-morse-seq', 'number-theory.thue-morse-take-while', 'number-theory.thue-morse?'],
  },
  'thue-morse?': {
    category: 'number-theory',
    description: 'Checks if a number is part of the Thue-Morse sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { thue-morse? } = import(number-theory);\nthue-morse?(1)',
      'let { thue-morse? } = import(number-theory);\nthue-morse?(2)',
    ],
    seeAlso: ['number-theory.thue-morse-seq', 'number-theory.thue-morse-nth', 'number-theory.thue-morse-take-while'],
  },
  'tribonacci-seq': {
    category: 'number-theory',
    description: 'Generates the tribonacci sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { tribonacci-seq } = import(number-theory);\ntribonacci-seq(1)',
      'let { tribonacci-seq } = import(number-theory);\ntribonacci-seq(2)',
      'let { tribonacci-seq } = import(number-theory);\ntribonacci-seq(10)',
    ],
    seeAlso: ['number-theory.tribonacci-nth', 'number-theory.tribonacci-take-while', 'number-theory.tribonacci?', 'number-theory.fibonacci-seq'],
  },
  'tribonacci-take-while': {
    category: 'number-theory',
    description: 'Generates the tribonacci sequence while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { tribonacci-take-while } = import(number-theory);\ntribonacci-take-while(-> $ < 100)',
    ],
    seeAlso: ['number-theory.tribonacci-seq', 'number-theory.tribonacci-nth', 'number-theory.tribonacci?'],
  },
  'tribonacci-nth': {
    category: 'number-theory',
    description: 'Generates the nth term of the tribonacci sequence.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { tribonacci-nth } = import(number-theory);\ntribonacci-nth(1)',
      'let { tribonacci-nth } = import(number-theory);\ntribonacci-nth(2)',
      'let { tribonacci-nth } = import(number-theory);\ntribonacci-nth(10)',
    ],
    seeAlso: ['number-theory.tribonacci-seq', 'number-theory.tribonacci-take-while', 'number-theory.tribonacci?'],
  },
  'tribonacci?': {
    category: 'number-theory',
    description: 'Determines if a number is in the tribonacci sequence.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { tribonacci? } = import(number-theory);\ntribonacci?(0)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(1)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(2)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(3)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(4)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(5)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(6)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(7)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(8)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(9)',
      'let { tribonacci? } = import(number-theory);\ntribonacci?(10)',
    ],
    seeAlso: ['number-theory.tribonacci-seq', 'number-theory.tribonacci-nth', 'number-theory.fibonacci?', 'number-theory.tribonacci-take-while'],
  },
  'count-combinations': {
    category: 'number-theory',
    description: 'Calculates the number of combinations of n items taken k at a time.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { count-combinations } = import(number-theory);\ncount-combinations(5, 3)',
      'let { count-combinations } = import(number-theory);\ncount-combinations(10, 2)',
    ],
    seeAlso: ['number-theory.combinations', 'number-theory.count-permutations', 'number-theory.factorial', 'number-theory.multinomial', 'number-theory.stirling-second', 'number-theory.count-partitions', 'number-theory.count-power-set'],
  },
  'combinations': {
    category: 'number-theory',
    description: 'Generates all possible combinations of a specified size from a collection.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'array',
        array: true,
        description: 'The input collection to generate combinations from.',
      },
      n: {
        type: 'integer',
        description: 'The size of each combination.',
      },
      a: {
        type: 'array',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'set',
          'n',
        ],
      },
    ],
    examples: [
      'let { combinations } = import(number-theory);\ncombinations([1, 2, 3], 2)',
      'let { combinations } = import(number-theory);\ncombinations(["a", "b", "c"], 2)',
      'let { combinations } = import(number-theory);\ncombinations([1, 2, 3], 0)',
      'let { combinations } = import(number-theory);\ncombinations([1, 2, 3], 1)',
      'let { combinations } = import(number-theory);\ncombinations([1, 2, 3], 3)',
    ],
    seeAlso: ['number-theory.count-combinations', 'number-theory.permutations', 'number-theory.power-set', 'number-theory.cartesian-product', 'number-theory.partitions'],
  },
  'count-derangements': {
    category: 'number-theory',
    description: 'Calculates the number of derangements (permutations where no element appears in its original position) of n items.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The total number of items.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-derangements } = import(number-theory);\ncount-derangements(4)',
      'let { count-derangements } = import(number-theory);\ncount-derangements(5)',
    ],
    seeAlso: ['number-theory.derangements', 'number-theory.count-permutations', 'number-theory.factorial'],
  },
  'derangements': {
    category: 'number-theory',
    description: 'Generates all derangements (permutations where no element appears in its original position) of a set.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'array',
        array: true,
        description: 'The input collection to generate derangements from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'set',
        ],
      },
    ],
    examples: [
      'let { derangements } = import(number-theory);\nderangements([1, 2, 3, 4])',
      'let { derangements } = import(number-theory);\nderangements(["a", "b", "c"])',
    ],
    seeAlso: ['number-theory.count-derangements', 'number-theory.permutations'],
  },
  'divisors': {
    category: 'number-theory',
    description: 'Returns the divisors of a number.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to find divisors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { divisors } = import(number-theory);\ndivisors(12)',
      'let { divisors } = import(number-theory);\ndivisors(100)',
      'let { divisors } = import(number-theory);\ndivisors(37)',
    ],
    seeAlso: ['number-theory.count-divisors', 'number-theory.proper-divisors', 'number-theory.sigma', 'number-theory.prime-factors', 'number-theory.divisible-by?', 'number-theory.lcm', 'number-theory.abundant?', 'number-theory.deficient?', 'number-theory.count-proper-divisors'],
  },
  'count-divisors': {
    category: 'number-theory',
    description: 'Returns the number of divisors of a number.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count divisors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-divisors } = import(number-theory);\ncount-divisors(12)',
      'let { count-divisors } = import(number-theory);\ncount-divisors(100)',
      'let { count-divisors } = import(number-theory);\ncount-divisors(37)',
    ],
    seeAlso: ['number-theory.divisors', 'number-theory.count-proper-divisors', 'number-theory.sigma'],
  },
  'proper-divisors': {
    category: 'number-theory',
    description: 'Returns the proper divisors of a number.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to find proper divisors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { proper-divisors } = import(number-theory);\nproper-divisors(12)',
      'let { proper-divisors } = import(number-theory);\nproper-divisors(100)',
      'let { proper-divisors } = import(number-theory);\nproper-divisors(37)',
    ],
    seeAlso: ['number-theory.count-proper-divisors', 'number-theory.divisors', 'number-theory.amicable?', 'number-theory.perfect?'],
  },
  'count-proper-divisors': {
    category: 'number-theory',
    description: 'Returns the number of proper divisors of a number.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count proper divisors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-proper-divisors } = import(number-theory);\ncount-proper-divisors(12)',
      'let { count-proper-divisors } = import(number-theory);\ncount-proper-divisors(100)',
      'let { count-proper-divisors } = import(number-theory);\ncount-proper-divisors(37)',
    ],
    seeAlso: ['number-theory.proper-divisors', 'number-theory.count-divisors', 'number-theory.divisors'],
  },
  'factorial': {
    category: 'number-theory',
    description: 'Calculates the factorial of a number.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the factorial for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { factorial } = import(number-theory);\nfactorial(5)',
      'let { factorial } = import(number-theory);\nfactorial(0)',
      'let { factorial } = import(number-theory);\nfactorial(10)',
      'let { factorial } = import(number-theory);\nfactorial(20)',
    ],
    seeAlso: ['number-theory.factorial-seq', 'number-theory.factorial-nth', 'number-theory.factorial?', 'number-theory.count-combinations', 'number-theory.count-permutations', 'number-theory.multinomial', 'number-theory.count-derangements'],
  },
  'partitions': {
    category: 'number-theory',
    description: 'Generates all partitions of a number.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to partition.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { partitions } = import(number-theory);\npartitions(4)',
      'let { partitions } = import(number-theory);\npartitions(8)',
    ],
    seeAlso: ['number-theory.count-partitions', 'number-theory.partition-seq', 'number-theory.combinations', 'number-theory.partition?'],
  },
  'count-partitions': {
    category: 'number-theory',
    description: 'Returns the number of partitions of a number.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count partitions for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-partitions } = import(number-theory);\ncount-partitions(4)',
      'let { count-partitions } = import(number-theory);\ncount-partitions(8)',
      'let { count-partitions } = import(number-theory);\ncount-partitions(15)',
    ],
    seeAlso: ['number-theory.partitions', 'number-theory.partition-seq', 'number-theory.count-combinations'],
  },
  'permutations': {
    category: 'number-theory',
    description: 'Generates all permutations of a collection.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'array',
        array: true,
        description: 'The input collection to generate permutations from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'set',
        ],
      },
    ],
    examples: [
      'let { permutations } = import(number-theory);\npermutations([1, 2, 3])',
      'let { permutations } = import(number-theory);\npermutations(["a", "b", "c"])',
      'let { permutations } = import(number-theory);\npermutations([1, 2, 3, 4])',
      'let { permutations } = import(number-theory);\npermutations([1, 2])',
      'let { permutations } = import(number-theory);\npermutations([1])',
      'let { permutations } = import(number-theory);\npermutations([])',
    ],
    seeAlso: ['number-theory.count-permutations', 'number-theory.combinations', 'number-theory.derangements', 'number-theory.cartesian-product'],
  },
  'count-permutations': {
    category: 'number-theory',
    description: 'Returns the number of permutations of n items taken k at a time.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { count-permutations } = import(number-theory);\ncount-permutations(5, 3)',
      'let { count-permutations } = import(number-theory);\ncount-permutations(10, 2)',
      'let { count-permutations } = import(number-theory);\ncount-permutations(10, 10)',
      'let { count-permutations } = import(number-theory);\ncount-permutations(10, 0)',
      'let { count-permutations } = import(number-theory);\ncount-permutations(10, 1)',
    ],
    seeAlso: ['number-theory.permutations', 'number-theory.count-combinations', 'number-theory.factorial', 'number-theory.multinomial', 'number-theory.stirling-first', 'number-theory.count-derangements'],
  },
  'power-set': {
    category: 'number-theory',
    description: 'Generates the power set of a collection.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      set: {
        type: 'any',
        array: true,
        description: 'The input collection to generate the power set from.',
      },
    },
    variants: [
      {
        argumentNames: [
          'set',
        ],
      },
    ],
    examples: [
      'let { power-set } = import(number-theory);\npower-set(["a", "b", "c"])',
      'let { power-set } = import(number-theory);\npower-set([1, 2])',
      'let { power-set } = import(number-theory);\npower-set([1])',
      'let { power-set } = import(number-theory);\npower-set([])',
    ],
    seeAlso: ['number-theory.count-power-set', 'number-theory.combinations', 'number-theory.cartesian-product'],
  },
  'count-power-set': {
    category: 'number-theory',
    description: 'Returns the number of subsets of a set.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The size of the set.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-power-set } = import(number-theory);\ncount-power-set(3)',
      'let { count-power-set } = import(number-theory);\ncount-power-set(5)',
      'let { count-power-set } = import(number-theory);\ncount-power-set(10)',
    ],
    seeAlso: ['number-theory.power-set', 'number-theory.count-combinations'],
  },
  'prime-factors': {
    category: 'number-theory',
    description: 'Returns the prime factors of a number.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to factor.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { prime-factors } = import(number-theory);\nprime-factors(12)',
      'let { prime-factors } = import(number-theory);\nprime-factors(100)',
      'let { prime-factors } = import(number-theory);\nprime-factors(37)',
    ],
    seeAlso: ['number-theory.count-prime-factors', 'number-theory.distinct-prime-factors', 'number-theory.prime?', 'number-theory.divisors', 'number-theory.euler-totient', 'number-theory.mobius', 'number-theory.composite?', 'number-theory.count-distinct-prime-factors'],
  },
  'count-prime-factors': {
    category: 'number-theory',
    description: 'Returns the number of prime factors of a number.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count prime factors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-prime-factors } = import(number-theory);\ncount-prime-factors(12)',
      'let { count-prime-factors } = import(number-theory);\ncount-prime-factors(100)',
      'let { count-prime-factors } = import(number-theory);\ncount-prime-factors(37)',
    ],
    seeAlso: ['number-theory.prime-factors', 'number-theory.distinct-prime-factors', 'number-theory.count-distinct-prime-factors'],
  },
  'distinct-prime-factors': {
    category: 'number-theory',
    description: 'Returns the distinct prime factors of a number.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to find distinct prime factors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { distinct-prime-factors } = import(number-theory);\ndistinct-prime-factors(12)',
      'let { distinct-prime-factors } = import(number-theory);\ndistinct-prime-factors(100)',
      'let { distinct-prime-factors } = import(number-theory);\ndistinct-prime-factors(37)',
    ],
    seeAlso: ['number-theory.prime-factors', 'number-theory.count-distinct-prime-factors', 'number-theory.count-prime-factors'],
  },
  'count-distinct-prime-factors': {
    category: 'number-theory',
    description: 'Returns the number of distinct prime factors of a number.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to count distinct prime factors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { count-distinct-prime-factors } = import(number-theory);\ncount-distinct-prime-factors(12)',
      'let { count-distinct-prime-factors } = import(number-theory);\ncount-distinct-prime-factors(100)',
      'let { count-distinct-prime-factors } = import(number-theory);\ncount-distinct-prime-factors(37)',
    ],
    seeAlso: ['number-theory.distinct-prime-factors', 'number-theory.prime-factors', 'number-theory.count-prime-factors'],
  },
  'coprime?': {
    category: 'number-theory',
    description: 'Checks if two numbers are coprime (i.e., their GCD is 1).',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { coprime? } = import(number-theory);\ncoprime?(12, 8)',
      'let { coprime? } = import(number-theory);\ncoprime?(12, 5)',
      'let { coprime? } = import(number-theory);\ncoprime?(37, 1)',
      'let { coprime? } = import(number-theory);\ncoprime?(0, 0)',
      'let { coprime? } = import(number-theory);\ncoprime?(0, 5)',
      'let { coprime? } = import(number-theory);\ncoprime?(5, 0)',
      'let { coprime? } = import(number-theory);\ncoprime?(1, 0)',
      'let { coprime? } = import(number-theory);\ncoprime?(0, 1)',
      'let { coprime? } = import(number-theory);\ncoprime?(1, 1)',
      'let { coprime? } = import(number-theory);\ncoprime?(2, 3)',
    ],
    seeAlso: ['number-theory.gcd', 'number-theory.euler-totient', 'number-theory.divisible-by?', 'number-theory.lcm', 'number-theory.carmichael-lambda'],
  },
  'divisible-by?': {
    category: 'number-theory',
    description: 'Checks if a number is divisible by another number.',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { divisible-by? } = import(number-theory);\ndivisible-by?(12, 4)',
      'let { divisible-by? } = import(number-theory);\ndivisible-by?(12, 5)',
      'let { divisible-by? } = import(number-theory);\ndivisible-by?(37, 1)',
      'let { divisible-by? } = import(number-theory);\ndivisible-by?(0, 0)',
      'let { divisible-by? } = import(number-theory);\ndivisible-by?(0, 5)',
      'let { divisible-by? } = import(number-theory);\ndivisible-by?(5, 0)',
    ],
    seeAlso: ['number-theory.divisors', 'number-theory.gcd', 'number-theory.coprime?'],
  },
  'gcd': {
    category: 'number-theory',
    description: 'Calculates the greatest common divisor (GCD) of two numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { gcd } = import(number-theory);\ngcd(100, 25)',
      'let { gcd } = import(number-theory);\ngcd(37, 1)',
      'let { gcd } = import(number-theory);\ngcd(0, 0)',
      'let { gcd } = import(number-theory);\ngcd(0, 5)',
      'let { gcd } = import(number-theory);\ngcd(5, 0)',
    ],
    seeAlso: ['number-theory.lcm', 'number-theory.extended-gcd', 'number-theory.coprime?', 'number-theory.divisible-by?'],
  },
  'lcm': {
    category: 'number-theory',
    description: 'Calculates the least common multiple (LCM) of two numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { lcm } = import(number-theory);\nlcm(100, 25)',
      'let { lcm } = import(number-theory);\nlcm(37, 1)',
      'let { lcm } = import(number-theory);\nlcm(0, 5)',
      'let { lcm } = import(number-theory);\nlcm(5, 0)',
    ],
    seeAlso: ['number-theory.gcd', 'number-theory.divisors', 'number-theory.coprime?'],
  },
  'multinomial': {
    category: 'number-theory',
    description: 'Calculates the multinomial coefficient from of a list of numbers representing the sizes of each group.',
    returns: {
      type: 'integer',
    },
    args: {
      args: {
        type: 'integer',
        rest: true,
        description: 'The numbers representing the sizes of each group.',
      },
    },
    variants: [
      {
        argumentNames: [
          'args',
        ],
      },
    ],
    examples: [
      'let { multinomial } = import(number-theory);\nmultinomial(5, 2, 3)',
      'let { multinomial } = import(number-theory);\nmultinomial(10, 2, 3, 5)',
    ],
    seeAlso: ['number-theory.count-combinations', 'number-theory.factorial', 'number-theory.count-permutations'],
    hideOperatorForm: true,
  },
  'amicable?': {
    category: 'number-theory',
    description: 'Checks if two numbers are amicable (i.e., the sum of the proper divisors of each number equals the other number).',
    returns: {
      type: 'boolean',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { amicable? } = import(number-theory);\namicable?(220, 284)',
      'let { amicable? } = import(number-theory);\namicable?(1184, 1210)',
      'let { amicable? } = import(number-theory);\namicable?(2620, 2924)',
      'let { amicable? } = import(number-theory);\namicable?(5020, 5564)',
      'let { amicable? } = import(number-theory);\namicable?(6232, 6368)',
    ],
    seeAlso: ['number-theory.proper-divisors', 'number-theory.perfect?', 'number-theory.sigma', 'number-theory.perfect-seq'],
  },
  'euler-totient': {
    category: 'number-theory',
    description: 'Calculates the Euler\'s totient function (φ(n)) of a number, which counts the integers up to n that are coprime to n.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the totient for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { euler-totient } = import(number-theory);\neuler-totient(1)',
      'let { euler-totient } = import(number-theory);\neuler-totient(2)',
      'let { euler-totient } = import(number-theory);\neuler-totient(10)',
      'let { euler-totient } = import(number-theory);\neuler-totient(20)',
    ],
    seeAlso: ['number-theory.coprime?', 'number-theory.carmichael-lambda', 'number-theory.mobius', 'number-theory.prime-factors', 'number-theory.mertens'],
  },
  'mobius': {
    category: 'number-theory',
    description: 'Calculates the Möbius function (μ(n)) of a number, which is used in number theory.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the Möbius function for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { mobius } = import(number-theory);\nmobius(1)',
      'let { mobius } = import(number-theory);\nmobius(2)',
      'let { mobius } = import(number-theory);\nmobius(3)',
      'let { mobius } = import(number-theory);\nmobius(4)',
      'let { mobius } = import(number-theory);\nmobius(6)',
      'let { mobius } = import(number-theory);\nmobius(12)',
      'let { mobius } = import(number-theory);\nmobius(30)',
    ],
    seeAlso: ['number-theory.mertens', 'number-theory.euler-totient', 'number-theory.prime-factors'],
  },
  'mertens': {
    category: 'number-theory',
    description: 'Calculates the Mertens function (M(n)) of a number, which is the sum of the Möbius function up to n.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the Mertens function for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { mobius } = import(number-theory);\nmobius(1)',
      'let { mobius } = import(number-theory);\nmobius(2)',
      'let { mobius } = import(number-theory);\nmobius(3)',
      'let { mobius } = import(number-theory);\nmobius(4)',
      'let { mobius } = import(number-theory);\nmobius(6)',
      'let { mobius } = import(number-theory);\nmobius(12)',
      'let { mobius } = import(number-theory);\nmobius(30)',
    ],
    seeAlso: ['number-theory.mobius', 'number-theory.euler-totient'],
  },
  'sigma': {
    category: 'number-theory',
    description: 'Calculates the sum of divisors function (σ(n)) of a number, which is the sum of all positive divisors of n.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the sum of divisors for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { sigma } = import(number-theory);\nsigma(1)',
      'let { sigma } = import(number-theory);\nsigma(2)',
      'let { sigma } = import(number-theory);\nsigma(3)',
      'let { sigma } = import(number-theory);\nsigma(4)',
      'let { sigma } = import(number-theory);\nsigma(6)',
      'let { sigma } = import(number-theory);\nsigma(12)',
      'let { sigma } = import(number-theory);\nsigma(30)',
    ],
    seeAlso: ['number-theory.divisors', 'number-theory.perfect?', 'number-theory.abundant?', 'number-theory.deficient?', 'number-theory.amicable?', 'number-theory.count-divisors'],
  },
  'carmichael-lambda': {
    category: 'number-theory',
    description: 'Calculates the Carmichael function (λ(n)) of a number, which is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to calculate the Carmichael function for.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(1)',
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(2)',
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(3)',
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(4)',
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(6)',
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(12)',
      'let { carmichael-lambda } = import(number-theory);\ncarmichael-lambda(30)',
    ],
    seeAlso: ['number-theory.euler-totient', 'number-theory.mod-exp', 'number-theory.coprime?'],
  },
  'cartesian-product': {
    category: 'number-theory',
    description: 'Calculates the Cartesian product of two or more sets.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      sets: {
        type: 'array',
        array: true,
        description: 'The input collections to calculate the Cartesian product from.',
      },
      a: {
        type: 'array',
      },
      b: {
        type: 'array',
      },
    },
    variants: [
      {
        argumentNames: [
          'sets',
        ],
      },
    ],
    examples: [
      'let { cartesian-product } = import(number-theory);\ncartesian-product([1, 2], ["a", "b"])',
      'let { cartesian-product } = import(number-theory);\ncartesian-product([1, 2], ["a", "b"], [true, false])',
      'let { cartesian-product } = import(number-theory);\ncartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
    seeAlso: ['number-theory.combinations', 'number-theory.power-set', 'number-theory.permutations'],
  },
  'perfect-power': {
    category: 'number-theory',
    description: 'Returns a tuple of the base and exponent if the number is a perfect power, otherwise returns null.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { perfect-power } = import(number-theory);\nperfect-power(1)',
      'let { perfect-power } = import(number-theory);\nperfect-power(2)',
      'let { perfect-power } = import(number-theory);\nperfect-power(4)',
      'let { perfect-power } = import(number-theory);\nperfect-power(8)',
      'let { perfect-power } = import(number-theory);\nperfect-power(9)',
      'let { perfect-power } = import(number-theory);\nperfect-power(16)',
      'let { perfect-power } = import(number-theory);\nperfect-power(19)',
    ],
    seeAlso: ['number-theory.perfect-power?', 'number-theory.perfect-power-seq', 'number-theory.perfect-square?', 'number-theory.perfect-cube?'],
  },
  'mod-exp': {
    category: 'number-theory',
    description: 'Calculates the modular exponentiation of a base raised to an exponent modulo a modulus.',
    returns: {
      type: 'integer',
    },
    args: {
      base: {
        type: 'integer',
      },
      exponent: {
        type: 'integer',
      },
      modulus: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'base',
          'exponent',
          'modulus',
        ],
      },
    ],
    examples: [
      'let { mod-exp } = import(number-theory);\nmod-exp(2, 3, 5)',
      'let { mod-exp } = import(number-theory);\nmod-exp(3, 4, 7)',
      'let { mod-exp } = import(number-theory);\nmod-exp(5, 6, 11)',
      'let { mod-exp } = import(number-theory);\nmod-exp(7, 8, 13)',
    ],
    seeAlso: ['number-theory.mod-inv', 'number-theory.carmichael-lambda', 'number-theory.chinese-remainder'],
  },
  'mod-inv': {
    category: 'number-theory',
    description: 'Calculates the modular multiplicative inverse of a number modulo another number.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'm',
        ],
      },
    ],
    examples: [
      'let { mod-inv } = import(number-theory);\nmod-inv(3, 11)',
      'let { mod-inv } = import(number-theory);\nmod-inv(10, 17)',
      'let { mod-inv } = import(number-theory);\nmod-inv(5, 13)',
      'let { mod-inv } = import(number-theory);\nmod-inv(7, 19)',
    ],
    seeAlso: ['number-theory.mod-exp', 'number-theory.extended-gcd', 'number-theory.chinese-remainder'],
  },
  'extended-gcd': {
    category: 'number-theory',
    description: 'Calculates the extended greatest common divisor (GCD) of two numbers, returning the GCD and the coefficients of Bézout\'s identity.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      a: {
        type: 'integer',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { extended-gcd } = import(number-theory);\nextended-gcd(30, 12)',
      'let { extended-gcd } = import(number-theory);\nextended-gcd(56, 98)',
      'let { extended-gcd } = import(number-theory);\nextended-gcd(101, 10)',
      'let { extended-gcd } = import(number-theory);\nextended-gcd(17, 13)',
    ],
    seeAlso: ['number-theory.gcd', 'number-theory.mod-inv', 'number-theory.chinese-remainder'],
  },
  'chinese-remainder': {
    category: 'number-theory',
    description: 'Solves a system of simultaneous congruences using the Chinese Remainder Theorem.',
    returns: {
      type: 'integer',
    },
    args: {
      remainders: {
        type: 'integer',
        array: true,
        description: 'The remainders of the congruences.',
      },
      moduli: {
        type: 'integer',
        array: true,
        description: 'The moduli of the congruences.',
      },
      a: {
        type: 'array',
      },
      b: {
        type: 'array',
      },
    },
    variants: [
      {
        argumentNames: [
          'remainders',
          'moduli',
        ],
      },
    ],
    examples: [
      'let { chinese-remainder } = import(number-theory);\nchinese-remainder([2, 3], [3, 5])',
      'let { chinese-remainder } = import(number-theory);\nchinese-remainder([1, 2], [3, 4])',
      'let { chinese-remainder } = import(number-theory);\nchinese-remainder([0, 1], [2, 3])',
      'let { chinese-remainder } = import(number-theory);\nchinese-remainder([1, 2, 3], [4, 5, 7])',
    ],
    seeAlso: ['number-theory.mod-exp', 'number-theory.mod-inv', 'number-theory.extended-gcd'],
  },
  'stirling-first': {
    category: 'number-theory',
    description: 'Calculates the Stirling numbers of the first kind, which count the number of permutations of n elements with k cycles.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
        description: 'The number of elements.',
      },
      b: {
        type: 'integer',
        description: 'The number of cycles.',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { stirling-first } = import(number-theory);\nstirling-first(5, 2)',
      'let { stirling-first } = import(number-theory);\nstirling-first(4, 3)',
      'let { stirling-first } = import(number-theory);\nstirling-first(6, 1)',
      'let { stirling-first } = import(number-theory);\nstirling-first(7, 4)',
      'let { stirling-first } = import(number-theory);\nstirling-first(8, 5)',
    ],
    seeAlso: ['number-theory.stirling-second', 'number-theory.bell-seq', 'number-theory.count-permutations'],
  },
  'stirling-second': {
    category: 'number-theory',
    description: 'Calculates the Stirling numbers of the second kind, which count the number of ways to partition n elements into k non-empty subsets.',
    returns: {
      type: 'integer',
    },
    args: {
      a: {
        type: 'integer',
        description: 'The number of elements.',
      },
      b: {
        type: 'integer',
        description: 'The number of subsets.',
      },
    },
    variants: [
      {
        argumentNames: [
          'a',
          'b',
        ],
      },
    ],
    examples: [
      'let { stirling-second } = import(number-theory);\nstirling-second(5, 2)',
      'let { stirling-second } = import(number-theory);\nstirling-second(4, 3)',
      'let { stirling-second } = import(number-theory);\nstirling-second(6, 1)',
      'let { stirling-second } = import(number-theory);\nstirling-second(7, 4)',
      'let { stirling-second } = import(number-theory);\nstirling-second(8, 5)',
    ],
    seeAlso: ['number-theory.stirling-first', 'number-theory.bell-seq', 'number-theory.count-combinations'],
  },
}
