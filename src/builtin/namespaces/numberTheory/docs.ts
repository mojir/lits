import type { FunctionDocs } from '../../interface'

export const namespaceDocs: Record<string, FunctionDocs> = {
  'abundant-seq': {
    category: 'Number Theory',
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
      'let { abundant-seq } = import("Number-Theory");\nabundant-seq(1)',
      'let { abundant-seq } = import("Number-Theory");\nabundant-seq(5)',
    ],
  },
  'abundant-take-while': {
    category: 'Number Theory',
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
      'let { abundant-take-while } = import("Number-Theory");\nabundant-take-while(-> $ < 100)',
    ],
  },
  'abundant-nth': {
    category: 'Number Theory',
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
      'let { abundant-nth } = import("Number-Theory");\nabundant-nth(1)',
      'let { abundant-nth } = import("Number-Theory");\nabundant-nth(5)',
    ],
  },
  'abundant?': {
    category: 'Number Theory',
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
      'let { abundant? } = import("Number-Theory");\nabundant?(12)',
      'let { abundant? } = import("Number-Theory");\nabundant?(15)',
    ],
  },
  'arithmetic-seq': {
    category: 'Number Theory',
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
      'let { arithmetic-seq } = import("Number-Theory");\narithmetic-seq(3, 2, 2)',
      'let { arithmetic-seq } = import("Number-Theory");\narithmetic-seq(2, 3, 2)',
      'let { arithmetic-seq } = import("Number-Theory");\narithmetic-seq(1, 2, 2)',
      'let { arithmetic-seq } = import("Number-Theory");\narithmetic-seq(1, 1.5, 12)',
    ],
  },
  'arithmetic-take-while': {
    category: 'Number Theory',
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
      'let { arithmetic-take-while } = import("Number-Theory");\narithmetic-take-while(1, 0.25, -> $ < 3)',
    ],
  },
  'arithmetic-nth': {
    category: 'Number Theory',
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
      'let { arithmetic-nth } = import("Number-Theory");\narithmetic-nth(3, 2, 2)',
      'let { arithmetic-nth } = import("Number-Theory");\narithmetic-nth(2, 3, 2)',
      'let { arithmetic-nth } = import("Number-Theory");\narithmetic-nth(1, 2, 2)',
      'let { arithmetic-nth } = import("Number-Theory");\narithmetic-nth(1, 1.5, 12)',
    ],
  },
  'arithmetic?': {
    category: 'Number Theory',
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
      'let { arithmetic? } = import("Number-Theory");\narithmetic?(3, 2, 2)',
      'let { arithmetic? } = import("Number-Theory");\narithmetic?(2, 3, 2)',
      'let { arithmetic? } = import("Number-Theory");\narithmetic?(1, 2, 2)',
      'let { arithmetic? } = import("Number-Theory");\narithmetic?(1, 1.5, 12)',
    ],
  },
  'bell-seq': {
    category: 'Number Theory',
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
      'let { bell-seq } = import("Number-Theory");\nbell-seq(5)',
      'let { bell-seq } = import("Number-Theory");\nbell-seq(10)',
      'let { bell-seq } = import("Number-Theory");\nbell-seq()',
    ],
  },
  'bell-take-while': {
    category: 'Number Theory',
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
      'let { bell-take-while } = import("Number-Theory");\nbell-take-while(-> $ < 1000)',
    ],
  },
  'bell-nth': {
    category: 'Number Theory',
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
      'let { bell-nth } = import("Number-Theory");\nbell-nth(5)',
      'let { bell-nth } = import("Number-Theory");\nbell-nth(10)',
    ],
  },
  'bell?': {
    category: 'Number Theory',
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
      'let { bell? } = import("Number-Theory");\nbell?(1)',
      'let { bell? } = import("Number-Theory");\nbell?(27644437)',
      'let { bell? } = import("Number-Theory");\nbell?(27644436)',
    ],
  },
  'bernoulli-seq': {
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
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { bernoulli-seq } = import("Number-Theory");\nbernoulli-seq(5)',
      'let { bernoulli-seq } = import("Number-Theory");\nbernoulli-seq(10)',
    ],
  },
  'bernoulli-take-while': {
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
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { bernoulli-take-while } = import("Number-Theory");\nbernoulli-take-while(-> abs($) < 100)',
    ],
  },
  'bernoulli-nth': {
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
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { bernoulli-nth } = import("Number-Theory");\nbernoulli-nth(5)',
      'let { bernoulli-nth } = import("Number-Theory");\nbernoulli-nth(10)',
      'let { bernoulli-nth } = import("Number-Theory");\nbernoulli-nth(23)',
    ],
  },
  'catalan-seq': {
    category: 'Number Theory',
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
      'let { catalan-seq } = import("Number-Theory");\ncatalan-seq(5)',
      'let { catalan-seq } = import("Number-Theory");\ncatalan-seq(10)',
      'let { catalan-seq } = import("Number-Theory");\ncatalan-seq()',
    ],
  },
  'catalan-take-while': {
    category: 'Number Theory',
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
      'let { catalan-take-while } = import("Number-Theory");\ncatalan-take-while(-> $ < 1000)',
    ],
  },
  'catalan-nth': {
    category: 'Number Theory',
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
      'let { catalan-nth } = import("Number-Theory");\ncatalan-nth(5)',
      'let { catalan-nth } = import("Number-Theory");\ncatalan-nth(10)',
    ],
  },
  'catalan?': {
    category: 'Number Theory',
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
      'let { catalan? } = import("Number-Theory");\ncatalan?(5)',
      'let { catalan? } = import("Number-Theory");\ncatalan?(10)',
    ],
  },
  'collatz-seq': {
    category: 'Number Theory',
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
      'let { collatz-seq } = import("Number-Theory");\ncollatz-seq(3)',
      'let { collatz-seq } = import("Number-Theory");\ncollatz-seq(11)',
    ],
  },
  'composite-seq': {
    category: 'Number Theory',
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
      'let { composite-seq } = import("Number-Theory");\ncomposite-seq(1)',
      'let { composite-seq } = import("Number-Theory");\ncomposite-seq(2)',
      'let { composite-seq } = import("Number-Theory");\ncomposite-seq(10)',
    ],
  },
  'composite-take-while': {
    category: 'Number Theory',
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
      'let { composite-take-while } = import("Number-Theory");\ncomposite-take-while(-> $ < 50)',
    ],
  },
  'composite-nth': {
    category: 'Number Theory',
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
      'let { composite-nth } = import("Number-Theory");\ncomposite-nth(1)',
      'let { composite-nth } = import("Number-Theory");\ncomposite-nth(2)',
      'let { composite-nth } = import("Number-Theory");\ncomposite-nth(10)',
    ],
  },
  'composite?': {
    category: 'Number Theory',
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
      'let { composite? } = import("Number-Theory");\ncomposite?(4)',
      'let { composite? } = import("Number-Theory");\ncomposite?(5)',
      'let { composite? } = import("Number-Theory");\ncomposite?(11)',
    ],
  },
  'deficient-seq': {
    category: 'Number Theory',
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
      'let { deficient-seq } = import("Number-Theory");\ndeficient-seq(1)',
      'let { deficient-seq } = import("Number-Theory");\ndeficient-seq(5)',
    ],
  },
  'deficient-take-while': {
    category: 'Number Theory',
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
      'let { deficient-take-while } = import("Number-Theory");\ndeficient-take-while(-> $ < 100)',
    ],
  },
  'deficient-nth': {
    category: 'Number Theory',
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
      'let { deficient-nth } = import("Number-Theory");\ndeficient-nth(5)',
      'let { deficient-nth } = import("Number-Theory");\ndeficient-nth(12)',
    ],
  },
  'deficient?': {
    category: 'Number Theory',
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
      'let { deficient? } = import("Number-Theory");\ndeficient?(12)',
      'let { deficient? } = import("Number-Theory");\ndeficient?(15)',
    ],
  },
  'factorial-seq': {
    category: 'Number Theory',
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
      'let { factorial-seq } = import("Number-Theory");\nfactorial-seq(1)',
      'let { factorial-seq } = import("Number-Theory");\nfactorial-seq(2)',
      'let { factorial-seq } = import("Number-Theory");\nfactorial-seq(3)',
      'let { factorial-seq } = import("Number-Theory");\nfactorial-seq(4)',
      'let { factorial-seq } = import("Number-Theory");\nfactorial-seq(5)',
      'let { factorial-seq } = import("Number-Theory");\nfactorial-seq(10)',
    ],
  },
  'factorial-take-while': {
    category: 'Number Theory',
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
      'let { factorial-take-while } = import("Number-Theory");\nfactorial-take-while(-> $ < 1000)',
    ],
  },
  'factorial-nth': {
    category: 'Number Theory',
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
      'let { factorial-nth } = import("Number-Theory");\nfactorial-nth(1)',
      'let { factorial-nth } = import("Number-Theory");\nfactorial-nth(2)',
      'let { factorial-nth } = import("Number-Theory");\nfactorial-nth(3)',
      'let { factorial-nth } = import("Number-Theory");\nfactorial-nth(4)',
      'let { factorial-nth } = import("Number-Theory");\nfactorial-nth(5)',
      'let { factorial-nth } = import("Number-Theory");\nfactorial-nth(10)',
    ],
  },
  'factorial?': {
    category: 'Number Theory',
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
      'let { factorial? } = import("Number-Theory");\nfactorial?(1)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(2)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(3)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(4)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(5)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(6)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(7)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(8)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(9)',
      'let { factorial? } = import("Number-Theory");\nfactorial?(3628800)',
    ],
  },
  'fibonacci-seq': {
    category: 'Number Theory',
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
      'let { fibonacci-seq } = import("Number-Theory");\nfibonacci-seq(1)',
      'let { fibonacci-seq } = import("Number-Theory");\nfibonacci-seq(2)',
      'let { fibonacci-seq } = import("Number-Theory");\nfibonacci-seq()',
    ],
  },
  'fibonacci-take-while': {
    category: 'Number Theory',
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
      'let { fibonacci-take-while } = import("Number-Theory");\nfibonacci-take-while(-> $ < 100)',
    ],
  },
  'fibonacci-nth': {
    category: 'Number Theory',
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
      'let { fibonacci-nth } = import("Number-Theory");\nfibonacci-nth(5)',
      'let { fibonacci-nth } = import("Number-Theory");\nfibonacci-nth(50)',
    ],
  },
  'fibonacci?': {
    category: 'Number Theory',
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
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(0)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(1)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(2)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(3)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(4)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(5)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(6)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(7)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(8)',
      'let { fibonacci? } = import("Number-Theory");\nfibonacci?(9)',
    ],
  },
  'geometric-seq': {
    category: 'Number Theory',
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
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(3, 2, 2)',
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(2, 3, 2)',
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(1, 2, 2)',
      'let { geometric-seq } = import("Number-Theory");\ngeometric-seq(1, 1.5, 12)',
    ],
  },
  'geometric-take-while': {
    category: 'Number Theory',
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
      'let { geometric-take-while } = import("Number-Theory");\ngeometric-take-while(1, 1.5, -> $ < 10)',
    ],
  },
  'geometric-nth': {
    category: 'Number Theory',
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
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(3, 2, 2)',
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(2, 3, 2)',
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(1, 2, 2)',
      'let { geometric-nth } = import("Number-Theory");\ngeometric-nth(1, 1.5, 4)',
    ],
  },
  'geometric?': {
    category: 'Number Theory',
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
      'let { geometric? } = import("Number-Theory");\ngeometric?(1, 2, 1)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(2, 3, 2)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(3, 2, 2)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(1, 1.5, 2.25)',
      'let { geometric? } = import("Number-Theory");\ngeometric?(1, 1.5, -4)',
    ],
  },
  'golomb-seq': {
    category: 'Number Theory',
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
      'let { golomb-seq } = import("Number-Theory");\ngolomb-seq(5)',
      'let { golomb-seq } = import("Number-Theory");\ngolomb-seq(20)',
    ],
  },
  'golomb-take-while': {
    category: 'Number Theory',
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
      'let { golomb-take-while } = import("Number-Theory");\ngolomb-take-while(-> $ <= 10)',
    ],
  },
  'golomb-nth': {
    category: 'Number Theory',
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
      'let { golomb-nth } = import("Number-Theory");\ngolomb-nth(5)',
      'let { golomb-nth } = import("Number-Theory");\ngolomb-nth(1000)',
    ],
  },
  'golomb?': {
    category: 'Number Theory',
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
      'let { golomb? } = import("Number-Theory");\ngolomb?(1)',
      'let { golomb? } = import("Number-Theory");\ngolomb?(2)',
      'let { golomb? } = import("Number-Theory");\ngolomb?(3345)',
      'let { golomb? } = import("Number-Theory");\ngolomb?(67867864)',
    ],
  },
  'happy-seq': {
    category: 'Number Theory',
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
      'let { happy-seq } = import("Number-Theory");\nhappy-seq(1)',
      'let { happy-seq } = import("Number-Theory");\nhappy-seq(2)',
      'let { happy-seq } = import("Number-Theory");\nhappy-seq(20)',
    ],
  },
  'happy-take-while': {
    category: 'Number Theory',
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
      'let { happy-take-while } = import("Number-Theory");\nhappy-take-while(-> $ < 100)',
    ],
  },
  'happy-nth': {
    category: 'Number Theory',
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
      'let { happy-nth } = import("Number-Theory");\nhappy-nth(1)',
      'let { happy-nth } = import("Number-Theory");\nhappy-nth(2)',
      'let { happy-nth } = import("Number-Theory");\nhappy-nth(20)',
    ],
  },
  'happy?': {
    category: 'Number Theory',
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
      'let { happy? } = import("Number-Theory");\nhappy?(1)',
      'let { happy? } = import("Number-Theory");\nhappy?(2)',
      'let { happy? } = import("Number-Theory");\nhappy?(100)',
    ],
  },
  'juggler-seq': {
    category: 'Number Theory',
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
      'let { juggler-seq } = import("Number-Theory");\njuggler-seq(3)',
      'let { juggler-seq } = import("Number-Theory");\njuggler-seq(5)',
    ],
  },
  'look-and-say-seq': {
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
      {
        argumentNames: [
          'length',
        ],
      },
    ],
    examples: [
      'let { look-and-say-seq } = import("Number-Theory");\nlook-and-say-seq(5)',
    ],
  },
  'look-and-say-take-while': {
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
      {
        argumentNames: [
          'takeWhile',
        ],
      },
    ],
    examples: [
      'let { look-and-say-take-while } = import("Number-Theory");\nlook-and-say-take-while((term, index) -> count(term) < 10)',
      'let { look-and-say-take-while } = import("Number-Theory");\nlook-and-say-take-while(-> $2 <= 10)',
    ],
  },
  'look-and-say-nth': {
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
      {
        argumentNames: [
          'n',
        ],
      },
    ],
    examples: [
      'let { look-and-say-nth } = import("Number-Theory");\nlook-and-say-nth(5)',
    ],
  },
  'look-and-say?': {
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
      {
        argumentNames: [
          'term',
        ],
      },
    ],
    examples: [
      'let { look-and-say? } = import("Number-Theory");\nlook-and-say?("111221")',
      'let { look-and-say? } = import("Number-Theory");\nlook-and-say?("123")',
    ],
  },
  'lucas-seq': {
    category: 'Number Theory',
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
      'let { lucas-seq } = import("Number-Theory");\nlucas-seq(1)',
      'let { lucas-seq } = import("Number-Theory");\nlucas-seq(2)',
      'let { lucas-seq } = import("Number-Theory");\nlucas-seq()',
    ],
  },
  'lucas-take-while': {
    category: 'Number Theory',
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
      'let { lucas-take-while } = import("Number-Theory");\nlucas-take-while(-> $ < 100)',
    ],
  },
  'lucas-nth': {
    category: 'Number Theory',
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
      'let { lucas-nth } = import("Number-Theory");\nlucas-nth(1)',
      'let { lucas-nth } = import("Number-Theory");\nlucas-nth(2)',
      'let { lucas-nth } = import("Number-Theory");\nlucas-nth(10)',
    ],
  },
  'lucas?': {
    category: 'Number Theory',
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
      'let { lucas? } = import("Number-Theory");\nlucas?(1)',
      'let { lucas? } = import("Number-Theory");\nlucas?(2)',
      'let { lucas? } = import("Number-Theory");\nlucas?(10)',
    ],
  },
  'lucky-seq': {
    category: 'Number Theory',
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
      'let { lucky-seq } = import("Number-Theory");\nlucky-seq(1)',
      'let { lucky-seq } = import("Number-Theory");\nlucky-seq(2)',
      'let { lucky-seq } = import("Number-Theory");\nlucky-seq(20)',
    ],
  },
  'lucky-take-while': {
    category: 'Number Theory',
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
      'let { lucky-take-while } = import("Number-Theory");\nlucky-take-while(-> $ < 100)',
    ],
  },
  'lucky-nth': {
    category: 'Number Theory',
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
      'let { lucky-nth } = import("Number-Theory");\nlucky-nth(1)',
      'let { lucky-nth } = import("Number-Theory");\nlucky-nth(2)',
      'let { lucky-nth } = import("Number-Theory");\nlucky-nth(20)',
    ],
  },
  'lucky?': {
    category: 'Number Theory',
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
      'let { lucky? } = import("Number-Theory");\nlucky?(4)',
      'let { lucky? } = import("Number-Theory");\nlucky?(7)',
      'let { lucky? } = import("Number-Theory");\nlucky?(33)',
    ],
  },
  'mersenne-seq': {
    category: 'Number Theory',
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
      'let { mersenne-seq } = import("Number-Theory");\nmersenne-seq(1)',
      'let { mersenne-seq } = import("Number-Theory");\nmersenne-seq(5)',
      'let { mersenne-seq } = import("Number-Theory");\nmersenne-seq()',
    ],
  },
  'mersenne-take-while': {
    category: 'Number Theory',
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
      'let { mersenne-take-while } = import("Number-Theory");\nmersenne-take-while(-> $ < 1000)',
    ],
  },
  'mersenne-nth': {
    category: 'Number Theory',
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
      'let { mersenne-nth } = import("Number-Theory");\nmersenne-nth(1)',
      'let { mersenne-nth } = import("Number-Theory");\nmersenne-nth(5)',
    ],
  },
  'mersenne?': {
    category: 'Number Theory',
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
      'let { mersenne? } = import("Number-Theory");\nmersenne?(3)',
      'let { mersenne? } = import("Number-Theory");\nmersenne?(4)',
      'let { mersenne? } = import("Number-Theory");\nmersenne?(7)',
    ],
  },
  'padovan-seq': {
    category: 'Number Theory',
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
      'let { padovan-seq } = import("Number-Theory");\npadovan-seq(5)',
      'let { padovan-seq } = import("Number-Theory");\npadovan-seq(10)',
      'let { padovan-seq } = import("Number-Theory");\npadovan-seq(20)',
    ],
  },
  'padovan-take-while': {
    category: 'Number Theory',
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
      'let { padovan-take-while } = import("Number-Theory");\npadovan-take-while(-> $ < 1000)',
    ],
  },
  'padovan-nth': {
    category: 'Number Theory',
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
      'let { padovan-nth } = import("Number-Theory");\npadovan-nth(5)',
      'let { padovan-nth } = import("Number-Theory");\npadovan-nth(10)',
      'let { padovan-nth } = import("Number-Theory");\npadovan-nth(20)',
    ],
  },
  'padovan?': {
    category: 'Number Theory',
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
      'let { padovan? } = import("Number-Theory");\npadovan?(1)',
      'let { padovan? } = import("Number-Theory");\npadovan?(265)',
      'let { padovan? } = import("Number-Theory");\npadovan?(6)',
    ],
  },
  'partition-seq': {
    category: 'Number Theory',
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
      'let { partition-seq } = import("Number-Theory");\npartition-seq(1)',
      'let { partition-seq } = import("Number-Theory");\npartition-seq(10)',
      'let { partition-seq } = import("Number-Theory");\npartition-seq()',
    ],
  },
  'partition-take-while': {
    category: 'Number Theory',
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
      'let { partition-take-while } = import("Number-Theory");\npartition-take-while(-> $ < 1000)',
    ],
  },
  'partition-nth': {
    category: 'Number Theory',
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
      'let { partition-nth } = import("Number-Theory");\npartition-nth(1)',
      'let { partition-nth } = import("Number-Theory");\npartition-nth(5)',
    ],
  },
  'partition?': {
    category: 'Number Theory',
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
      'let { partition? } = import("Number-Theory");\npartition?(0)',
      'let { partition? } = import("Number-Theory");\npartition?(1)',
      'let { partition? } = import("Number-Theory");\npartition?(2)',
      'let { partition? } = import("Number-Theory");\npartition?(3)',
      'let { partition? } = import("Number-Theory");\npartition?(4)',
      'let { partition? } = import("Number-Theory");\npartition?(5)',
    ],
  },
  'pell-seq': {
    category: 'Number Theory',
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
      'let { pell-seq } = import("Number-Theory");\npell-seq(5)',
      'let { pell-seq } = import("Number-Theory");\npell-seq(10)',
      'let { pell-seq } = import("Number-Theory");\npell-seq()',
    ],
  },
  'pell-take-while': {
    category: 'Number Theory',
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
      'let { pell-take-while } = import("Number-Theory");\npell-take-while(-> $ < 1000)',
    ],
  },
  'pell-nth': {
    category: 'Number Theory',
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
      'let { pell-nth } = import("Number-Theory");\npell-nth(5)',
      'let { pell-nth } = import("Number-Theory");\npell-nth(10)',
      'let { pell-nth } = import("Number-Theory");\npell-nth(20)',
    ],
  },
  'pell?': {
    category: 'Number Theory',
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
      'let { pell? } = import("Number-Theory");\npell?(1)',
      'let { pell? } = import("Number-Theory");\npell?(470832)',
      'let { pell? } = import("Number-Theory");\npell?(10)',
    ],
  },
  'perfect-seq': {
    category: 'Number Theory',
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
      'let { perfect-seq } = import("Number-Theory");\nperfect-seq(1)',
      'let { perfect-seq } = import("Number-Theory");\nperfect-seq(5)',
      'let { perfect-seq } = import("Number-Theory");\nperfect-seq()',
    ],
  },
  'perfect-take-while': {
    category: 'Number Theory',
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
      'let { perfect-take-while } = import("Number-Theory");\nperfect-take-while(-> $ < 1000)',
    ],
  },
  'perfect-nth': {
    category: 'Number Theory',
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
      'let { perfect-nth } = import("Number-Theory");\nperfect-nth(1)',
      'let { perfect-nth } = import("Number-Theory");\nperfect-nth(5)',
    ],
  },
  'perfect?': {
    category: 'Number Theory',
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
      'let { perfect? } = import("Number-Theory");\nperfect?(0)',
      'let { perfect? } = import("Number-Theory");\nperfect?(1)',
      'let { perfect? } = import("Number-Theory");\nperfect?(2)',
      'let { perfect? } = import("Number-Theory");\nperfect?(3)',
      'let { perfect? } = import("Number-Theory");\nperfect?(4)',
      'let { perfect? } = import("Number-Theory");\nperfect?(5)',
      'let { perfect? } = import("Number-Theory");\nperfect?(6)',
      'let { perfect? } = import("Number-Theory");\nperfect?(7)',
      'let { perfect? } = import("Number-Theory");\nperfect?(8)',
      'let { perfect? } = import("Number-Theory");\nperfect?(9)',
    ],
  },
  'perfect-square-seq': {
    category: 'Number Theory',
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
      'let { perfect-square-seq } = import("Number-Theory");\nperfect-square-seq(5)',
      'let { perfect-square-seq } = import("Number-Theory");\nperfect-square-seq(20)',
    ],
  },
  'perfect-square-take-while': {
    category: 'Number Theory',
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
      'let { perfect-square-take-while } = import("Number-Theory");\nperfect-square-take-while(-> $ <= 100)',
    ],
  },
  'perfect-square-nth': {
    category: 'Number Theory',
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
      'let { perfect-square-nth } = import("Number-Theory");\nperfect-square-nth(1)',
      'let { perfect-square-nth } = import("Number-Theory");\nperfect-square-nth(5)',
    ],
  },
  'perfect-square?': {
    category: 'Number Theory',
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
      'let { perfect-square? } = import("Number-Theory");\nperfect-square?(16)',
      'let { perfect-square? } = import("Number-Theory");\nperfect-square?(20)',
    ],
  },
  'perfect-cube-seq': {
    category: 'Number Theory',
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
      'let { perfect-cube-seq } = import("Number-Theory");\nperfect-cube-seq(5)',
      'let { perfect-cube-seq } = import("Number-Theory");\nperfect-cube-seq(20)',
    ],
  },
  'perfect-cube-take-while': {
    category: 'Number Theory',
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
      'let { perfect-cube-take-while } = import("Number-Theory");\nperfect-cube-take-while(-> $ <= 100)',
    ],
  },
  'perfect-cube-nth': {
    category: 'Number Theory',
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
      'let { perfect-cube-nth } = import("Number-Theory");\nperfect-cube-nth(1)',
      'let { perfect-cube-nth } = import("Number-Theory");\nperfect-cube-nth(5)',
    ],
  },
  'perfect-cube?': {
    category: 'Number Theory',
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
      'let { perfect-cube? } = import("Number-Theory");\nperfect-cube?(7)',
      'let { perfect-cube? } = import("Number-Theory");\nperfect-cube?(8)',
      'let { perfect-cube? } = import("Number-Theory");\nperfect-cube?(9)',
    ],
  },
  'perfect-power-seq': {
    category: 'Number Theory',
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
      'let { perfect-power-seq } = import("Number-Theory");\nperfect-power-seq(5)',
      'let { perfect-power-seq } = import("Number-Theory");\nperfect-power-seq(20)',
    ],
  },
  'perfect-power-take-while': {
    category: 'Number Theory',
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
      'let { perfect-power-take-while } = import("Number-Theory");\nperfect-power-take-while(-> $ <= 100)',
    ],
  },
  'perfect-power-nth': {
    category: 'Number Theory',
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
      'let { perfect-power-nth } = import("Number-Theory");\nperfect-power-nth(3)',
      'let { perfect-power-nth } = import("Number-Theory");\nperfect-power-nth(15)',
    ],
  },
  'perfect-power?': {
    category: 'Number Theory',
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
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(7)',
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(8)',
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(9)',
      'let { perfect-power? } = import("Number-Theory");\nperfect-power?(10)',
    ],
  },
  'polygonal-seq': {
    category: 'Number Theory',
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
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(3, 2)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(4, 2)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(5, 3)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(6, 5)',
      'let { polygonal-seq } = import("Number-Theory");\npolygonal-seq(100, 10)',
    ],
  },
  'polygonal-take-while': {
    category: 'Number Theory',
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
      'let { polygonal-take-while } = import("Number-Theory");\npolygonal-take-while(15, -> $ < 1000)',
    ],
  },
  'polygonal-nth': {
    category: 'Number Theory',
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
      'let { polygonal-nth } = import("Number-Theory");\npolygonal-nth(3, 9)',
      'let { polygonal-nth } = import("Number-Theory");\npolygonal-nth(4, 5)',
      'let { polygonal-nth } = import("Number-Theory");\npolygonal-nth(5, 5)',
    ],
  },
  'polygonal?': {
    category: 'Number Theory',
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
      'let { polygonal? } = import("Number-Theory");\npolygonal?(3, 10)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(3, 9)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(4, 10000)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(4, 1000)',
      'let { polygonal? } = import("Number-Theory");\npolygonal?(6, 45)',
    ],
  },
  'prime-seq': {
    category: 'Number Theory',
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
      'let { prime-seq } = import("Number-Theory");\nprime-seq(1)',
      'let { prime-seq } = import("Number-Theory");\nprime-seq(2)',
      'let { prime-seq } = import("Number-Theory");\nprime-seq(10)',
    ],
  },
  'prime-take-while': {
    category: 'Number Theory',
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
      'let { prime-take-while } = import("Number-Theory");\nprime-take-while(-> $ < 50)',
    ],
  },
  'prime-nth': {
    category: 'Number Theory',
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
      'let { prime-nth } = import("Number-Theory");\nprime-nth(1)',
      'let { prime-nth } = import("Number-Theory");\nprime-nth(2)',
      'let { prime-nth } = import("Number-Theory");\nprime-nth(10)',
    ],
  },
  'prime?': {
    category: 'Number Theory',
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
      'let { prime? } = import("Number-Theory");\nprime?(1)',
      'let { prime? } = import("Number-Theory");\nprime?(2)',
      'let { prime? } = import("Number-Theory");\nprime?(3)',
      'let { prime? } = import("Number-Theory");\nprime?(4)',
      'let { prime? } = import("Number-Theory");\nprime?(997)',
      'let { prime? } = import("Number-Theory");\nprime?(1001)',
    ],
  },
  'recaman-seq': {
    category: 'Number Theory',
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
      'let { recaman-seq } = import("Number-Theory");\nrecaman-seq(5)',
      'let { recaman-seq } = import("Number-Theory");\nrecaman-seq(10)',
      'let { recaman-seq } = import("Number-Theory");\nrecaman-seq(20)',
    ],
  },
  'recaman-take-while': {
    category: 'Number Theory',
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
      'let { recaman-take-while } = import("Number-Theory");\nrecaman-take-while(-> $ < 10)',
    ],
  },
  'recaman-nth': {
    category: 'Number Theory',
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
      'let { recaman-nth } = import("Number-Theory");\nrecaman-nth(5)',
      'let { recaman-nth } = import("Number-Theory");\nrecaman-nth(10)',
      'let { recaman-nth } = import("Number-Theory");\nrecaman-nth(20)',
    ],
  },
  'recaman?': {
    category: 'Number Theory',
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
      'let { recaman? } = import("Number-Theory");\nrecaman?(5)',
      'let { recaman? } = import("Number-Theory");\nrecaman?(10)',
      'let { recaman? } = import("Number-Theory");\nrecaman?(20)',
    ],
  },
  'sylvester-seq': {
    category: 'Number Theory',
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
      'let { sylvester-seq } = import("Number-Theory");\nsylvester-seq(5)',
      'let { sylvester-seq } = import("Number-Theory");\nsylvester-seq()',
      'let { sylvester-seq } = import("Number-Theory");\nsylvester-seq()',
    ],
  },
  'sylvester-take-while': {
    category: 'Number Theory',
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
      'let { sylvester-take-while } = import("Number-Theory");\nsylvester-take-while(-> $ < 100000)',
    ],
  },
  'sylvester-nth': {
    category: 'Number Theory',
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
      'let { sylvester-nth } = import("Number-Theory");\nsylvester-nth(1)',
      'let { sylvester-nth } = import("Number-Theory");\nsylvester-nth(5)',
    ],
  },
  'sylvester?': {
    category: 'Number Theory',
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
      'let { sylvester? } = import("Number-Theory");\nsylvester?(2)',
      'let { sylvester? } = import("Number-Theory");\nsylvester?(3)',
      'let { sylvester? } = import("Number-Theory");\nsylvester?(6)',
    ],
  },
  'thue-morse-seq': {
    category: 'Number Theory',
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
      'let { thue-morse-seq } = import("Number-Theory");\nthue-morse-seq(5)',
      'let { thue-morse-seq } = import("Number-Theory");\nthue-morse-seq(10)',
      'let { thue-morse-seq } = import("Number-Theory");\nthue-morse-seq(20)',
    ],
  },
  'thue-morse-take-while': {
    category: 'Number Theory',
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
      'let { thue-morse-take-while } = import("Number-Theory");\nthue-morse-take-while(-> $2 < 10)',
    ],
  },
  'thue-morse-nth': {
    category: 'Number Theory',
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
      'let { thue-morse-nth } = import("Number-Theory");\nthue-morse-nth(5)',
      'let { thue-morse-nth } = import("Number-Theory");\nthue-morse-nth(10)',
      'let { thue-morse-nth } = import("Number-Theory");\nthue-morse-nth(20)',
    ],
  },
  'thue-morse?': {
    category: 'Number Theory',
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
      'let { thue-morse? } = import("Number-Theory");\nthue-morse?(1)',
      'let { thue-morse? } = import("Number-Theory");\nthue-morse?(2)',
    ],
  },
  'tribonacci-seq': {
    category: 'Number Theory',
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
      'let { tribonacci-seq } = import("Number-Theory");\ntribonacci-seq(1)',
      'let { tribonacci-seq } = import("Number-Theory");\ntribonacci-seq(2)',
      'let { tribonacci-seq } = import("Number-Theory");\ntribonacci-seq(10)',
    ],
  },
  'tribonacci-take-while': {
    category: 'Number Theory',
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
      'let { tribonacci-take-while } = import("Number-Theory");\ntribonacci-take-while(-> $ < 100)',
    ],
  },
  'tribonacci-nth': {
    category: 'Number Theory',
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
      'let { tribonacci-nth } = import("Number-Theory");\ntribonacci-nth(1)',
      'let { tribonacci-nth } = import("Number-Theory");\ntribonacci-nth(2)',
      'let { tribonacci-nth } = import("Number-Theory");\ntribonacci-nth(10)',
    ],
  },
  'tribonacci?': {
    category: 'Number Theory',
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
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(0)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(1)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(2)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(3)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(4)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(5)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(6)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(7)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(8)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(9)',
      'let { tribonacci? } = import("Number-Theory");\ntribonacci?(10)',
    ],
  },
  'count-combinations': {
    category: 'Number Theory',
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
      'let { count-combinations } = import("Number-Theory");\ncount-combinations(5, 3)',
      'let { count-combinations } = import("Number-Theory");\ncount-combinations(10, 2)',
    ],
  },
  'combinations': {
    category: 'Number Theory',
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
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 2)',
      'let { combinations } = import("Number-Theory");\ncombinations(["a", "b", "c"], 2)',
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 0)',
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 1)',
      'let { combinations } = import("Number-Theory");\ncombinations([1, 2, 3], 3)',
    ],
  },
  'count-derangements': {
    category: 'Number Theory',
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
      'let { count-derangements } = import("Number-Theory");\ncount-derangements(4)',
      'let { count-derangements } = import("Number-Theory");\ncount-derangements(5)',
    ],
  },
  'derangements': {
    category: 'Number Theory',
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
      'let { derangements } = import("Number-Theory");\nderangements([1, 2, 3, 4])',
      'let { derangements } = import("Number-Theory");\nderangements(["a", "b", "c"])',
    ],
  },
  'divisors': {
    category: 'Number Theory',
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
      'let { divisors } = import("Number-Theory");\ndivisors(12)',
      'let { divisors } = import("Number-Theory");\ndivisors(100)',
      'let { divisors } = import("Number-Theory");\ndivisors(37)',
    ],
  },
  'count-divisors': {
    category: 'Number Theory',
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
      'let { count-divisors } = import("Number-Theory");\ncount-divisors(12)',
      'let { count-divisors } = import("Number-Theory");\ncount-divisors(100)',
      'let { count-divisors } = import("Number-Theory");\ncount-divisors(37)',
    ],
  },
  'proper-divisors': {
    category: 'Number Theory',
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
      'let { proper-divisors } = import("Number-Theory");\nproper-divisors(12)',
      'let { proper-divisors } = import("Number-Theory");\nproper-divisors(100)',
      'let { proper-divisors } = import("Number-Theory");\nproper-divisors(37)',
    ],
  },
  'count-proper-divisors': {
    category: 'Number Theory',
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
      'let { count-proper-divisors } = import("Number-Theory");\ncount-proper-divisors(12)',
      'let { count-proper-divisors } = import("Number-Theory");\ncount-proper-divisors(100)',
      'let { count-proper-divisors } = import("Number-Theory");\ncount-proper-divisors(37)',
    ],
  },
  'factorial': {
    category: 'Number Theory',
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
      'let { factorial } = import("Number-Theory");\nfactorial(5)',
      'let { factorial } = import("Number-Theory");\nfactorial(0)',
      'let { factorial } = import("Number-Theory");\nfactorial(10)',
      'let { factorial } = import("Number-Theory");\nfactorial(20)',
    ],
  },
  'partitions': {
    category: 'Number Theory',
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
      'let { partitions } = import("Number-Theory");\npartitions(4)',
      'let { partitions } = import("Number-Theory");\npartitions(8)',
    ],
  },
  'count-partitions': {
    category: 'Number Theory',
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
      'let { count-partitions } = import("Number-Theory");\ncount-partitions(4)',
      'let { count-partitions } = import("Number-Theory");\ncount-partitions(8)',
      'let { count-partitions } = import("Number-Theory");\ncount-partitions(15)',
    ],
  },
  'permutations': {
    category: 'Number Theory',
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
      'let { permutations } = import("Number-Theory");\npermutations([1, 2, 3])',
      'let { permutations } = import("Number-Theory");\npermutations(["a", "b", "c"])',
      'let { permutations } = import("Number-Theory");\npermutations([1, 2, 3, 4])',
      'let { permutations } = import("Number-Theory");\npermutations([1, 2])',
      'let { permutations } = import("Number-Theory");\npermutations([1])',
      'let { permutations } = import("Number-Theory");\npermutations([])',
    ],
  },
  'count-permutations': {
    category: 'Number Theory',
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
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(5, 3)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 2)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 10)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 0)',
      'let { count-permutations } = import("Number-Theory");\ncount-permutations(10, 1)',
    ],
  },
  'power-set': {
    category: 'Number Theory',
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
      'let { power-set } = import("Number-Theory");\npower-set(["a", "b", "c"])',
      'let { power-set } = import("Number-Theory");\npower-set([1, 2])',
      'let { power-set } = import("Number-Theory");\npower-set([1])',
      'let { power-set } = import("Number-Theory");\npower-set([])',
    ],
  },
  'count-power-set': {
    category: 'Number Theory',
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
      'let { count-power-set } = import("Number-Theory");\ncount-power-set(3)',
      'let { count-power-set } = import("Number-Theory");\ncount-power-set(5)',
      'let { count-power-set } = import("Number-Theory");\ncount-power-set(10)',
    ],
  },
  'prime-factors': {
    category: 'Number Theory',
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
      'let { prime-factors } = import("Number-Theory");\nprime-factors(12)',
      'let { prime-factors } = import("Number-Theory");\nprime-factors(100)',
      'let { prime-factors } = import("Number-Theory");\nprime-factors(37)',
    ],
  },
  'count-prime-factors': {
    category: 'Number Theory',
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
      'let { count-prime-factors } = import("Number-Theory");\ncount-prime-factors(12)',
      'let { count-prime-factors } = import("Number-Theory");\ncount-prime-factors(100)',
      'let { count-prime-factors } = import("Number-Theory");\ncount-prime-factors(37)',
    ],
  },
  'distinct-prime-factors': {
    category: 'Number Theory',
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
      'let { distinct-prime-factors } = import("Number-Theory");\ndistinct-prime-factors(12)',
      'let { distinct-prime-factors } = import("Number-Theory");\ndistinct-prime-factors(100)',
      'let { distinct-prime-factors } = import("Number-Theory");\ndistinct-prime-factors(37)',
    ],
  },
  'count-distinct-prime-factors': {
    category: 'Number Theory',
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
      'let { count-distinct-prime-factors } = import("Number-Theory");\ncount-distinct-prime-factors(12)',
      'let { count-distinct-prime-factors } = import("Number-Theory");\ncount-distinct-prime-factors(100)',
      'let { count-distinct-prime-factors } = import("Number-Theory");\ncount-distinct-prime-factors(37)',
    ],
  },
  'coprime?': {
    category: 'Number Theory',
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
      'let { coprime? } = import("Number-Theory");\ncoprime?(12, 8)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(12, 5)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(37, 1)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(0, 0)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(0, 5)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(5, 0)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(1, 0)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(0, 1)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(1, 1)',
      'let { coprime? } = import("Number-Theory");\ncoprime?(2, 3)',
    ],
  },
  'divisible-by?': {
    category: 'Number Theory',
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
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(12, 4)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(12, 5)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(37, 1)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(0, 0)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(0, 5)',
      'let { divisible-by? } = import("Number-Theory");\ndivisible-by?(5, 0)',
    ],
  },
  'gcd': {
    category: 'Number Theory',
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
      'let { gcd } = import("Number-Theory");\ngcd(100, 25)',
      'let { gcd } = import("Number-Theory");\ngcd(37, 1)',
      'let { gcd } = import("Number-Theory");\ngcd(0, 0)',
      'let { gcd } = import("Number-Theory");\ngcd(0, 5)',
      'let { gcd } = import("Number-Theory");\ngcd(5, 0)',
    ],
  },
  'lcm': {
    category: 'Number Theory',
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
      'let { lcm } = import("Number-Theory");\nlcm(100, 25)',
      'let { lcm } = import("Number-Theory");\nlcm(37, 1)',
      'let { lcm } = import("Number-Theory");\nlcm(0, 5)',
      'let { lcm } = import("Number-Theory");\nlcm(5, 0)',
    ],
  },
  'multinomial': {
    category: 'Number Theory',
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
      'let { multinomial } = import("Number-Theory");\nmultinomial(5, 2, 3)',
      'let { multinomial } = import("Number-Theory");\nmultinomial(10, 2, 3, 5)',
    ],
    hideOperatorForm: true,
  },
  'amicable?': {
    category: 'Number Theory',
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
      'let { amicable? } = import("Number-Theory");\namicable?(220, 284)',
      'let { amicable? } = import("Number-Theory");\namicable?(1184, 1210)',
      'let { amicable? } = import("Number-Theory");\namicable?(2620, 2924)',
      'let { amicable? } = import("Number-Theory");\namicable?(5020, 5564)',
      'let { amicable? } = import("Number-Theory");\namicable?(6232, 6368)',
    ],
  },
  'euler-totient': {
    category: 'Number Theory',
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
      'let { euler-totient } = import("Number-Theory");\neuler-totient(1)',
      'let { euler-totient } = import("Number-Theory");\neuler-totient(2)',
      'let { euler-totient } = import("Number-Theory");\neuler-totient(10)',
      'let { euler-totient } = import("Number-Theory");\neuler-totient(20)',
    ],
  },
  'mobius': {
    category: 'Number Theory',
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
      'let { mobius } = import("Number-Theory");\nmobius(1)',
      'let { mobius } = import("Number-Theory");\nmobius(2)',
      'let { mobius } = import("Number-Theory");\nmobius(3)',
      'let { mobius } = import("Number-Theory");\nmobius(4)',
      'let { mobius } = import("Number-Theory");\nmobius(6)',
      'let { mobius } = import("Number-Theory");\nmobius(12)',
      'let { mobius } = import("Number-Theory");\nmobius(30)',
    ],
  },
  'mertens': {
    category: 'Number Theory',
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
      'let { mobius } = import("Number-Theory");\nmobius(1)',
      'let { mobius } = import("Number-Theory");\nmobius(2)',
      'let { mobius } = import("Number-Theory");\nmobius(3)',
      'let { mobius } = import("Number-Theory");\nmobius(4)',
      'let { mobius } = import("Number-Theory");\nmobius(6)',
      'let { mobius } = import("Number-Theory");\nmobius(12)',
      'let { mobius } = import("Number-Theory");\nmobius(30)',
    ],
  },
  'sigma': {
    category: 'Number Theory',
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
      'let { sigma } = import("Number-Theory");\nsigma(1)',
      'let { sigma } = import("Number-Theory");\nsigma(2)',
      'let { sigma } = import("Number-Theory");\nsigma(3)',
      'let { sigma } = import("Number-Theory");\nsigma(4)',
      'let { sigma } = import("Number-Theory");\nsigma(6)',
      'let { sigma } = import("Number-Theory");\nsigma(12)',
      'let { sigma } = import("Number-Theory");\nsigma(30)',
    ],
  },
  'carmichael-lambda': {
    category: 'Number Theory',
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
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(1)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(2)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(3)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(4)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(6)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(12)',
      'let { carmichael-lambda } = import("Number-Theory");\ncarmichael-lambda(30)',
    ],
  },
  'cartesian-product': {
    category: 'Number Theory',
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
      'let { cartesian-product } = import("Number-Theory");\ncartesian-product([1, 2], ["a", "b"])',
      'let { cartesian-product } = import("Number-Theory");\ncartesian-product([1, 2], ["a", "b"], [true, false])',
      'let { cartesian-product } = import("Number-Theory");\ncartesian-product([1, 2, 3], ["x", "y", "z"])',
    ],
  },
  'perfect-power': {
    category: 'Number Theory',
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
      'let { perfect-power } = import("Number-Theory");\nperfect-power(1)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(2)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(4)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(8)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(9)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(16)',
      'let { perfect-power } = import("Number-Theory");\nperfect-power(19)',
    ],
  },
  'mod-exp': {
    category: 'Number Theory',
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
      'let { mod-exp } = import("Number-Theory");\nmod-exp(2, 3, 5)',
      'let { mod-exp } = import("Number-Theory");\nmod-exp(3, 4, 7)',
      'let { mod-exp } = import("Number-Theory");\nmod-exp(5, 6, 11)',
      'let { mod-exp } = import("Number-Theory");\nmod-exp(7, 8, 13)',
    ],
  },
  'mod-inv': {
    category: 'Number Theory',
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
      'let { mod-inv } = import("Number-Theory");\nmod-inv(3, 11)',
      'let { mod-inv } = import("Number-Theory");\nmod-inv(10, 17)',
      'let { mod-inv } = import("Number-Theory");\nmod-inv(5, 13)',
      'let { mod-inv } = import("Number-Theory");\nmod-inv(7, 19)',
    ],
  },
  'extended-gcd': {
    category: 'Number Theory',
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
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(30, 12)',
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(56, 98)',
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(101, 10)',
      'let { extended-gcd } = import("Number-Theory");\nextended-gcd(17, 13)',
    ],
  },
  'chinese-remainder': {
    category: 'Number Theory',
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
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([2, 3], [3, 5])',
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([1, 2], [3, 4])',
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([0, 1], [2, 3])',
      'let { chinese-remainder } = import("Number-Theory");\nchinese-remainder([1, 2, 3], [4, 5, 7])',
    ],
  },
  'stirling-first': {
    category: 'Number Theory',
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
      'let { stirling-first } = import("Number-Theory");\nstirling-first(5, 2)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(4, 3)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(6, 1)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(7, 4)',
      'let { stirling-first } = import("Number-Theory");\nstirling-first(8, 5)',
    ],
  },
  'stirling-second': {
    category: 'Number Theory',
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
      'let { stirling-second } = import("Number-Theory");\nstirling-second(5, 2)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(4, 3)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(6, 1)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(7, 4)',
      'let { stirling-second } = import("Number-Theory");\nstirling-second(8, 5)',
    ],
  },
}
