import type { FunctionDocs } from '../../interface'

export const moduleDocs: Record<string, FunctionDocs> = {
  'moving-mean': {
    category: 'vector',
    description: 'Returns the **moving mean** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving mean** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-mean } = import(vector);\nmoving-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-mean } = import(vector);\nmoving-mean([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['moving-fn', 'mean', 'vector.centered-moving-mean', 'vector.running-mean'],
  },
  'centered-moving-mean': {
    category: 'vector',
    description: 'Returns the **centered moving mean** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving mean** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-mean } = import(vector);\ncentered-moving-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-mean } = import(vector);\ncentered-moving-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-mean } = import(vector);\ncentered-moving-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
    seeAlso: ['mean', 'vector.moving-mean', 'vector.running-mean'],
  },
  'running-mean': {
    category: 'vector',
    description: 'Returns the **running mean** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running mean** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-mean } = import(vector);\nrunning-mean([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['running-fn', 'mean', 'vector.moving-mean', 'vector.centered-moving-mean'],
  },
  'geometric-mean': {
    category: 'vector',
    description: 'Returns the **geometric mean** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **geometric mean** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { geometric-mean } = import(vector);\ngeometric-mean([1, 2, 3])',
      'let { geometric-mean } = import(vector);\ngeometric-mean([1, 2, 9])',
    ],
    seeAlso: ['vector.moving-geometric-mean', 'vector.centered-moving-geometric-mean', 'vector.running-geometric-mean', 'mean', 'vector.harmonic-mean'],
  },
  'moving-geometric-mean': {
    category: 'vector',
    description: 'Returns the **moving geometric mean** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving geometric mean** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-geometric-mean } = import(vector);\nmoving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-geometric-mean } = import(vector);\nmoving-geometric-mean([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.geometric-mean', 'vector.centered-moving-geometric-mean', 'vector.running-geometric-mean'],
  },
  'centered-moving-geometric-mean': {
    category: 'vector',
    description: 'Returns the **centered moving geometric mean** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving geometric mean** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-geometric-mean } = import(vector);\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-geometric-mean } = import(vector);\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-geometric-mean } = import(vector);\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
    seeAlso: ['vector.geometric-mean', 'vector.moving-geometric-mean', 'vector.running-geometric-mean'],
  },
  'running-geometric-mean': {
    category: 'vector',
    description: 'Returns the **running geometric mean** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running geometric mean** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-geometric-mean } = import(vector);\nrunning-geometric-mean([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['vector.geometric-mean', 'vector.moving-geometric-mean', 'vector.centered-moving-geometric-mean'],
  },
  'harmonic-mean': {
    category: 'vector',
    description: 'Returns the **harmonic mean** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **harmonic mean** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { harmonic-mean } = import(vector);\nharmonic-mean([1, 2, 3])',
      'let { harmonic-mean } = import(vector);\nharmonic-mean([1, 2, 9])',
    ],
    seeAlso: ['vector.moving-harmonic-mean', 'vector.centered-moving-harmonic-mean', 'vector.running-harmonic-mean', 'mean', 'vector.geometric-mean'],
  },
  'moving-harmonic-mean': {
    category: 'vector',
    description: 'Returns the **moving harmonic mean** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving harmonic mean** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-harmonic-mean } = import(vector);\nmoving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-harmonic-mean } = import(vector);\nmoving-harmonic-mean([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.harmonic-mean', 'vector.centered-moving-harmonic-mean', 'vector.running-harmonic-mean'],
  },
  'centered-moving-harmonic-mean': {
    category: 'vector',
    description: 'Returns the **centered moving harmonic mean** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving harmonic mean** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-harmonic-mean } = import(vector);\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-harmonic-mean } = import(vector);\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-harmonic-mean } = import(vector);\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
    seeAlso: ['vector.harmonic-mean', 'vector.moving-harmonic-mean', 'vector.running-harmonic-mean'],
  },
  'running-harmonic-mean': {
    category: 'vector',
    description: 'Returns the **running harmonic mean** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running harmonic mean** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-harmonic-mean } = import(vector);\nrunning-harmonic-mean([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['vector.harmonic-mean', 'vector.moving-harmonic-mean', 'vector.centered-moving-harmonic-mean'],
  },
  'moving-median': {
    category: 'vector',
    description: 'Returns the **moving median** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving median** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-median } = import(vector);\nmoving-median([1, 2, 3, 4, 5], 3)',
      'let { moving-median } = import(vector);\nmoving-median([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['median', 'vector.centered-moving-median', 'vector.running-median'],
  },
  'centered-moving-median': {
    category: 'vector',
    description: 'Returns the **centered moving median** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving median** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-median } = import(vector);\ncentered-moving-median([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-median } = import(vector);\ncentered-moving-median([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-median } = import(vector);\ncentered-moving-median([1, 2, 3, 4, 5], 3, 10)',
    ],
    seeAlso: ['median', 'vector.moving-median', 'vector.running-median'],
  },
  'running-median': {
    category: 'vector',
    description: 'Returns the **running median** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running median** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-median } = import(vector);\nrunning-median([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['median', 'vector.moving-median', 'vector.centered-moving-median'],
  },
  'variance': {
    category: 'vector',
    description: 'Returns the **variance** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **variance** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { variance } = import(vector);\nvariance([1, 2, 3])',
      'let { variance } = import(vector);\nvariance([1, 2, -3])',
    ],
    seeAlso: ['linear-algebra.cov', 'vector.moving-variance', 'vector.centered-moving-variance', 'vector.running-variance', 'vector.stdev', 'vector.sample-variance', 'vector.mad'],
  },
  'moving-variance': {
    category: 'vector',
    description: 'Returns the **moving variance** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving variance** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-variance } = import(vector);\nmoving-variance([1, 2, 3, 4, 5], 3)',
      'let { moving-variance } = import(vector);\nmoving-variance([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.variance', 'vector.centered-moving-variance', 'vector.running-variance'],
  },
  'centered-moving-variance': {
    category: 'vector',
    description: 'Returns the **centered moving variance** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving variance** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-variance } = import(vector);\ncentered-moving-variance([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-variance } = import(vector);\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-variance } = import(vector);\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-variance } = import(vector);\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
    seeAlso: ['vector.variance', 'vector.moving-variance', 'vector.running-variance'],
  },
  'running-variance': {
    category: 'vector',
    description: 'Returns the **running variance** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running variance** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-variance } = import(vector);\nrunning-variance([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['vector.variance', 'vector.moving-variance', 'vector.centered-moving-variance'],
  },
  'sample-variance': {
    category: 'vector',
    description: 'Returns the sample variance of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample variance of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { sample-variance } = import(vector);\nsample-variance([1, 2, 3])',
      'let { sample-variance } = import(vector);\nsample-variance([1, 2, -3])',
      'let { sample-variance } = import(vector);\nsample-variance([1, 2, 3, 4])',
      'let { sample-variance } = import(vector);\nsample-variance([1, 2, -3, 4])',
      'let { sample-variance } = import(vector);\nsample-variance([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['vector.moving-sample-variance', 'vector.centered-moving-sample-variance', 'vector.running-sample-variance', 'vector.variance', 'vector.sample-stdev'],
  },
  'moving-sample-variance': {
    category: 'vector',
    description: 'Returns the **moving sample variance** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample variance** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-sample-variance } = import(vector);\nmoving-sample-variance([1, 2, 3, 4, 5], 3)',
      'let { moving-sample-variance } = import(vector);\nmoving-sample-variance([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.sample-variance', 'vector.centered-moving-sample-variance', 'vector.running-sample-variance'],
  },
  'centered-moving-sample-variance': {
    category: 'vector',
    description: 'Returns the **centered moving sample variance** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample variance** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-sample-variance } = import(vector);\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sample-variance } = import(vector);\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-sample-variance } = import(vector);\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-sample-variance } = import(vector);\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
    seeAlso: ['vector.sample-variance', 'vector.moving-sample-variance', 'vector.running-sample-variance'],
  },
  'running-sample-variance': {
    category: 'vector',
    description: 'Returns the **running sample variance** of the `vector`.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample variance** of. First element in result is `null` since **sample variance** is not defined for a single element.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-sample-variance } = import(vector);\nrunning-sample-variance([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['vector.sample-variance', 'vector.moving-sample-variance', 'vector.centered-moving-sample-variance'],
  },
  'stdev': {
    category: 'vector',
    description: 'Returns the standard deviation of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the standard deviation of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { stdev } = import(vector);\nstdev([1, 2, 3])',
      'let { stdev } = import(vector);\nstdev([1, 2, -3])',
      'let { stdev } = import(vector);\nstdev([1, 2, 3, 4])',
      'let { stdev } = import(vector);\nstdev([1, 2, -3, 4])',
      'let { stdev } = import(vector);\nstdev([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['vector.moving-stdev', 'vector.centered-moving-stdev', 'vector.running-stdev', 'vector.variance', 'vector.sample-stdev', 'vector.rms', 'vector.mad'],
  },
  'moving-stdev': {
    category: 'vector',
    description: 'Returns the **moving standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving standard deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-stdev } = import(vector);\nmoving-stdev([1, 2, 3, 4, 5], 3)',
      'let { moving-stdev } = import(vector);\nmoving-stdev([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.stdev', 'vector.centered-moving-stdev', 'vector.running-stdev'],
  },
  'centered-moving-stdev': {
    category: 'vector',
    description: 'Returns the **centered moving standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving standard deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-stdev } = import(vector);\ncentered-moving-stdev([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-stdev } = import(vector);\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-stdev } = import(vector);\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-stdev } = import(vector);\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
    seeAlso: ['vector.stdev', 'vector.moving-stdev', 'vector.running-stdev'],
  },
  'running-stdev': {
    category: 'vector',
    description: 'Returns the **running standard deviation** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running standard deviation** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-stdev } = import(vector);\nrunning-stdev([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['vector.stdev', 'vector.moving-stdev', 'vector.centered-moving-stdev'],
  },
  'sample-stdev': {
    category: 'vector',
    description: 'Returns the sample standard deviation of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sample standard deviation of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { sample-stdev } = import(vector);\nsample-stdev([1, 2, 3])',
      'let { sample-stdev } = import(vector);\nsample-stdev([1, 2, -3])',
      'let { sample-stdev } = import(vector);\nsample-stdev([1, 2, 3, 4])',
      'let { sample-stdev } = import(vector);\nsample-stdev([1, 2, -3, 4])',
      'let { sample-stdev } = import(vector);\nsample-stdev([1, 2, 3, 40, 50])',
    ],
    seeAlso: ['vector.moving-sample-stdev', 'vector.centered-moving-sample-stdev', 'vector.running-sample-stdev', 'vector.stdev', 'vector.sample-variance'],
  },
  'moving-sample-stdev': {
    category: 'vector',
    description: 'Returns the **moving sample standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample standard deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-sample-stdev } = import(vector);\nmoving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'let { moving-sample-stdev } = import(vector);\nmoving-sample-stdev([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.sample-stdev', 'vector.centered-moving-sample-stdev', 'vector.running-sample-stdev'],
  },
  'centered-moving-sample-stdev': {
    category: 'vector',
    description: 'Returns the **centered moving sample standard deviation** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample standard deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-sample-stdev } = import(vector);\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sample-stdev } = import(vector);\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-sample-stdev } = import(vector);\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-sample-stdev } = import(vector);\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
    seeAlso: ['vector.sample-stdev', 'vector.moving-sample-stdev', 'vector.running-sample-stdev'],
  },
  'running-sample-stdev': {
    category: 'vector',
    description: 'Returns the **running sample standard deviation** of the `vector`.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample standard deviation** of. First element in result is `null` since **sample standard deviation** is not defined for a single element.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-sample-stdev } = import(vector);\nrunning-sample-stdev([1, 2, 3, 4, 5])',
    ],
    seeAlso: ['vector.sample-stdev', 'vector.moving-sample-stdev', 'vector.centered-moving-sample-stdev'],
  },
  'iqr': {
    category: 'vector',
    description: 'Calculates the **interquartile range** of a `vector`. Returns the difference between the third and first quartiles.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **interquartile range** of. Minimum length is 4.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { iqr } = import(vector);\niqr([1, 2, 3, 4])',
      'let { iqr } = import(vector);\niqr([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { iqr } = import(vector);\niqr(range(1, 1000))',
      'let { iqr } = import(vector);\niqr(map(range(1000), -> 1e6 / ($ + 1) ^ 2))',
      'let { iqr } = import(vector);\nlet { ln } = import(math);\niqr(map(range(1000), -> ln($ + 1)))',
    ],
    seeAlso: ['vector.moving-iqr', 'vector.centered-moving-iqr', 'vector.running-iqr', 'vector.quartiles', 'median', 'vector.mad', 'vector.medad', 'vector.outliers?', 'vector.outliers'],
  },
  'moving-iqr': {
    category: 'vector',
    description: 'Calculates the **moving interquartile range** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving interquartile range** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-iqr } = import(vector);\nmoving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-iqr } = import(vector);\nmoving-iqr([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-iqr } = import(vector);\nmoving-iqr([1, 2, 4, 7, 11, 16], 6)',
    ],
    seeAlso: ['vector.iqr', 'vector.centered-moving-iqr', 'vector.running-iqr'],
  },
  'centered-moving-iqr': {
    category: 'vector',
    description: 'Calculates the **centered moving interquartile range** of a `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving interquartile range** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-iqr } = import(vector);\ncentered-moving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-iqr } = import(vector);\ncentered-moving-iqr([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
    seeAlso: ['vector.iqr', 'vector.moving-iqr', 'vector.running-iqr'],
  },
  'running-iqr': {
    category: 'vector',
    description: 'Calculates the **running interquartile range** of a `vector`. First three element in result is `null` since **running interquartile range** is not defined for less than four elements.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running interquartile range** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-iqr } = import(vector);\nrunning-iqr([1, 2, 3, 4, 5, 6])',
      'let { running-iqr } = import(vector);\nrunning-iqr([-1, -2, -3, 1, 2, 3])',
    ],
    seeAlso: ['vector.iqr', 'vector.moving-iqr', 'vector.centered-moving-iqr'],
  },
  'moving-sum': {
    category: 'vector',
    description: 'Returns the **moving sum** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sum** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-sum } = import(vector);\nmoving-sum([1, 2, 3, 4, 5], 3)',
      'let { moving-sum } = import(vector);\nmoving-sum([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['sum', 'vector.centered-moving-sum', 'vector.running-sum'],
  },
  'centered-moving-sum': {
    category: 'vector',
    description: 'Returns the **centered moving sum** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sum** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the centered moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-sum } = import(vector);\ncentered-moving-sum([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sum } = import(vector);\ncentered-moving-sum([1, 2, 3, 4, 5], 3, 0, 0)',
      'let { centered-moving-sum } = import(vector);\ncentered-moving-sum([1, 2, 3, 4, 5], 3, 10)',
    ],
    seeAlso: ['sum', 'vector.moving-sum', 'vector.running-sum'],
  },
  'running-sum': {
    category: 'vector',
    description: 'Returns the **running sum** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sum** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-sum } = import(vector);\nrunning-sum([1, 2, 3])',
      'let { running-sum } = import(vector);\nrunning-sum([1, -2, -3])',
    ],
    seeAlso: ['sum', 'vector.moving-sum', 'vector.centered-moving-sum', 'vector.cumsum'],
  },
  'moving-prod': {
    category: 'vector',
    description: 'Returns the **moving product** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving product** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-prod } = import(vector);\nmoving-prod([1, 2, 3, 4, 5], 3)',
      'let { moving-prod } = import(vector);\nmoving-prod([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['prod', 'vector.centered-moving-prod', 'vector.running-prod'],
  },
  'centered-moving-prod': {
    category: 'vector',
    description: 'Returns the **centered moving product** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving product** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-prod } = import(vector);\ncentered-moving-prod([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-prod } = import(vector);\ncentered-moving-prod([1, 2, 3, 4, 5], 3, 0, 0)',
    ],
    seeAlso: ['prod', 'vector.moving-prod', 'vector.running-prod'],
  },
  'running-prod': {
    category: 'vector',
    description: 'Returns the **running product** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running product** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-prod } = import(vector);\nrunning-prod([1, 2, 3, 4, 5])',
      'let { running-prod } = import(vector);\nrunning-prod([1, -2, -3])',
    ],
    seeAlso: ['prod', 'vector.moving-prod', 'vector.centered-moving-prod', 'vector.cumprod'],
  },
  'span': {
    category: 'vector',
    description: 'Returns the difference between the maximum and minimum values in a vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the span of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { span } = import(vector);\nspan([1, 2, 3])',
      'let { span } = import(vector);\nspan([1, 1, 2, 3, 3])',
      'let { span } = import(vector);\nspan([1, 2, -3])',
    ],
    seeAlso: ['vector.moving-span', 'vector.centered-moving-span', 'vector.running-span', 'min', 'max'],
  },
  'moving-span': {
    category: 'vector',
    description: 'Calculates the **moving span** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving span** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-span } = import(vector);\nmoving-span([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-span } = import(vector);\nmoving-span([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-span } = import(vector);\nmoving-span([1, 2, 4, 7, 11, 16], 6)',
    ],
    seeAlso: ['vector.span', 'vector.centered-moving-span', 'vector.running-span'],
  },
  'centered-moving-span': {
    category: 'vector',
    description: 'Calculates the **centered moving span** of a `vector` with a given window size. The result is padded with `leftPadding` on the left and right.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving span** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'The value to pad the result with on the left.',
      },
      rightPadding: {
        type: 'number',
        description: 'The value to pad the result with on the right.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-span } = import(vector);\ncentered-moving-span([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-span } = import(vector);\ncentered-moving-span([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
    seeAlso: ['vector.span', 'vector.moving-span', 'vector.running-span'],
  },
  'running-span': {
    category: 'vector',
    description: 'Calculates the **running span** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running span** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-span } = import(vector);\nrunning-span([1, 2, 4])',
    ],
    seeAlso: ['vector.span', 'vector.moving-span', 'vector.centered-moving-span'],
  },
  'skewness': {
    category: 'vector',
    description: 'Calculates the **skewness** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **skewness** of. Minimum length is 3.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { skewness } = import(vector);\nskewness([1, 2, 3, 6, 20])',
      'let { skewness } = import(vector);\nskewness([1, 2, 2, 3])',
    ],
    seeAlso: ['vector.moving-skewness', 'vector.centered-moving-skewness', 'vector.running-skewness', 'vector.kurtosis', 'vector.sample-skewness', 'vector.excess-kurtosis'],
  },
  'moving-skewness': {
    category: 'vector',
    description: 'Calculates the **moving skewness** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving skewness** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-skewness } = import(vector);\nmoving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-skewness } = import(vector);\nmoving-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
    seeAlso: ['vector.skewness', 'vector.centered-moving-skewness', 'vector.running-skewness'],
  },
  'centered-moving-skewness': {
    category: 'vector',
    description: 'Calculates the **centered moving skewness** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving skewness** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-skewness } = import(vector);\ncentered-moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-skewness } = import(vector);\ncentered-moving-skewness([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
    seeAlso: ['vector.skewness', 'vector.moving-skewness', 'vector.running-skewness'],
  },
  'running-skewness': {
    category: 'vector',
    description: 'Calculates the **running skewness** of a `vector` with a given window size. First two element in result is `null` since **running skewness** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running skewness** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-skewness } = import(vector);\nrunning-skewness([1, 2, 4, 7, 11])',
    ],
    seeAlso: ['vector.skewness', 'vector.moving-skewness', 'vector.centered-moving-skewness'],
  },
  'sample-skewness': {
    category: 'vector',
    description: 'Calculates the **sample skewness** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample skewness** of. Minimum length is 3.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { sample-skewness } = import(vector);\nsample-skewness([1, 2, 3, 6, 20])',
      'let { sample-skewness } = import(vector);\nsample-skewness([1, 2, 2, 3])',
    ],
    seeAlso: ['vector.moving-sample-skewness', 'vector.centered-moving-sample-skewness', 'vector.running-sample-skewness', 'vector.skewness', 'vector.sample-kurtosis'],
  },
  'moving-sample-skewness': {
    category: 'vector',
    description: 'Calculates the **moving sample skewness** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample skewness** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-sample-skewness } = import(vector);\nmoving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-skewness } = import(vector);\nmoving-sample-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
    seeAlso: ['vector.sample-skewness', 'vector.centered-moving-sample-skewness', 'vector.running-sample-skewness'],
  },
  'centered-moving-sample-skewness': {
    category: 'vector',
    description: 'Calculates the **centered moving sample skewness** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample skewness** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-sample-skewness } = import(vector);\ncentered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-skewness } = import(vector);\ncentered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
    seeAlso: ['vector.sample-skewness', 'vector.moving-sample-skewness', 'vector.running-sample-skewness'],
  },
  'running-sample-skewness': {
    category: 'vector',
    description: 'Calculates the **running sample skewness** of a `vector` with a given window size. First two element in result is `null` since **running sample skewness** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample skewness** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-sample-skewness } = import(vector);\nrunning-sample-skewness([1, 2, 4, 7, 11])',
    ],
    seeAlso: ['vector.sample-skewness', 'vector.moving-sample-skewness', 'vector.centered-moving-sample-skewness'],
  },
  'excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **excess kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **excess kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { excess-kurtosis } = import(vector);\nexcess-kurtosis([1, 2, 3, 6, 20])',
      'let { excess-kurtosis } = import(vector);\nexcess-kurtosis([1, 2, 2, 3])',
    ],
    seeAlso: ['vector.moving-excess-kurtosis', 'vector.centered-moving-excess-kurtosis', 'vector.running-excess-kurtosis', 'vector.kurtosis', 'vector.sample-excess-kurtosis', 'vector.skewness'],
  },
  'moving-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **moving excess kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving excess kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-excess-kurtosis } = import(vector);\nmoving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-excess-kurtosis } = import(vector);\nmoving-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
    seeAlso: ['vector.excess-kurtosis', 'vector.centered-moving-excess-kurtosis', 'vector.running-excess-kurtosis'],
  },
  'centered-moving-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **centered moving excess kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving excess kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-excess-kurtosis } = import(vector);\ncentered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-excess-kurtosis } = import(vector);\ncentered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
    seeAlso: ['vector.excess-kurtosis', 'vector.moving-excess-kurtosis', 'vector.running-excess-kurtosis'],
  },
  'running-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **running excess kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running excess kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running excess kurtosis** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-excess-kurtosis } = import(vector);\nrunning-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
    seeAlso: ['vector.excess-kurtosis', 'vector.moving-excess-kurtosis', 'vector.centered-moving-excess-kurtosis'],
  },
  'kurtosis': {
    category: 'vector',
    description: 'Calculates the **kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { kurtosis } = import(vector);\nkurtosis([1, 2, 3, 6, 20])',
      'let { kurtosis } = import(vector);\nkurtosis([1, 2, 2, 3])',
    ],
    seeAlso: ['vector.moving-kurtosis', 'vector.centered-moving-kurtosis', 'vector.running-kurtosis', 'vector.excess-kurtosis', 'vector.sample-kurtosis', 'vector.skewness'],
  },
  'moving-kurtosis': {
    category: 'vector',
    description: 'Calculates the **moving kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-kurtosis } = import(vector);\nmoving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-kurtosis } = import(vector);\nmoving-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
    seeAlso: ['vector.kurtosis', 'vector.centered-moving-kurtosis', 'vector.running-kurtosis'],
  },
  'centered-moving-kurtosis': {
    category: 'vector',
    description: 'Calculates the **centered moving kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-kurtosis } = import(vector);\ncentered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-kurtosis } = import(vector);\ncentered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
    seeAlso: ['vector.kurtosis', 'vector.moving-kurtosis', 'vector.running-kurtosis'],
  },
  'running-kurtosis': {
    category: 'vector',
    description: 'Calculates the **running kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running kurtosis** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-kurtosis } = import(vector);\nrunning-kurtosis([1, 2, 4, 7, 11])',
    ],
    seeAlso: ['vector.kurtosis', 'vector.moving-kurtosis', 'vector.centered-moving-kurtosis'],
  },
  'sample-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **sample excess kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample excess kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { sample-excess-kurtosis } = import(vector);\nsample-excess-kurtosis([1, 2, 3, 6, 20])',
      'let { sample-excess-kurtosis } = import(vector);\nsample-excess-kurtosis([1, 2, 2, 3])',
    ],
    seeAlso: ['vector.moving-sample-excess-kurtosis', 'vector.centered-moving-sample-excess-kurtosis', 'vector.running-sample-excess-kurtosis', 'vector.sample-kurtosis', 'vector.excess-kurtosis'],
  },
  'moving-sample-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **moving sample excess kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample excess kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-sample-excess-kurtosis } = import(vector);\nmoving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-excess-kurtosis } = import(vector);\nmoving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
    seeAlso: ['vector.sample-excess-kurtosis', 'vector.centered-moving-sample-excess-kurtosis', 'vector.running-sample-excess-kurtosis'],
  },
  'centered-moving-sample-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **centered moving sample excess kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample excess kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-sample-excess-kurtosis } = import(vector);\ncentered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-excess-kurtosis } = import(vector);\ncentered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
    seeAlso: ['vector.sample-excess-kurtosis', 'vector.moving-sample-excess-kurtosis', 'vector.running-sample-excess-kurtosis'],
  },
  'running-sample-excess-kurtosis': {
    category: 'vector',
    description: 'Calculates the **running sample excess kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running sample excess kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample excess kurtosis** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-sample-excess-kurtosis } = import(vector);\nrunning-sample-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
    seeAlso: ['vector.sample-excess-kurtosis', 'vector.moving-sample-excess-kurtosis', 'vector.centered-moving-sample-excess-kurtosis'],
  },
  'sample-kurtosis': {
    category: 'vector',
    description: 'Calculates the **sample kurtosis** of a `vector`. Returns the third standardized moment.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **sample kurtosis** of. Minimum length is 3.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { sample-kurtosis } = import(vector);\nsample-kurtosis([1, 2, 3, 6, 20])',
      'let { sample-kurtosis } = import(vector);\nsample-kurtosis([1, 2, 2, 3])',
    ],
    seeAlso: ['vector.moving-sample-kurtosis', 'vector.centered-moving-sample-kurtosis', 'vector.running-sample-kurtosis', 'vector.sample-excess-kurtosis', 'vector.kurtosis', 'vector.sample-skewness'],
  },
  'moving-sample-kurtosis': {
    category: 'vector',
    description: 'Calculates the **moving sample kurtosis** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving sample kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-sample-kurtosis } = import(vector);\nmoving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-kurtosis } = import(vector);\nmoving-sample-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
    seeAlso: ['vector.sample-kurtosis', 'vector.centered-moving-sample-kurtosis', 'vector.running-sample-kurtosis'],
  },
  'centered-moving-sample-kurtosis': {
    category: 'vector',
    description: 'Calculates the **centered moving sample kurtosis** of a `vector` with a given window size and padding.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving sample kurtosis** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-sample-kurtosis } = import(vector);\ncentered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-kurtosis } = import(vector);\ncentered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
    seeAlso: ['vector.sample-kurtosis', 'vector.moving-sample-kurtosis', 'vector.running-sample-kurtosis'],
  },
  'running-sample-kurtosis': {
    category: 'vector',
    description: 'Calculates the **running sample kurtosis** of a `vector` with a given window size. First two element in result is `null` since **running sample kurtosis** is not defined for less than three elements.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running sample kurtosis** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-sample-kurtosis } = import(vector);\nrunning-sample-kurtosis([1, 2, 4, 7, 11])',
    ],
    seeAlso: ['vector.sample-kurtosis', 'vector.moving-sample-kurtosis', 'vector.centered-moving-sample-kurtosis'],
  },
  'rms': {
    category: 'vector',
    description: 'Calculates the **root mean square** of a `vector`. Returns the square root of the average of the squares of the elements.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **root mean square** of. Minimum length is 1.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { rms } = import(vector);\nrms([1, 2, 3, 4])',
      'let { rms } = import(vector);\nrms([5, 4, 3, 2, 1])',
      'let { rms } = import(vector);\nrms(range(1, 1000))',
      'let { rms } = import(vector);\nrms(map(range(1000), -> 1e6 / ($ + 1) ^ 2))',
      'let { rms } = import(vector);\nlet { ln } = import(math);\nrms(map(range(1000), -> ln($ + 1)))',
    ],
    seeAlso: ['vector.moving-rms', 'vector.centered-moving-rms', 'vector.running-rms', 'mean', 'vector.stdev'],
  },
  'moving-rms': {
    category: 'vector',
    description: 'Calculates the **moving root mean square** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving root mean square** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-rms } = import(vector);\nmoving-rms([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-rms } = import(vector);\nmoving-rms([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-rms } = import(vector);\nmoving-rms([1, 2, 4, 7, 11, 16], 6)',
    ],
    seeAlso: ['vector.rms', 'vector.centered-moving-rms', 'vector.running-rms'],
  },
  'centered-moving-rms': {
    category: 'vector',
    description: 'Calculates the **centered moving root mean square** of a `vector` with a given window size and padding value.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving root mean square** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-rms } = import(vector);\ncentered-moving-rms([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-rms } = import(vector);\ncentered-moving-rms([1, 2, 4, 7, 11, 16], 5, 0)',
      'let { centered-moving-rms } = import(vector);\ncentered-moving-rms([1, 2, 4, 7, 11, 16], 6, 0, 0)',
    ],
    seeAlso: ['vector.rms', 'vector.moving-rms', 'vector.running-rms'],
  },
  'running-rms': {
    category: 'vector',
    description: 'Calculates the **running root mean square** of a `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running root mean square** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-rms } = import(vector);\nrunning-rms([1, 2, 3, 4, 5, 6])',
      'let { running-rms } = import(vector);\nrunning-rms([1, -3, 2])',
      'let { running-rms } = import(vector);\nrunning-rms([-1, -2, -3])',
      'let { running-rms } = import(vector);\nrunning-rms([0])',
    ],
    seeAlso: ['vector.rms', 'vector.moving-rms', 'vector.centered-moving-rms'],
  },
  'mad': {
    category: 'vector',
    description: 'Returns the **mean absolute deviation** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **mean absolute deviation** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { mad } = import(vector);\nmad([1, 2, 3])',
      'let { mad } = import(vector);\nmad([1, 2, -3])',
    ],
    seeAlso: ['vector.moving-mad', 'vector.centered-moving-mad', 'vector.running-mad', 'vector.medad', 'vector.stdev', 'vector.variance', 'vector.iqr'],
  },
  'moving-mad': {
    category: 'vector',
    description: 'Returns the **moving mean absolute deviation** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving mean absolute deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-mad } = import(vector);\nmoving-mad([1, 2, 3, 4, 5], 3)',
      'let { moving-mad } = import(vector);\nmoving-mad([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.mad', 'vector.centered-moving-mad', 'vector.running-mad'],
  },
  'centered-moving-mad': {
    category: 'vector',
    description: 'Returns the **centered moving mean absolute deviation** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving mean absolute deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-mad } = import(vector);\ncentered-moving-mad([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-mad } = import(vector);\ncentered-moving-mad([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.mad', 'vector.moving-mad', 'vector.running-mad'],
  },
  'running-mad': {
    category: 'vector',
    description: 'Returns the **running mean absolute deviation** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running mean absolute deviation** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-mad } = import(vector);\nrunning-mad([1, 2, 3])',
      'let { running-mad } = import(vector);\nrunning-mad([1, 2, -3])',
    ],
    seeAlso: ['vector.mad', 'vector.moving-mad', 'vector.centered-moving-mad'],
  },
  'medad': {
    category: 'vector',
    description: 'Returns the **median absolute deviation** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **median absolute deviation** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { medad } = import(vector);\nmedad([1, 2, 3])',
      'let { medad } = import(vector);\nmedad([1, 2, -3])',
    ],
    seeAlso: ['vector.moving-medad', 'vector.centered-moving-medad', 'vector.running-medad', 'vector.mad', 'median', 'vector.iqr'],
  },
  'moving-medad': {
    category: 'vector',
    description: 'Returns the **moving median absolute deviation** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving median absolute deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-medad } = import(vector);\nmoving-medad([1, 2, 3, 4, 5], 3)',
      'let { moving-medad } = import(vector);\nmoving-medad([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.medad', 'vector.centered-moving-medad', 'vector.running-medad'],
  },
  'centered-moving-medad': {
    category: 'vector',
    description: 'Returns the **centered moving median absolute deviation** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving median absolute deviation** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-medad } = import(vector);\ncentered-moving-medad([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-medad } = import(vector);\ncentered-moving-medad([1, 2, 3, 4, 5], 5)',
    ],
    seeAlso: ['vector.medad', 'vector.moving-medad', 'vector.running-medad'],
  },
  'running-medad': {
    category: 'vector',
    description: 'Returns the **running median absolute deviation** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running median absolute deviation** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-medad } = import(vector);\nrunning-medad([1, 2, 3])',
      'let { running-medad } = import(vector);\nrunning-medad([1, 2, -3])',
    ],
    seeAlso: ['vector.medad', 'vector.moving-medad', 'vector.centered-moving-medad'],
  },
  'gini-coefficient': {
    category: 'vector',
    description: 'Returns the **gini coefficient** of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **gini coefficient** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { gini-coefficient } = import(vector);\ngini-coefficient([1, 2, 3])',
      'let { gini-coefficient } = import(vector);\ngini-coefficient([1, 1, 3])',
    ],
    seeAlso: ['vector.moving-gini-coefficient', 'vector.centered-moving-gini-coefficient', 'vector.running-gini-coefficient', 'vector.entropy'],
  },
  'moving-gini-coefficient': {
    category: 'vector',
    description: 'Returns the **moving gini coefficient** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving gini coefficient** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-gini-coefficient } = import(vector);\nmoving-gini-coefficient([1, 2, 3], 2)',
      'let { moving-gini-coefficient } = import(vector);\nmoving-gini-coefficient([1, 1, 3], 2)',
    ],
    seeAlso: ['vector.gini-coefficient', 'vector.centered-moving-gini-coefficient', 'vector.running-gini-coefficient'],
  },
  'centered-moving-gini-coefficient': {
    category: 'vector',
    description: 'Returns the **centered moving gini coefficient** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving gini coefficient** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-gini-coefficient } = import(vector);\ncentered-moving-gini-coefficient([1, 2, 3], 2)',
      'let { centered-moving-gini-coefficient } = import(vector);\ncentered-moving-gini-coefficient([1, 1, 3], 2)',
    ],
    seeAlso: ['vector.gini-coefficient', 'vector.moving-gini-coefficient', 'vector.running-gini-coefficient'],
  },
  'running-gini-coefficient': {
    category: 'vector',
    description: 'Returns the **running gini coefficient** of the `vector`.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running gini coefficient** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-gini-coefficient } = import(vector);\nrunning-gini-coefficient([1, 2, 3])',
      'let { running-gini-coefficient } = import(vector);\nrunning-gini-coefficient([1, 1, 3])',
    ],
    seeAlso: ['vector.gini-coefficient', 'vector.moving-gini-coefficient', 'vector.centered-moving-gini-coefficient'],
  },
  'entropy': {
    category: 'vector',
    description: 'Calculates the **entropy** of a `vector`. The entropy is a measure of the uncertainty associated with a random variable.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **entropy** of. Minimum length is 1.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { entropy } = import(vector);\nentropy([1, 1, 2, 3, 3, 3])',
      'let { entropy } = import(vector);\nentropy([1, 2, 3])',
      'let { entropy } = import(vector);\nentropy([1, 2, 2, 3])',
      'let { entropy } = import(vector);\nentropy([0])',
      'let { entropy } = import(vector);\nentropy([1])',
      'let { entropy } = import(vector);\nentropy([1, 2])',
    ],
    seeAlso: ['vector.moving-entropy', 'vector.centered-moving-entropy', 'vector.running-entropy', 'vector.gini-coefficient'],
  },
  'moving-entropy': {
    category: 'vector',
    description: 'Calculates the **moving entropy** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving entropy** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
    ],
    examples: [
      'let { moving-entropy } = import(vector);\nmoving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'let { moving-entropy } = import(vector);\nmoving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'let { moving-entropy } = import(vector);\nmoving-entropy([1, 2], 2)',
    ],
    seeAlso: ['vector.entropy', 'vector.centered-moving-entropy', 'vector.running-entropy'],
  },
  'centered-moving-entropy': {
    category: 'vector',
    description: 'Calculates the **centered moving entropy** of a `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving entropy** of.',
      },
      windowSize: {
        type: 'integer',
        description: 'The size of the moving window.',
      },
      leftPadding: {
        type: 'number',
        description: 'Optional value to use for padding. Default is `null`.',
      },
      rightPadding: {
        type: 'number',
        description: 'Optional value to use for right padding. Default is `null`.',
      },
      a: {
        type: 'vector',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'windowSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
        ],
      },
      {
        argumentNames: [
          'vector',
          'windowSize',
          'leftPadding',
          'rightPadding',
        ],
      },
    ],
    examples: [
      'let { centered-moving-entropy } = import(vector);\ncentered-moving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'let { centered-moving-entropy } = import(vector);\ncentered-moving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'let { centered-moving-entropy } = import(vector);\ncentered-moving-entropy([1, 2], 2)',
    ],
    seeAlso: ['vector.entropy', 'vector.moving-entropy', 'vector.running-entropy'],
  },
  'running-entropy': {
    category: 'vector',
    description: 'Calculates the **running entropy** of a `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running entropy** of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { running-entropy } = import(vector);\nrunning-entropy([1, 1, 2, 3, 3, 3])',
      'let { running-entropy } = import(vector);\nrunning-entropy([1, 2])',
    ],
    seeAlso: ['vector.entropy', 'vector.moving-entropy', 'vector.centered-moving-entropy'],
  },
  'monotonic?': {
    category: 'vector',
    description: 'Checks if a vector is monotonic.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { monotonic? } = import(vector);\nmonotonic?([1, 2, 3])',
      'let { monotonic? } = import(vector);\nmonotonic?([1, 2, 2, 3])',
      'let { monotonic? } = import(vector);\nmonotonic?([3, 2, 1])',
      'let { monotonic? } = import(vector);\nmonotonic?([3, 2, 1, 1])',
      'let { monotonic? } = import(vector);\nmonotonic?([3, 2, 1, 2])',
      'let { monotonic? } = import(vector);\nmonotonic?([1])',
      'let { monotonic? } = import(vector);\nmonotonic?([])',
    ],
    seeAlso: ['vector.strictly-monotonic?', 'vector.increasing?', 'vector.decreasing?'],
  },
  'strictly-monotonic?': {
    category: 'vector',
    description: 'Checks if a vector is strictly monotonic.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([1, 2, 3])',
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([1, 2, 2, 3])',
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([3, 2, 1])',
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([3, 2, 1, 1])',
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([3, 2, 1, 2])',
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([1])',
      'let { strictly-monotonic? } = import(vector);\nstrictly-monotonic?([])',
    ],
    seeAlso: ['vector.monotonic?', 'vector.strictly-increasing?', 'vector.strictly-decreasing?'],
  },
  'increasing?': {
    category: 'vector',
    description: 'Checks if a vector is increasing.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { increasing? } = import(vector);\nincreasing?([1, 2, 3])',
      'let { increasing? } = import(vector);\nincreasing?([1, 2, 2, 3])',
      'let { increasing? } = import(vector);\nincreasing?([3, 2, 1])',
      'let { increasing? } = import(vector);\nincreasing?([3, 2, 1, 1])',
      'let { increasing? } = import(vector);\nincreasing?([3, 2, 1, 2])',
      'let { increasing? } = import(vector);\nincreasing?([1])',
      'let { increasing? } = import(vector);\nincreasing?([])',
    ],
    seeAlso: ['vector.strictly-increasing?', 'vector.decreasing?', 'vector.strictly-decreasing?', 'vector.monotonic?'],
  },
  'decreasing?': {
    category: 'vector',
    description: 'Checks if a vector is decreasing.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { decreasing? } = import(vector);\ndecreasing?([1, 2, 3])',
      'let { decreasing? } = import(vector);\ndecreasing?([1, 2, 2, 3])',
      'let { decreasing? } = import(vector);\ndecreasing?([3, 2, 1])',
      'let { decreasing? } = import(vector);\ndecreasing?([3, 2, 1, 1])',
      'let { decreasing? } = import(vector);\ndecreasing?([3, 2, 1, 2])',
      'let { decreasing? } = import(vector);\ndecreasing?([1])',
      'let { decreasing? } = import(vector);\ndecreasing?([])',
    ],
    seeAlso: ['vector.strictly-decreasing?', 'vector.increasing?', 'vector.strictly-increasing?', 'vector.monotonic?'],
  },
  'strictly-increasing?': {
    category: 'vector',
    description: 'Checks if a vector is strictly increasing.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([1, 2, 3])',
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([1, 2, 2, 3])',
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([3, 2, 1])',
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([3, 2, 1, 1])',
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([3, 2, 1, 2])',
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([1])',
      'let { strictly-increasing? } = import(vector);\nstrictly-increasing?([])',
    ],
    seeAlso: ['vector.increasing?', 'vector.decreasing?', 'vector.strictly-decreasing?', 'vector.strictly-monotonic?'],
  },
  'strictly-decreasing?': {
    category: 'vector',
    description: 'Checks if a vector is strictly decreasing.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to check.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([1, 2, 3])',
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([1, 2, 2, 3])',
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([3, 2, 1])',
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([3, 2, 1, 1])',
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([3, 2, 1, 2])',
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([1])',
      'let { strictly-decreasing? } = import(vector);\nstrictly-decreasing?([])',
    ],
    seeAlso: ['vector.increasing?', 'vector.strictly-increasing?', 'vector.decreasing?', 'vector.strictly-monotonic?'],
  },
  'mode': {
    category: 'vector',
    description: 'Returns the mode of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the mode of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { mode } = import(vector);\nmode([1, 2, 3])',
      'let { mode } = import(vector);\nmode([1, 2, -3, 1])',
      'let { mode } = import(vector);\nmode([2, 2, 3, 3, 4])',
      'let { mode } = import(vector);\nmode([2, 2, 3, 3])',
      'let { mode } = import(vector);\nmode([1, 2, 3, 2, 1, 2])',
    ],
    seeAlso: ['mean', 'median'],
  },
  'min-index': {
    category: 'vector',
    description: 'Returns the index of the minimum value of all elements in the vector.',
    returns: {
      type: 'integer',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the minimum index of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { min-index } = import(vector);\nmin-index([1, 2, 3])',
      'let { min-index } = import(vector);\nmin-index([1, 1, 2, 3, 3])',
      'let { min-index } = import(vector);\nmin-index([1, 2, -3])',
      'let { min-index } = import(vector);\nmin-index([1, 2, 3, 4])',
      'let { min-index } = import(vector);\nmin-index([1, 2, -3, 4])',
    ],
    seeAlso: ['vector.max-index', 'min'],
  },
  'max-index': {
    category: 'vector',
    description: 'Returns the index of the maximum value of all elements in the vector.',
    returns: {
      type: 'integer',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the maximum index of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { max-index } = import(vector);\nmax-index([1, 2, 3])',
      'let { max-index } = import(vector);\nmax-index([1, 1, 2, 3, 3])',
      'let { max-index } = import(vector);\nmax-index([1, 2, -3])',
      'let { max-index } = import(vector);\nmax-index([1, 2, 3, 4])',
      'let { max-index } = import(vector);\nmax-index([1, 2, -3, 4])',
    ],
    seeAlso: ['vector.min-index', 'max'],
  },
  'sort-indices': {
    category: 'vector',
    description: 'Returns the indices of the elements in the vector sorted in ascending order.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the sorted indices of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { sort-indices } = import(vector);\nsort-indices([1, 2, 3])',
      'let { sort-indices } = import(vector);\nsort-indices([1, 1, 2, 3, 3])',
      'let { sort-indices } = import(vector);\nsort-indices([1, 2, -3])',
      'let { sort-indices } = import(vector);\nsort-indices([1, 2, 3, 4])',
      'let { sort-indices } = import(vector);\nsort-indices([1, 2, -3, 4])',
    ],
    seeAlso: ['sort'],
  },
  'count-values': {
    category: 'vector',
    description: 'Counts the number of occurrences of each value in the vector.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Vector to count the values of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { count-values } = import(vector);\ncount-values([1, 2, 3])',
      'let { count-values } = import(vector);\ncount-values([1, 1, 2, 3, 3])',
      'let { count-values } = import(vector);\ncount-values([1, 2, -3])',
      'let { count-values } = import(vector);\ncount-values([1, 2, 2, 1, 3, 2, 4, 2, 1, 2, 2, 1, 3, 2, 4])',
    ],
    seeAlso: ['sequence.frequencies', 'vector.bincount'],
  },
  'linspace': {
    category: 'vector',
    description: 'Generates a vector of evenly spaced numbers between two values.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      start: {
        type: 'number',
        description: 'The starting value.',
      },
      stop: {
        type: 'number',
        description: 'The ending value.',
      },
      n: {
        type: 'integer',
        description: 'The number of values to generate.',
      },
    },
    variants: [
      {
        argumentNames: [
          'start',
          'stop',
          'n',
        ],
      },
    ],
    examples: [
      'let { linspace } = import(vector);\nlinspace(0, 10, 6)',
      'let { linspace } = import(vector);\nlinspace(10, 20, 25)',
    ],
    seeAlso: [
      'range',
    ],
  },
  'cumsum': {
    category: 'vector',
    description: 'Calculates the cumulative sum of a vector.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the cumulative sum of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { cumsum } = import(vector);\ncumsum([1, 2, 3])',
      'let { cumsum } = import(vector);\ncumsum([1, 2, -3])',
      'let { cumsum } = import(vector);\ncumsum([])',
    ],
    seeAlso: ['vector.cumprod', 'sum', 'vector.running-sum'],
  },
  'cumprod': {
    category: 'vector',
    description: 'Calculates the cumulative product of a vector.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the cumulative product of.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { cumprod } = import(vector);\ncumprod([1, 2, 3])',
      'let { cumprod } = import(vector);\ncumprod([1, 2, -3, 0, 10])',
      'let { cumprod } = import(vector);\ncumprod([])',
    ],
    seeAlso: ['vector.cumsum', 'prod', 'vector.running-prod'],
  },
  'quartiles': {
    category: 'vector',
    description: 'Calculates the quartiles of a vector. Returns an array containing the first, second (median), and third quartiles.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the quartiles of. Minimum length is 4.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { quartiles } = import(vector);\nquartiles([1, 2, 3, 4])',
      'let { quartiles } = import(vector);\nquartiles([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { quartiles } = import(vector);\nquartiles(range(1, 1000))',
      'let { quartiles } = import(vector);\nquartiles(map(range(1000), -> 1e6 / ($ + 1) ^ 2))',
      'let { quartiles } = import(vector);\nlet { ln } = import(math);\nquartiles(map(range(1000), -> ln($ + 1)))',
    ],
    seeAlso: ['vector.percentile', 'vector.quantile', 'median', 'vector.iqr'],
  },
  'percentile': {
    category: 'vector',
    description: 'Calculates the percentile of a vector. Returns the value at the specified percentile.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The non empty vector to calculate the percentile of.',
      },
      percentile: {
        type: 'number',
        description: 'The percentile to calculate. Must be between 0 and 1.',
      },
      a: {
        type: 'number',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'percentile',
        ],
      },
    ],
    examples: [
      'let { percentile } = import(vector);\npercentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 0)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 10)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 20)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 30)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 40)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 50)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 60)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 70)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 80)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 90)',
      'let { percentile } = import(vector);\npercentile(range(100) ^ 0.5, 100)',
    ],
    seeAlso: ['vector.quantile', 'vector.quartiles', 'median', 'vector.ecdf', 'vector.winsorize'],
  },
  'quantile': {
    category: 'vector',
    description: 'Calculates the quantile of a vector. Returns the value at the specified quantile.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The non empty vector to calculate the quantile of.',
      },
      quantile: {
        type: 'number',
        description: 'The quantile to calculate. Must be between 0 and 1.',
      },
      a: {
        type: 'number',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'quantile',
        ],
      },
    ],
    examples: [
      'let { quantile } = import(vector);\nquantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.1)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.2)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.3)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.4)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.5)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.6)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.7)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.8)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 0.9)',
      'let { quantile } = import(vector);\nquantile(range(100) ^ 0.5, 1)',
    ],
    seeAlso: ['vector.percentile', 'vector.quartiles', 'vector.ecdf'],
  },
  'histogram': {
    category: 'vector',
    description: 'Creates a histogram from a numeric `array` by dividing the data range into the specified number of bins. Returns an `array` of `[binStart, binEnd, count]` tuples representing each bin\'s range and the number of values within it. Handles empty arrays, identical values, and properly places maximum values in the last bin.',
    returns: {
      type: 'array',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The numeric array to create a histogram from.',
      },
      bins: {
        type: 'integer',
        description: 'The number of bins to divide the data range into.',
      },
      a: {
        type: 'number',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'bins',
        ],
      },
    ],
    examples: [
      'let { histogram } = import(vector);\nhistogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'let { histogram } = import(vector);\nhistogram([1, 2, 3, 4, 5], 5)',
      'let { histogram } = import(vector);\nhistogram([1, 2, 3, 4, 5], 10)',
      'let { histogram } = import(vector);\nhistogram([1, 2, 3, 4, 5], 1)',
    ],
    seeAlso: ['vector.bincount', 'vector.ecdf'],
  },
  'ecdf': {
    category: 'vector',
    description: 'Calculates the empirical cumulative distribution function value for a given threshold in a non empty dataset. Returns the proportion of values in the `array` that are less than or equal to the specified threshold.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The numeric array to calculate the ECDF from.',
      },
      threshold: {
        type: 'number',
        description: 'The threshold value to calculate the ECDF for.',
      },
      a: {
        type: 'number',
      },
      b: {
        type: 'integer',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'threshold',
        ],
      },
    ],
    examples: [
      'let { ecdf } = import(vector);\necdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)',
      'let { ecdf } = import(vector);\necdf([1, 2, 3, 4, 5], 3)',
      'let { ecdf } = import(vector);\necdf([1, 2, 3, 4, 5], 0)',
      'let { ecdf } = import(vector);\necdf([1, 2, 3, 4, 5], 10)',
      'let { ecdf } = import(vector);\necdf([1, 2, 3, 4, 5], 2)',
    ],
    seeAlso: ['vector.histogram', 'vector.percentile', 'vector.quantile'],
  },
  'outliers?': {
    category: 'vector',
    description: 'Checks if the `vector` contains outliers based on the interquartile range (IQR) method. Returns `true` if outliers are present, `false` otherwise.',
    returns: {
      type: 'boolean',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to check for outliers.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { outliers? } = import(vector);\noutliers?([1, 2, 3])',
      'let { outliers? } = import(vector);\noutliers?([1, 2, -3])',
      'let { outliers? } = import(vector);\noutliers?([1, 2, 3, 2, 4, 120])',
    ],
    seeAlso: ['vector.outliers', 'vector.winsorize', 'vector.iqr'],
  },
  'outliers': {
    category: 'vector',
    description: 'Identifies outliers in the `vector` based on the interquartile range (IQR) method. Returns an array of outlier values.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to check for outliers.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
    ],
    examples: [
      'let { outliers } = import(vector);\noutliers([1, 2, 3])',
      'let { outliers } = import(vector);\noutliers([1, 2, -3])',
      'let { outliers } = import(vector);\noutliers([1, 2, 3, 2, 4, 120])',
    ],
    seeAlso: ['vector.outliers?', 'vector.winsorize', 'vector.iqr'],
  },
  'bincount': {
    category: 'vector',
    description: 'counts occurrences of each `integer` in a vector, returning an array where index `i` contains the count of value `i`, with optional **minimum size** and **weights parameters**.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to count occurrences in.',
      },
      minSize: {
        type: 'integer',
        description: 'Optional minimum size of the output array.',
      },
      weights: {
        type: 'number',
        array: true,
        description: 'Optional weights for each element in the vector.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
        ],
      },
      {
        argumentNames: [
          'vector',
          'minSize',
        ],
      },
      {
        argumentNames: [
          'vector',
          'minSize',
          'weights',
        ],
      },
    ],
    examples: [
      'let { bincount } = import(vector);\nbincount([1, 2, 3])',
      'let { bincount } = import(vector);\nbincount([1, 2, 2, 3, 3])',
    ],
    seeAlso: ['vector.count-values', 'vector.histogram'],
    hideOperatorForm: true,
  },
  'winsorize': {
    category: 'vector',
    description: 'Limits extreme values in a `vector` by replacing values below the **lower quantile** and above the **upper quantile** with the values at those quantiles. The function takes a `vector` of values and **quantile thresholds** (between 0 and 1), with the upper quantile. Winsorization reduces the influence of outliers while preserving the overall distribution shape, making statistical analyses more robust.',
    returns: {
      type: 'vector',
    },
    args: {
      'vector': {
        type: 'vector',
        description: 'The vector to winsorize.',
      },
      'lower-quantile': {
        type: 'number',
        description: 'The lower quantile threshold (between 0 and 1).',
      },
      'upper-quantile': {
        type: 'number',
        description: 'Optional Upper quantile threshold (between 0 and 1). Defaults to `(1 - lower-quantile)` if `lower-quantile <= 0.5` otherwise `1`.',
      },
    },
    variants: [
      {
        argumentNames: [
          'vector',
          'lower-quantile',
        ],
      },
      {
        argumentNames: [
          'vector',
          'lower-quantile',
          'upper-quantile',
        ],
      },
    ],
    examples: [
      'let { winsorize } = import(vector);\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25)',
      'let { winsorize } = import(vector);\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.75)',
      'let { winsorize } = import(vector);\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.5)',
    ],
    seeAlso: ['vector.outliers', 'vector.outliers?', 'vector.percentile'],
    hideOperatorForm: true,
  },
  'mse': {
    category: 'vector',
    description: 'Calculates the **Mean Squared Error (MSE)** between two vectors. Returns the average of the squared differences between corresponding elements.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
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
      'let { mse } = import(vector);\nmse([1, 2, 3], [1, 2, 3])',
      'let { mse } = import(vector);\nmse([1, 2, 3], [4, 5, 6])',
      'let { mse } = import(vector);\nmse([1, 2, 3], [2, 2, 2])',
      'let { mse } = import(vector);\nmse([1, 2], [3, 3])',
      'let { mse } = import(vector);\nmse([1], [3])',
    ],
    seeAlso: ['vector.rmse', 'vector.mae', 'vector.smape'],
  },
  'rmse': {
    category: 'vector',
    description: 'Calculates the **Root Mean Squared Error (RMSE)** between two vectors. Returns the square root of the average of the squared differences between corresponding elements.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
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
      'let { rmse } = import(vector);\nrmse([1, 2, 3], [1, 2, 3])',
      'let { rmse } = import(vector);\nrmse([1, 2, 3], [4, 5, 6])',
      'let { rmse } = import(vector);\nrmse([1, 2, 3], [2, 2, 2])',
      'let { rmse } = import(vector);\nrmse([1, 2], [3, 3])',
      'let { rmse } = import(vector);\nrmse([1], [3])',
    ],
    seeAlso: ['vector.mse', 'vector.mae', 'vector.smape'],
  },
  'mae': {
    category: 'vector',
    description: 'Calculates the **Mean Absolute Error (MAE)** between two vectors. Returns the average of the absolute differences between corresponding elements.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
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
      'let { mae } = import(vector);\nmae([1, 2, 3], [1, 2, 3])',
      'let { mae } = import(vector);\nmae([1, 2, 3], [4, 5, 6])',
      'let { mae } = import(vector);\nmae([1, 2, 3], [2, 2, 2])',
      'let { mae } = import(vector);\nmae([1, 2], [3, 3])',
      'let { mae } = import(vector);\nmae([1], [3])',
    ],
    seeAlso: ['vector.mse', 'vector.rmse', 'vector.smape'],
  },
  'smape': {
    category: 'vector',
    description: 'Calculates the **Symmetric Mean Absolute Percentage Error (SMAPE)** between two vectors. Returns the average of the absolute percentage differences between corresponding elements.',
    returns: {
      type: 'number',
    },
    args: {
      a: {
        type: 'vector',
        description: 'The first vector.',
      },
      b: {
        type: 'vector',
        description: 'The second vector.',
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
      'let { smape } = import(vector);\nsmape([1, 2, 3], [1, 2, 3])',
      'let { smape } = import(vector);\nsmape([1, 2, 3], [4, 5, 6])',
      'let { smape } = import(vector);\nsmape([1, 2, 3], [2, 2, 2])',
      'let { smape } = import(vector);\nsmape([1, 2], [3, 3])',
      'let { smape } = import(vector);\nsmape([1], [3])',
    ],
    seeAlso: ['vector.mse', 'vector.rmse', 'vector.mae'],
  },
}
