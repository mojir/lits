import type { FunctionDocs } from '../../interface'

export const namespaceDocs: Record<string, FunctionDocs> = {
  'mean': {
    category: 'Vector',
    description: 'Returns the `mean` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `mean` of.',
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
      'let { mean } = import("Vector");\nmean([1, 2, 3])',
      'let { mean } = import("Vector");\nmean([1, 2, -3])',
    ],
  },
  'moving-mean': {
    category: 'Vector',
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
      'let { moving-mean } = import("Vector");\nmoving-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-mean } = import("Vector");\nmoving-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-mean': {
    category: 'Vector',
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
      'let { centered-moving-mean } = import("Vector");\ncentered-moving-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-mean } = import("Vector");\ncentered-moving-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-mean } = import("Vector");\ncentered-moving-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'running-mean': {
    category: 'Vector',
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
      'let { running-mean } = import("Vector");\nrunning-mean([1, 2, 3, 4, 5])',
    ],
  },
  'geometric-mean': {
    category: 'Vector',
    description: 'Returns the `geometric mean` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `geometric mean` of.',
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
      'let { geometric-mean } = import("Vector");\ngeometric-mean([1, 2, 3])',
      'let { geometric-mean } = import("Vector");\ngeometric-mean([1, 2, 9])',
    ],
  },
  'moving-geometric-mean': {
    category: 'Vector',
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
      'let { moving-geometric-mean } = import("Vector");\nmoving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-geometric-mean } = import("Vector");\nmoving-geometric-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-geometric-mean': {
    category: 'Vector',
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
      'let { centered-moving-geometric-mean } = import("Vector");\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-geometric-mean } = import("Vector");\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-geometric-mean } = import("Vector");\ncentered-moving-geometric-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'running-geometric-mean': {
    category: 'Vector',
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
      'let { running-geometric-mean } = import("Vector");\nrunning-geometric-mean([1, 2, 3, 4, 5])',
    ],
  },
  'harmonic-mean': {
    category: 'Vector',
    description: 'Returns the `harmonic mean` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `harmonic mean` of.',
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
      'let { harmonic-mean } = import("Vector");\nharmonic-mean([1, 2, 3])',
      'let { harmonic-mean } = import("Vector");\nharmonic-mean([1, 2, 9])',
    ],
  },
  'moving-harmonic-mean': {
    category: 'Vector',
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
      'let { moving-harmonic-mean } = import("Vector");\nmoving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'let { moving-harmonic-mean } = import("Vector");\nmoving-harmonic-mean([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-harmonic-mean': {
    category: 'Vector',
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
      'let { centered-moving-harmonic-mean } = import("Vector");\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-harmonic-mean } = import("Vector");\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-harmonic-mean } = import("Vector");\ncentered-moving-harmonic-mean([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'running-harmonic-mean': {
    category: 'Vector',
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
      'let { running-harmonic-mean } = import("Vector");\nrunning-harmonic-mean([1, 2, 3, 4, 5])',
    ],
  },
  'median': {
    category: 'Vector',
    description: 'Returns the median of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to calculate the median of.',
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
      'let { median } = import("Vector");\nmedian([1, 2, 3])',
      'let { median } = import("Vector");\nmedian([1, 2, -3])',
      'let { median } = import("Vector");\nmedian([1, 2, 3, 4])',
      'let { median } = import("Vector");\nmedian([1, 2, -3, 4])',
    ],
  },
  'moving-median': {
    category: 'Vector',
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
      'let { moving-median } = import("Vector");\nmoving-median([1, 2, 3, 4, 5], 3)',
      'let { moving-median } = import("Vector");\nmoving-median([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-median': {
    category: 'Vector',
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
      'let { centered-moving-median } = import("Vector");\ncentered-moving-median([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-median } = import("Vector");\ncentered-moving-median([1, 2, 3, 4, 5], 3, 0, 10)',
      'let { centered-moving-median } = import("Vector");\ncentered-moving-median([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'running-median': {
    category: 'Vector',
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
      'let { running-median } = import("Vector");\nrunning-median([1, 2, 3, 4, 5])',
    ],
  },
  'variance': {
    category: 'Vector',
    description: 'Returns the `variance` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `variance` of.',
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
      'let { variance } = import("Vector");\nvariance([1, 2, 3])',
      'let { variance } = import("Vector");\nvariance([1, 2, -3])',
    ],
  },
  'moving-variance': {
    category: 'Vector',
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
      'let { moving-variance } = import("Vector");\nmoving-variance([1, 2, 3, 4, 5], 3)',
      'let { moving-variance } = import("Vector");\nmoving-variance([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-variance': {
    category: 'Vector',
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
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-variance } = import("Vector");\ncentered-moving-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'running-variance': {
    category: 'Vector',
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
      'let { running-variance } = import("Vector");\nrunning-variance([1, 2, 3, 4, 5])',
    ],
  },
  'sample-variance': {
    category: 'Vector',
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
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, -3])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3, 4])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, -3, 4])',
      'let { sample-variance } = import("Vector");\nsample-variance([1, 2, 3, 40, 50])',
    ],
  },
  'moving-sample-variance': {
    category: 'Vector',
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
      'let { moving-sample-variance } = import("Vector");\nmoving-sample-variance([1, 2, 3, 4, 5], 3)',
      'let { moving-sample-variance } = import("Vector");\nmoving-sample-variance([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-sample-variance': {
    category: 'Vector',
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
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-sample-variance } = import("Vector");\ncentered-moving-sample-variance([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'running-sample-variance': {
    category: 'Vector',
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
      'let { running-sample-variance } = import("Vector");\nrunning-sample-variance([1, 2, 3, 4, 5])',
    ],
  },
  'stdev': {
    category: 'Vector',
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
      'let { stdev } = import("Vector");\nstdev([1, 2, 3])',
      'let { stdev } = import("Vector");\nstdev([1, 2, -3])',
      'let { stdev } = import("Vector");\nstdev([1, 2, 3, 4])',
      'let { stdev } = import("Vector");\nstdev([1, 2, -3, 4])',
      'let { stdev } = import("Vector");\nstdev([1, 2, 3, 40, 50])',
    ],
  },
  'moving-stdev': {
    category: 'Vector',
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
      'let { moving-stdev } = import("Vector");\nmoving-stdev([1, 2, 3, 4, 5], 3)',
      'let { moving-stdev } = import("Vector");\nmoving-stdev([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-stdev': {
    category: 'Vector',
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
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-stdev } = import("Vector");\ncentered-moving-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'running-stdev': {
    category: 'Vector',
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
      'let { running-stdev } = import("Vector");\nrunning-stdev([1, 2, 3, 4, 5])',
    ],
  },
  'sample-stdev': {
    category: 'Vector',
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
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, 3])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, -3])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, 3, 4])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, -3, 4])',
      'let { sample-stdev } = import("Vector");\nsample-stdev([1, 2, 3, 40, 50])',
    ],
  },
  'moving-sample-stdev': {
    category: 'Vector',
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
      'let { moving-sample-stdev } = import("Vector");\nmoving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'let { moving-sample-stdev } = import("Vector");\nmoving-sample-stdev([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-sample-stdev': {
    category: 'Vector',
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
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1)',
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 1, 5)',
      'let { centered-moving-sample-stdev } = import("Vector");\ncentered-moving-sample-stdev([1, 2, 3, 4, 5], 3, 0, 6)',
    ],
  },
  'running-sample-stdev': {
    category: 'Vector',
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
      'let { running-sample-stdev } = import("Vector");\nrunning-sample-stdev([1, 2, 3, 4, 5])',
    ],
  },
  'iqr': {
    category: 'Vector',
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
      'let { iqr } = import("Vector");\niqr([1, 2, 3, 4])',
      'let { iqr } = import("Vector");\niqr([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { iqr } = import("Vector");\niqr(range(1, 1000))',
      'let { iqr, generate } = import("Vector");\niqr(generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let { iqr, generate } = import("Vector");\niqr(generate(1000, -> ln($ + 1)))',
    ],
  },
  'moving-iqr': {
    category: 'Vector',
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
      'let { moving-iqr } = import("Vector");\nmoving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-iqr } = import("Vector");\nmoving-iqr([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-iqr } = import("Vector");\nmoving-iqr([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'centered-moving-iqr': {
    category: 'Vector',
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
      'let { centered-moving-iqr } = import("Vector");\ncentered-moving-iqr([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-iqr } = import("Vector");\ncentered-moving-iqr([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'running-iqr': {
    category: 'Vector',
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
      'let { running-iqr } = import("Vector");\nrunning-iqr([1, 2, 3, 4, 5, 6])',
      'let { running-iqr } = import("Vector");\nrunning-iqr([-1, -2, -3, 1, 2, 3])',
    ],
  },
  'sum': {
    category: 'Vector',
    description: 'Returns the sum of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The vector to sum.',
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
      'let { sum } = import("Vector");\nsum([1, 2, 3])',
      'let { sum } = import("Vector");\nsum([1, 2, -3])',
    ],
  },
  'moving-sum': {
    category: 'Vector',
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
      'let { moving-sum } = import("Vector");\nmoving-sum([1, 2, 3, 4, 5], 3)',
      'let { moving-sum } = import("Vector");\nmoving-sum([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-sum': {
    category: 'Vector',
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
      'let { centered-moving-sum } = import("Vector");\ncentered-moving-sum([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-sum } = import("Vector");\ncentered-moving-sum([1, 2, 3, 4, 5], 3, 0, 0)',
      'let { centered-moving-sum } = import("Vector");\ncentered-moving-sum([1, 2, 3, 4, 5], 3, 10)',
    ],
  },
  'running-sum': {
    category: 'Vector',
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
      'let { running-sum } = import("Vector");\nrunning-sum([1, 2, 3])',
      'let { running-sum } = import("Vector");\nrunning-sum([1, -2, -3])',
    ],
  },
  'prod': {
    category: 'Vector',
    description: 'Returns the `product` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `product` of.',
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
      'let { prod } = import("Vector");\nprod([1, 2, 3])',
      'let { prod } = import("Vector");\nprod([1, 2, -3])',
    ],
  },
  'moving-prod': {
    category: 'Vector',
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
      'let { moving-prod } = import("Vector");\nmoving-prod([1, 2, 3, 4, 5], 3)',
      'let { moving-prod } = import("Vector");\nmoving-prod([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-prod': {
    category: 'Vector',
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
      'let { centered-moving-prod } = import("Vector");\ncentered-moving-prod([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-prod } = import("Vector");\ncentered-moving-prod([1, 2, 3, 4, 5], 3, 0, 0)',
    ],
  },
  'running-prod': {
    category: 'Vector',
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
      'let { running-prod } = import("Vector");\nrunning-prod([1, 2, 3, 4, 5])',
      'let { running-prod } = import("Vector");\nrunning-prod([1, -2, -3])',
    ],
  },
  'min': {
    category: 'Vector',
    description: 'Returns the minimum value of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the minimum of.',
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
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, 3])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 1, 2, 3, 3])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, -3])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, 3, 4])',
      '// Using "as" alias because "min" shadows a builtin function\nlet { min as vec-min } = import("Vector");\nvec-min([1, 2, -3, 4])',
    ],
  },
  'moving-min': {
    category: 'Vector',
    description: 'Returns the **moving minimum** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving minimum** of.',
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
      'let { moving-min } = import("Vector");\nmoving-min([1, 2, 3, 4, 5], 3)',
      'let { moving-min } = import("Vector");\nmoving-min([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-min': {
    category: 'Vector',
    description: 'Returns the **centered moving minimum** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving minimum** of.',
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
      'let { centered-moving-min } = import("Vector");\ncentered-moving-min([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-min } = import("Vector");\ncentered-moving-min([1, 2, 3, 4, 5], 3, 0, 100)',
      'let { centered-moving-min } = import("Vector");\ncentered-moving-min([1, 2, 3, 4, 5], 3, 0)',
    ],
  },
  'running-min': {
    category: 'Vector',
    description: 'Returns the **running minimum** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running minimum** of.',
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
      'let { running-min } = import("Vector");\nrunning-min([1, 2, 3])',
      'let { running-min } = import("Vector");\nrunning-min([1, -2, -3])',
    ],
  },
  'max': {
    category: 'Vector',
    description: 'Returns the maximum value of all elements in the vector.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'Non emtpy vector to calculate the maximum of.',
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
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, 3])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 1, 2, 3, 3])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, -3])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, 3, 4])',
      '// Using "as" alias because "max" shadows a builtin function\nlet { max as vec-max } = import("Vector");\nvec-max([1, 2, -3, 4])',
    ],
  },
  'moving-max': {
    category: 'Vector',
    description: 'Returns the **moving maximum** of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **moving maximum** of.',
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
      'let { moving-max } = import("Vector");\nmoving-max([1, 2, 3, 4, 5], 3)',
      'let { moving-max } = import("Vector");\nmoving-max([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-max': {
    category: 'Vector',
    description: 'Returns the **centered moving maximum** of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **centered moving maximum** of.',
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
      'let { centered-moving-max } = import("Vector");\ncentered-moving-max([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-max } = import("Vector");\ncentered-moving-max([1, 2, 3, 4, 5], 3, 0, 100)',
      'let { centered-moving-max } = import("Vector");\ncentered-moving-max([1, 2, 3, 4, 5], 3, 0)',
    ],
  },
  'running-max': {
    category: 'Vector',
    description: 'Returns the **running maximum** of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the **running maximum** of.',
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
      'let { running-max } = import("Vector");\nrunning-max([1, 2, 3])',
      'let { running-max } = import("Vector");\nrunning-max([1, -2, -3])',
    ],
  },
  'span': {
    category: 'Vector',
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
      'let { span } = import("Vector");\nspan([1, 2, 3])',
      'let { span } = import("Vector");\nspan([1, 1, 2, 3, 3])',
      'let { span } = import("Vector");\nspan([1, 2, -3])',
    ],
  },
  'moving-span': {
    category: 'Vector',
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
      'let { moving-span } = import("Vector");\nmoving-span([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-span } = import("Vector");\nmoving-span([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-span } = import("Vector");\nmoving-span([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'centered-moving-span': {
    category: 'Vector',
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
      'let { centered-moving-span } = import("Vector");\ncentered-moving-span([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-span } = import("Vector");\ncentered-moving-span([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
  },
  'running-span': {
    category: 'Vector',
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
      'let { running-span } = import("Vector");\nrunning-span([1, 2, 4])',
    ],
  },
  'skewness': {
    category: 'Vector',
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
      'let { skewness } = import("Vector");\nskewness([1, 2, 3, 6, 20])',
      'let { skewness } = import("Vector");\nskewness([1, 2, 2, 3])',
    ],
  },
  'moving-skewness': {
    category: 'Vector',
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
      'let { moving-skewness } = import("Vector");\nmoving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-skewness } = import("Vector");\nmoving-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'centered-moving-skewness': {
    category: 'Vector',
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
      'let { centered-moving-skewness } = import("Vector");\ncentered-moving-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-skewness } = import("Vector");\ncentered-moving-skewness([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'running-skewness': {
    category: 'Vector',
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
      'let { running-skewness } = import("Vector");\nrunning-skewness([1, 2, 4, 7, 11])',
    ],
  },
  'sample-skewness': {
    category: 'Vector',
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
      'let { sample-skewness } = import("Vector");\nsample-skewness([1, 2, 3, 6, 20])',
      'let { sample-skewness } = import("Vector");\nsample-skewness([1, 2, 2, 3])',
    ],
  },
  'moving-sample-skewness': {
    category: 'Vector',
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
      'let { moving-sample-skewness } = import("Vector");\nmoving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-skewness } = import("Vector");\nmoving-sample-skewness([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'centered-moving-sample-skewness': {
    category: 'Vector',
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
      'let { centered-moving-sample-skewness } = import("Vector");\ncentered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-skewness } = import("Vector");\ncentered-moving-sample-skewness([1, 2, 4, 7, 11, 16], 3, 0, 100)',
    ],
  },
  'running-sample-skewness': {
    category: 'Vector',
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
      'let { running-sample-skewness } = import("Vector");\nrunning-sample-skewness([1, 2, 4, 7, 11])',
    ],
  },
  'excess-kurtosis': {
    category: 'Vector',
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
      'let { excess-kurtosis } = import("Vector");\nexcess-kurtosis([1, 2, 3, 6, 20])',
      'let { excess-kurtosis } = import("Vector");\nexcess-kurtosis([1, 2, 2, 3])',
    ],
  },
  'moving-excess-kurtosis': {
    category: 'Vector',
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
      'let { moving-excess-kurtosis } = import("Vector");\nmoving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-excess-kurtosis } = import("Vector");\nmoving-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'centered-moving-excess-kurtosis': {
    category: 'Vector',
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
      'let { centered-moving-excess-kurtosis } = import("Vector");\ncentered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-excess-kurtosis } = import("Vector");\ncentered-moving-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'running-excess-kurtosis': {
    category: 'Vector',
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
      'let { running-excess-kurtosis } = import("Vector");\nrunning-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
  'kurtosis': {
    category: 'Vector',
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
      'let { kurtosis } = import("Vector");\nkurtosis([1, 2, 3, 6, 20])',
      'let { kurtosis } = import("Vector");\nkurtosis([1, 2, 2, 3])',
    ],
  },
  'moving-kurtosis': {
    category: 'Vector',
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
      'let { moving-kurtosis } = import("Vector");\nmoving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-kurtosis } = import("Vector");\nmoving-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'centered-moving-kurtosis': {
    category: 'Vector',
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
      'let { centered-moving-kurtosis } = import("Vector");\ncentered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-kurtosis } = import("Vector");\ncentered-moving-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 0)',
    ],
  },
  'running-kurtosis': {
    category: 'Vector',
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
      'let { running-kurtosis } = import("Vector");\nrunning-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
  'sample-excess-kurtosis': {
    category: 'Vector',
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
      'let { sample-excess-kurtosis } = import("Vector");\nsample-excess-kurtosis([1, 2, 3, 6, 20])',
      'let { sample-excess-kurtosis } = import("Vector");\nsample-excess-kurtosis([1, 2, 2, 3])',
    ],
  },
  'moving-sample-excess-kurtosis': {
    category: 'Vector',
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
      'let { moving-sample-excess-kurtosis } = import("Vector");\nmoving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-excess-kurtosis } = import("Vector");\nmoving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'centered-moving-sample-excess-kurtosis': {
    category: 'Vector',
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
      'let { centered-moving-sample-excess-kurtosis } = import("Vector");\ncentered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-excess-kurtosis } = import("Vector");\ncentered-moving-sample-excess-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
  },
  'running-sample-excess-kurtosis': {
    category: 'Vector',
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
      'let { running-sample-excess-kurtosis } = import("Vector");\nrunning-sample-excess-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
  'sample-kurtosis': {
    category: 'Vector',
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
      'let { sample-kurtosis } = import("Vector");\nsample-kurtosis([1, 2, 3, 6, 20])',
      'let { sample-kurtosis } = import("Vector");\nsample-kurtosis([1, 2, 2, 3])',
    ],
  },
  'moving-sample-kurtosis': {
    category: 'Vector',
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
      'let { moving-sample-kurtosis } = import("Vector");\nmoving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-sample-kurtosis } = import("Vector");\nmoving-sample-kurtosis([1, 2, 4, 7, 11, 16], 5)',
    ],
  },
  'centered-moving-sample-kurtosis': {
    category: 'Vector',
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
      'let { centered-moving-sample-kurtosis } = import("Vector");\ncentered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-sample-kurtosis } = import("Vector");\ncentered-moving-sample-kurtosis([1, 2, 4, 7, 11, 16], 4, 0, 100)',
    ],
  },
  'running-sample-kurtosis': {
    category: 'Vector',
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
      'let { running-sample-kurtosis } = import("Vector");\nrunning-sample-kurtosis([1, 2, 4, 7, 11])',
    ],
  },
  'rms': {
    category: 'Vector',
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
      'let { rms } = import("Vector");\nrms([1, 2, 3, 4])',
      'let { rms } = import("Vector");\nrms([5, 4, 3, 2, 1])',
      'let { rms } = import("Vector");\nrms(range(1, 1000))',
      'let { rms, generate } = import("Vector");\nrms(generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let { rms, generate } = import("Vector");\nrms(generate(1000, -> ln($ + 1)))',
    ],
  },
  'moving-rms': {
    category: 'Vector',
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
      'let { moving-rms } = import("Vector");\nmoving-rms([1, 2, 4, 7, 11, 16], 4)',
      'let { moving-rms } = import("Vector");\nmoving-rms([1, 2, 4, 7, 11, 16], 5)',
      'let { moving-rms } = import("Vector");\nmoving-rms([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'centered-moving-rms': {
    category: 'Vector',
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
      'let { centered-moving-rms } = import("Vector");\ncentered-moving-rms([1, 2, 4, 7, 11, 16], 4)',
      'let { centered-moving-rms } = import("Vector");\ncentered-moving-rms([1, 2, 4, 7, 11, 16], 5, 0)',
      'let { centered-moving-rms } = import("Vector");\ncentered-moving-rms([1, 2, 4, 7, 11, 16], 6, 0, 0)',
    ],
  },
  'running-rms': {
    category: 'Vector',
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
      'let { running-rms } = import("Vector");\nrunning-rms([1, 2, 3, 4, 5, 6])',
      'let { running-rms } = import("Vector");\nrunning-rms([1, -3, 2])',
      'let { running-rms } = import("Vector");\nrunning-rms([-1, -2, -3])',
      'let { running-rms } = import("Vector");\nrunning-rms([0])',
    ],
  },
  'mad': {
    category: 'Vector',
    description: 'Returns the `mean absolute deviation` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `mean absolute deviation` of.',
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
      'let { mad } = import("Vector");\nmad([1, 2, 3])',
      'let { mad } = import("Vector");\nmad([1, 2, -3])',
    ],
  },
  'moving-mad': {
    category: 'Vector',
    description: 'Returns the `moving mean absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving mean absolute deviation` of.',
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
      'let { moving-mad } = import("Vector");\nmoving-mad([1, 2, 3, 4, 5], 3)',
      'let { moving-mad } = import("Vector");\nmoving-mad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-mad': {
    category: 'Vector',
    description: 'Returns the `centered moving mean absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving mean absolute deviation` of.',
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
      'let { centered-moving-mad } = import("Vector");\ncentered-moving-mad([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-mad } = import("Vector");\ncentered-moving-mad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'running-mad': {
    category: 'Vector',
    description: 'Returns the `running mean absolute deviation` of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running mean absolute deviation` of.',
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
      'let { running-mad } = import("Vector");\nrunning-mad([1, 2, 3])',
      'let { running-mad } = import("Vector");\nrunning-mad([1, 2, -3])',
    ],
  },
  'medad': {
    category: 'Vector',
    description: 'Returns the `median absolute deviation` of all elements in the `vector`.',
    returns: {
      type: 'number',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `median absolute deviation` of.',
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
      'let { medad } = import("Vector");\nmedad([1, 2, 3])',
      'let { medad } = import("Vector");\nmedad([1, 2, -3])',
    ],
  },
  'moving-medad': {
    category: 'Vector',
    description: 'Returns the `moving median absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `moving median absolute deviation` of.',
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
      'let { moving-medad } = import("Vector");\nmoving-medad([1, 2, 3, 4, 5], 3)',
      'let { moving-medad } = import("Vector");\nmoving-medad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'centered-moving-medad': {
    category: 'Vector',
    description: 'Returns the `centered moving median absolute deviation` of the `vector` with a given window size.',
    returns: {
      type: 'array',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `centered moving median absolute deviation` of.',
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
      'let { centered-moving-medad } = import("Vector");\ncentered-moving-medad([1, 2, 3, 4, 5], 3)',
      'let { centered-moving-medad } = import("Vector");\ncentered-moving-medad([1, 2, 3, 4, 5], 5)',
    ],
  },
  'running-medad': {
    category: 'Vector',
    description: 'Returns the `running median absolute deviation` of the `vector`.',
    returns: {
      type: 'vector',
    },
    args: {
      vector: {
        type: 'vector',
        description: 'The `vector` to calculate the `running median absolute deviation` of.',
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
      'let { running-medad } = import("Vector");\nrunning-medad([1, 2, 3])',
      'let { running-medad } = import("Vector");\nrunning-medad([1, 2, -3])',
    ],
  },
  'gini-coefficient': {
    category: 'Vector',
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
      'let { gini-coefficient } = import("Vector");\ngini-coefficient([1, 2, 3])',
      'let { gini-coefficient } = import("Vector");\ngini-coefficient([1, 1, 3])',
    ],
  },
  'moving-gini-coefficient': {
    category: 'Vector',
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
      'let { moving-gini-coefficient } = import("Vector");\nmoving-gini-coefficient([1, 2, 3], 2)',
      'let { moving-gini-coefficient } = import("Vector");\nmoving-gini-coefficient([1, 1, 3], 2)',
    ],
  },
  'centered-moving-gini-coefficient': {
    category: 'Vector',
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
      'let { centered-moving-gini-coefficient } = import("Vector");\ncentered-moving-gini-coefficient([1, 2, 3], 2)',
      'let { centered-moving-gini-coefficient } = import("Vector");\ncentered-moving-gini-coefficient([1, 1, 3], 2)',
    ],
  },
  'running-gini-coefficient': {
    category: 'Vector',
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
      'let { running-gini-coefficient } = import("Vector");\nrunning-gini-coefficient([1, 2, 3])',
      'let { running-gini-coefficient } = import("Vector");\nrunning-gini-coefficient([1, 1, 3])',
    ],
  },
  'entropy': {
    category: 'Vector',
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
      'let { entropy } = import("Vector");\nentropy([1, 1, 2, 3, 3, 3])',
      'let { entropy } = import("Vector");\nentropy([1, 2, 3])',
      'let { entropy } = import("Vector");\nentropy([1, 2, 2, 3])',
      'let { entropy } = import("Vector");\nentropy([0])',
      'let { entropy } = import("Vector");\nentropy([1])',
      'let { entropy } = import("Vector");\nentropy([1, 2])',
    ],
  },
  'moving-entropy': {
    category: 'Vector',
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
      'let { moving-entropy } = import("Vector");\nmoving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'let { moving-entropy } = import("Vector");\nmoving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'let { moving-entropy } = import("Vector");\nmoving-entropy([1, 2], 2)',
    ],
  },
  'centered-moving-entropy': {
    category: 'Vector',
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
      'let { centered-moving-entropy } = import("Vector");\ncentered-moving-entropy([1, 1, 2, 3, 3, 3], 4)',
      'let { centered-moving-entropy } = import("Vector");\ncentered-moving-entropy([1, 1, 2, 3, 3, 3], 3)',
      'let { centered-moving-entropy } = import("Vector");\ncentered-moving-entropy([1, 2], 2)',
    ],
  },
  'running-entropy': {
    category: 'Vector',
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
      'let { running-entropy } = import("Vector");\nrunning-entropy([1, 1, 2, 3, 3, 3])',
      'let { running-entropy } = import("Vector");\nrunning-entropy([1, 2])',
    ],
  },
  'monotonic?': {
    category: 'Vector',
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
      'let { monotonic? } = import("Vector");\nmonotonic?([1, 2, 3])',
      'let { monotonic? } = import("Vector");\nmonotonic?([1, 2, 2, 3])',
      'let { monotonic? } = import("Vector");\nmonotonic?([3, 2, 1])',
      'let { monotonic? } = import("Vector");\nmonotonic?([3, 2, 1, 1])',
      'let { monotonic? } = import("Vector");\nmonotonic?([3, 2, 1, 2])',
      'let { monotonic? } = import("Vector");\nmonotonic?([1])',
      'let { monotonic? } = import("Vector");\nmonotonic?([])',
    ],
  },
  'strictly-monotonic?': {
    category: 'Vector',
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
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([1, 2, 3])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([1, 2, 2, 3])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([3, 2, 1])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([3, 2, 1, 1])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([3, 2, 1, 2])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([1])',
      'let { strictly-monotonic? } = import("Vector");\nstrictly-monotonic?([])',
    ],
  },
  'increasing?': {
    category: 'Vector',
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
      'let { increasing? } = import("Vector");\nincreasing?([1, 2, 3])',
      'let { increasing? } = import("Vector");\nincreasing?([1, 2, 2, 3])',
      'let { increasing? } = import("Vector");\nincreasing?([3, 2, 1])',
      'let { increasing? } = import("Vector");\nincreasing?([3, 2, 1, 1])',
      'let { increasing? } = import("Vector");\nincreasing?([3, 2, 1, 2])',
      'let { increasing? } = import("Vector");\nincreasing?([1])',
      'let { increasing? } = import("Vector");\nincreasing?([])',
    ],
  },
  'decreasing?': {
    category: 'Vector',
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
      'let { decreasing? } = import("Vector");\ndecreasing?([1, 2, 3])',
      'let { decreasing? } = import("Vector");\ndecreasing?([1, 2, 2, 3])',
      'let { decreasing? } = import("Vector");\ndecreasing?([3, 2, 1])',
      'let { decreasing? } = import("Vector");\ndecreasing?([3, 2, 1, 1])',
      'let { decreasing? } = import("Vector");\ndecreasing?([3, 2, 1, 2])',
      'let { decreasing? } = import("Vector");\ndecreasing?([1])',
      'let { decreasing? } = import("Vector");\ndecreasing?([])',
    ],
  },
  'strictly-increasing?': {
    category: 'Vector',
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
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([1, 2, 3])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([1, 2, 2, 3])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([3, 2, 1])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([3, 2, 1, 1])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([3, 2, 1, 2])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([1])',
      'let { strictly-increasing? } = import("Vector");\nstrictly-increasing?([])',
    ],
  },
  'strictly-decreasing?': {
    category: 'Vector',
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
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([1, 2, 3])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([1, 2, 2, 3])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([3, 2, 1])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([3, 2, 1, 1])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([3, 2, 1, 2])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([1])',
      'let { strictly-decreasing? } = import("Vector");\nstrictly-decreasing?([])',
    ],
  },
  'mode': {
    category: 'Vector',
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
      'let { mode } = import("Vector");\nmode([1, 2, 3])',
      'let { mode } = import("Vector");\nmode([1, 2, -3, 1])',
      'let { mode } = import("Vector");\nmode([2, 2, 3, 3, 4])',
      'let { mode } = import("Vector");\nmode([2, 2, 3, 3])',
      'let { mode } = import("Vector");\nmode([1, 2, 3, 2, 1, 2])',
    ],
  },
  'min-index': {
    category: 'Vector',
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
      'let { min-index } = import("Vector");\nmin-index([1, 2, 3])',
      'let { min-index } = import("Vector");\nmin-index([1, 1, 2, 3, 3])',
      'let { min-index } = import("Vector");\nmin-index([1, 2, -3])',
      'let { min-index } = import("Vector");\nmin-index([1, 2, 3, 4])',
      'let { min-index } = import("Vector");\nmin-index([1, 2, -3, 4])',
    ],
  },
  'max-index': {
    category: 'Vector',
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
      'let { max-index } = import("Vector");\nmax-index([1, 2, 3])',
      'let { max-index } = import("Vector");\nmax-index([1, 1, 2, 3, 3])',
      'let { max-index } = import("Vector");\nmax-index([1, 2, -3])',
      'let { max-index } = import("Vector");\nmax-index([1, 2, 3, 4])',
      'let { max-index } = import("Vector");\nmax-index([1, 2, -3, 4])',
    ],
  },
  'sort-indices': {
    category: 'Vector',
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
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, 3])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 1, 2, 3, 3])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, -3])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, 3, 4])',
      'let { sort-indices } = import("Vector");\nsort-indices([1, 2, -3, 4])',
    ],
  },
  'count-values': {
    category: 'Vector',
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
      'let { count-values } = import("Vector");\ncount-values([1, 2, 3])',
      'let { count-values } = import("Vector");\ncount-values([1, 1, 2, 3, 3])',
      'let { count-values } = import("Vector");\ncount-values([1, 2, -3])',
      'let { count-values } = import("Vector");\ncount-values([1, 2, 2, 1, 3, 2, 4, 2, 1, 2, 2, 1, 3, 2, 4])',
    ],
  },
  'linspace': {
    category: 'Vector',
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
      'let { linspace } = import("Vector");\nlinspace(0, 10, 6)',
      'let { linspace } = import("Vector");\nlinspace(10, 20, 25)',
    ],
  },
  'ones': {
    category: 'Vector',
    description: 'Generates a vector of ones.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
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
      'let { ones } = import("Vector");\nones(5)',
      'let { ones } = import("Vector");\nones(10)',
      'let { ones } = import("Vector");\nones(0)',
    ],
  },
  'zeros': {
    category: 'Vector',
    description: 'Generates a vector of zeros.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
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
      'let { zeros } = import("Vector");\nzeros(5)',
      'let { zeros } = import("Vector");\nzeros(10)',
      'let { zeros } = import("Vector");\nzeros(0)',
    ],
  },
  'fill': {
    category: 'Vector',
    description: 'Generates a vector filled with a number.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
      },
      value: {
        type: 'number',
        description: 'The value to fill the vector with.',
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
          'length',
          'value',
        ],
      },
    ],
    examples: [
      'let { fill } = import("Vector");\nfill(5, PI)',
      'let { fill } = import("Vector");\nfill(10, -1)',
    ],
  },
  'generate': {
    category: 'Vector',
    description: 'Generates a vector of numbers based on a function.',
    returns: {
      type: 'number',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the vector.',
      },
      func: {
        type: 'function',
        description: 'A function that takes an index and returns a number.',
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
          'length',
          'func',
        ],
      },
    ],
    examples: [
      'let { generate } = import("Vector");\ngenerate(5, -> $ * 2)',
      'let { generate } = import("Vector");\ngenerate(10, -> $ + 1)',
      'let { generate } = import("Vector");\ngenerate(0, -> $ + 1)',
    ],
  },
  'cumsum': {
    category: 'Vector',
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
      'let { cumsum } = import("Vector");\ncumsum([1, 2, 3])',
      'let { cumsum } = import("Vector");\ncumsum([1, 2, -3])',
      'let { cumsum } = import("Vector");\ncumsum([])',
    ],
  },
  'cumprod': {
    category: 'Vector',
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
      'let { cumprod } = import("Vector");\ncumprod([1, 2, 3])',
      'let { cumprod } = import("Vector");\ncumprod([1, 2, -3, 0, 10])',
      'let { cumprod } = import("Vector");\ncumprod([])',
    ],
  },
  'quartiles': {
    category: 'Vector',
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
      'let { quartiles } = import("Vector");\nquartiles([1, 2, 3, 4])',
      'let { quartiles } = import("Vector");\nquartiles([5, 4, 3, 2, 1, 2, 3, 4, 5])',
      'let { quartiles } = import("Vector");\nquartiles(range(1, 1000))',
      'let { quartiles, generate } = import("Vector");\nquartiles(generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let { quartiles, generate } = import("Vector");\nquartiles(generate(1000, -> ln($ + 1)))',
    ],
  },
  'percentile': {
    category: 'Vector',
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
      'let { percentile } = import("Vector");\npercentile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 35)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 0)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 10)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 20)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 30)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 40)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 50)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 60)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 70)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 80)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 90)',
      'let { percentile } = import("Vector");\npercentile(range(100) ^ 0.5, 100)',
    ],
  },
  'quantile': {
    category: 'Vector',
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
      'let { quantile } = import("Vector");\nquantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.35)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.1)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.2)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.3)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.4)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.5)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.6)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.7)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.8)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 0.9)',
      'let { quantile } = import("Vector");\nquantile(range(100) ^ 0.5, 1)',
    ],
  },
  'histogram': {
    category: 'Vector',
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
      'let { histogram } = import("Vector");\nhistogram([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9], 3)',
      'let { histogram } = import("Vector");\nhistogram([1, 2, 3, 4, 5], 5)',
      'let { histogram } = import("Vector");\nhistogram([1, 2, 3, 4, 5], 10)',
      'let { histogram } = import("Vector");\nhistogram([1, 2, 3, 4, 5], 1)',
    ],
  },
  'ecdf': {
    category: 'Vector',
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
      'let { ecdf } = import("Vector");\necdf([1, 2, 2, 3, 2, 6, 4, 3, 2, 4, 1, 3, 2, 9, 10, 12], 5)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 3)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 0)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 10)',
      'let { ecdf } = import("Vector");\necdf([1, 2, 3, 4, 5], 2)',
    ],
  },
  'outliers?': {
    category: 'Vector',
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
      'let { outliers? } = import("Vector");\noutliers?([1, 2, 3])',
      'let { outliers? } = import("Vector");\noutliers?([1, 2, -3])',
      'let { outliers? } = import("Vector");\noutliers?([1, 2, 3, 2, 4, 120])',
    ],
  },
  'outliers': {
    category: 'Vector',
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
      'let { outliers } = import("Vector");\noutliers([1, 2, 3])',
      'let { outliers } = import("Vector");\noutliers([1, 2, -3])',
      'let { outliers } = import("Vector");\noutliers([1, 2, 3, 2, 4, 120])',
    ],
  },
  'bincount': {
    category: 'Vector',
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
      'let { bincount } = import("Vector");\nbincount([1, 2, 3])',
      'let { bincount } = import("Vector");\nbincount([1, 2, 2, 3, 3])',
    ],
    hideOperatorForm: true,
  },
  'winsorize': {
    category: 'Vector',
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
      'let { winsorize } = import("Vector");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25)',
      'let { winsorize } = import("Vector");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.75)',
      'let { winsorize } = import("Vector");\nwinsorize([2, 5, 8, 10, 15, 18, 20, 35, 60, 100], 0.25, 0.5)',
    ],
    hideOperatorForm: true,
  },
  'mse': {
    category: 'Vector',
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
      'let { mse } = import("Vector");\nmse([1, 2, 3], [1, 2, 3])',
      'let { mse } = import("Vector");\nmse([1, 2, 3], [4, 5, 6])',
      'let { mse } = import("Vector");\nmse([1, 2, 3], [2, 2, 2])',
      'let { mse } = import("Vector");\nmse([1, 2], [3, 3])',
      'let { mse } = import("Vector");\nmse([1], [3])',
    ],
  },
  'rmse': {
    category: 'Vector',
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
      'let { rmse } = import("Vector");\nrmse([1, 2, 3], [1, 2, 3])',
      'let { rmse } = import("Vector");\nrmse([1, 2, 3], [4, 5, 6])',
      'let { rmse } = import("Vector");\nrmse([1, 2, 3], [2, 2, 2])',
      'let { rmse } = import("Vector");\nrmse([1, 2], [3, 3])',
      'let { rmse } = import("Vector");\nrmse([1], [3])',
    ],
  },
  'mae': {
    category: 'Vector',
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
      'let { mae } = import("Vector");\nmae([1, 2, 3], [1, 2, 3])',
      'let { mae } = import("Vector");\nmae([1, 2, 3], [4, 5, 6])',
      'let { mae } = import("Vector");\nmae([1, 2, 3], [2, 2, 2])',
      'let { mae } = import("Vector");\nmae([1, 2], [3, 3])',
      'let { mae } = import("Vector");\nmae([1], [3])',
    ],
  },
  'smape': {
    category: 'Vector',
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
      'let { smape } = import("Vector");\nsmape([1, 2, 3], [1, 2, 3])',
      'let { smape } = import("Vector");\nsmape([1, 2, 3], [4, 5, 6])',
      'let { smape } = import("Vector");\nsmape([1, 2, 3], [2, 2, 2])',
      'let { smape } = import("Vector");\nsmape([1, 2], [3, 3])',
      'let { smape } = import("Vector");\nsmape([1], [3])',
    ],
  },
}
