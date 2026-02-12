import { getOperatorArgs } from '../../api'
import type { VectorReductionReference } from '.'

export const rootMeanSquareReference: VectorReductionReference<'rms'> = {
  'vec.rms': {
    title: 'vec.rms',
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
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let vec = import("vec");\nvec.rms([1, 2, 3, 4])',
      'let vec = import("vec");\nvec.rms([5, 4, 3, 2, 1])',
      'let vec = import("vec");\nvec.rms(range(1, 1000))',
      'let vec = import("vec");\nvec.rms(vec.generate(1000, -> 1e6 / ($ + 1) ^ 2))',
      'let vec = import("vec");\nvec.rms(vec.generate(1000, -> ln($ + 1)))',
    ],
  },
  'vec.moving-rms': {
    title: 'vec.moving-rms',
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
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
    ],
    examples: [
      'let vec = import("vec");\nvec.moving-rms([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.moving-rms([1, 2, 4, 7, 11, 16], 5)',
      'let vec = import("vec");\nvec.moving-rms([1, 2, 4, 7, 11, 16], 6)',
    ],
  },
  'vec.centered-moving-rms': {
    title: 'vec.centered-moving-rms',
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
      ...getOperatorArgs('vector', 'integer'),
    },
    variants: [
      { argumentNames: ['vector', 'windowSize'] },
      { argumentNames: ['vector', 'windowSize', 'leftPadding'] },
      { argumentNames: ['vector', 'windowSize', 'leftPadding', 'rightPadding'] },
    ],
    examples: [
      'let vec = import("vec");\nvec.centered-moving-rms([1, 2, 4, 7, 11, 16], 4)',
      'let vec = import("vec");\nvec.centered-moving-rms([1, 2, 4, 7, 11, 16], 5, 0)',
      'let vec = import("vec");\nvec.centered-moving-rms([1, 2, 4, 7, 11, 16], 6, 0, 0)',
    ],
  },
  'vec.running-rms': {
    title: 'vec.running-rms',
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
      { argumentNames: ['vector'] },
    ],
    examples: [
      'let vec = import("vec");\nvec.running-rms([1, 2, 3, 4, 5, 6])',
      'let vec = import("vec");\nvec.running-rms([1, -3, 2])',
      'let vec = import("vec");\nvec.running-rms([-1, -2, -3])',
      'let vec = import("vec");\nvec.running-rms([0])',
    ],
  },
}
