import type { FunctionReference } from '..'
import type { MatrixApiName } from '../api'

export const matrixReference: Record<MatrixApiName, FunctionReference<'Matrix'>> = {
  'm:matrix?': {
    title: 'matrix?',
    category: 'Matrix',
    linkName: 'matrix-question',
    returns: {
      type: 'boolean',
    },
    args: {
      value: {
        type: 'any',
      },
    },
    variants: [
      { argumentNames: ['value'] },
    ],
    description: 'Returns true of $value is a matrix.',
    examples: [
      'matrix?([[1, 2], [3, 4]])',
      'matrix?([[1, 2], [3, 4], [5, "A string"]])',
      'matrix?([[1, 2], [3]])',
      'matrix?([1, 2])',
      'matrix?([[]])',
    ],
  },
}
