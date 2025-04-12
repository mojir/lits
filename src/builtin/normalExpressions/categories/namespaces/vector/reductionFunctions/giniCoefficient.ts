import type { ReductionFunctionDefinition } from '.'

export const giniCoefficientReductionFunction: ReductionFunctionDefinition<'gini-coefficient'> = {
  'vec:gini-coefficient': (vector) => {
    if (vector.some(x => x < 0)) {
      throw new Error('Gini coefficient is not defined for negative values')
    }
    const sorted = [...vector].sort((a, b) => a - b)
    const n = sorted.length
    const sum = sorted.reduce((acc, val) => acc + val, 0)
    if (sum === 0) {
      return 0
    }
    const gini = (2 * sorted.reduce((acc, val, i) => acc + (i + 1) * val, 0)) / (n * sum) - (n + 1) / n
    return gini
  },
  'minLength': 1,
}
