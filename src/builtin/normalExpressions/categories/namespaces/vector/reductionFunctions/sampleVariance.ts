import { calcMean } from '../calcMean'
import type { ReductionFunctionDefinition } from '.'

export const sampleVarianceReductionFunction: ReductionFunctionDefinition<'sample-variance'> = {
  'vec:sample-variance': (vector) => {
    const mean = calcMean(vector)
    return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / (vector.length - 1)
  },
  'minLength': 2,
  'paddingValue': 0,
}
