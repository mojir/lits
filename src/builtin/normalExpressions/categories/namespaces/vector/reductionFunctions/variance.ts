import { calcMean } from '../calcMean'
import type { ReductionFunctionDefinition } from '.'

export const varianceReductionFunction: ReductionFunctionDefinition<'variance'> = {
  'vec:variance': (vector) => {
    const mean = calcMean(vector)
    return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length
  },
  'minLength': 1,
  'paddingValue': 0,
}
