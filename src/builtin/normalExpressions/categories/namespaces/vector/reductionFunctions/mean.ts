import { calcMean } from '../calcMean'
import type { ReductionFunctionDefinition } from '.'

export const meanReductionFunction: ReductionFunctionDefinition<'mean'> = {
  'vec:mean': vector => calcMean(vector),
  'minLength': 1,
  'paddingValue': 0,
}
