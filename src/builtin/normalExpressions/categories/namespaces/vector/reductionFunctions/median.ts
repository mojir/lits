import { calcMedian } from '../calcMedian'
import type { ReductionFunctionDefinition } from '.'

export const medianReductionFunction: ReductionFunctionDefinition<'median'> = {
  'vec:median': vector => calcMedian(vector),
  'minLength': 1,
  'paddingValue': 0,
}
