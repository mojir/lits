import { calcMedian } from '../calcMedian'
import type { ReductionFunctionDefinition } from '.'

export const medianReductionFunction: ReductionFunctionDefinition<'median'> = {
  'median': vector => calcMedian(vector),
}
