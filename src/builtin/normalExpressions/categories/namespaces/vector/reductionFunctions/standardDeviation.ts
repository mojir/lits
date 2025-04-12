import { calcSampleStdDev, calcStdDev } from '../calcStdDev'
import type { ReductionFunctionDefinition } from '.'

export const stdevReductionFunction: ReductionFunctionDefinition<'stdev'> = {
  'vec:stdev': vector => calcStdDev(vector),
}

export const sampleStdevReductionFunction: ReductionFunctionDefinition<'sample-stdev'> = {
  'vec:sample-stdev': vector => calcSampleStdDev(vector),
  'minLength': 2,
}
