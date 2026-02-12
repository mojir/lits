import { calcSampleStdDev, calcStdDev } from '../calcStdDev'
import type { ReductionFunctionDefinition } from '.'

export const stdevReductionFunction: ReductionFunctionDefinition<'stdev'> = {
  'stdev': vector => calcStdDev(vector),
}

export const sampleStdevReductionFunction: ReductionFunctionDefinition<'sample-stdev'> = {
  'sample-stdev': vector => calcSampleStdDev(vector),
  'minLength': 2,
}
