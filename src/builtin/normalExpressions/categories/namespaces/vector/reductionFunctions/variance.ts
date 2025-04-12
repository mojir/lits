import { calcSampleVariance, calcVariance } from '../calcVariance'
import type { ReductionFunctionDefinition } from '.'

export const varianceReductionFunction: ReductionFunctionDefinition<'variance'> = {
  'vec:variance': vector => calcVariance(vector),
}

export const sampleVarianceReductionFunction: ReductionFunctionDefinition<'sample-variance'> = {
  'vec:sample-variance': vector => calcSampleVariance(vector),
  'minLength': 2,
}
