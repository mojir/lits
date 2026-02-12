import { calcSampleVariance, calcVariance } from '../calcVariance'
import type { ReductionFunctionDefinition } from '.'

export const varianceReductionFunction: ReductionFunctionDefinition<'variance'> = {
  'variance': vector => calcVariance(vector),
}

export const sampleVarianceReductionFunction: ReductionFunctionDefinition<'sample-variance'> = {
  'sample-variance': vector => calcSampleVariance(vector),
  'minLength': 2,
}
