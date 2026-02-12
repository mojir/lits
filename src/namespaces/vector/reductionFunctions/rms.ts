import type { ReductionFunctionDefinition } from '.'

export const rmsReductionFunction: ReductionFunctionDefinition<'rms'> = {
  'rms': vector => Math.sqrt(vector.reduce((acc, val) => acc + val ** 2, 0) / vector.length),
}
