import type { ReductionFunctionDefinition } from '.'

export const rmsReductionFunction: ReductionFunctionDefinition<'rms'> = {
  'vec:rms': vector => Math.sqrt(vector.reduce((acc, val) => acc + val ** 2, 0) / vector.length),
}
