import { quartiles } from '../quartiles'
import type { ReductionFunctionDefinition } from '.'

export const iqrReductionFunction: ReductionFunctionDefinition<'iqr'> = {
  'iqr': (vector) => {
    const [q1, , q3] = quartiles(vector)
    return q3 - q1
  },
  'minLength': 4,
}
