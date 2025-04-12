import { calculateEntropy } from '../entropy'
import type { ReductionFunctionDefinition } from '.'

export const entropyReductionFunction: ReductionFunctionDefinition<'entropy'> = {
  'vec:entropy': vector => calculateEntropy(vector),
  'minLength': 1,
}
