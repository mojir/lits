import { calcMad } from '../calcMad'
import type { ReductionFunctionDefinition } from '.'

export const madReductionFunction: ReductionFunctionDefinition<'mad'> = {
  'vec:mad': vector => calcMad(vector),
}
