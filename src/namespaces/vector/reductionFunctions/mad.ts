import { calcMad } from '../calcMad'
import type { ReductionFunctionDefinition } from '.'

export const madReductionFunction: ReductionFunctionDefinition<'mad'> = {
  'mad': vector => calcMad(vector),
}
