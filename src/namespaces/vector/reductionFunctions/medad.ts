import { calcMedad } from '../calcMedad'
import type { ReductionFunctionDefinition } from '.'

export const medadReductionFunction: ReductionFunctionDefinition<'medad'> = {
  'medad': vector => calcMedad(vector),
}
