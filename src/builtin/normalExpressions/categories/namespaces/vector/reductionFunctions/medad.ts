import { calcMedad } from '../calcMedad'
import type { ReductionFunctionDefinition } from '.'

export const medadReductionFunction: ReductionFunctionDefinition<'medad'> = {
  'vec:medad': vector => calcMedad(vector),
}
