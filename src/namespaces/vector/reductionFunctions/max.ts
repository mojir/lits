import type { ReductionFunctionDefinition } from '.'

export const maxReductionFunction: ReductionFunctionDefinition<'TEMP-max'> = {
  'TEMP-max': vector => Math.max(...vector),
  'padding': -Number.MAX_VALUE,
}
