import type { ReductionFunctionDefinition } from '.'

export const minReductionFunction: ReductionFunctionDefinition<'TEMP-min'> = {
  'TEMP-min': vector => Math.min(...vector),
  'padding': Number.MAX_VALUE,
}
