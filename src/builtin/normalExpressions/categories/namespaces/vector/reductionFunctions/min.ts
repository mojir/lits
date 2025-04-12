import type { ReductionFunctionDefinition } from '.'

export const minReductionFunction: ReductionFunctionDefinition<'min'> = {
  'vec:min': vector => Math.min(...vector),
  'padding': Number.MAX_VALUE,
}
