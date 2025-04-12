import type { ReductionFunctionDefinition } from '.'

export const maxReductionFunction: ReductionFunctionDefinition<'max'> = {
  'vec:max': vector => Math.max(...vector),
  'padding': -Number.MAX_VALUE,
}
