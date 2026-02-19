import type { ReductionFunctionDefinition } from '.'

export const maxReductionFunction: ReductionFunctionDefinition<'max'> = {
  max: vector => Math.max(...vector),
  padding: -Number.MAX_VALUE,
}
