import type { ReductionFunctionDefinition } from '.'

export const minReductionFunction: ReductionFunctionDefinition<'min'> = {
  'vec:min': vector => Math.min(...vector),
  'minLength': 1,
  'paddingValue': Number.MAX_VALUE,
}
