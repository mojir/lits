import type { ReductionFunctionDefinition } from '.'

export const maxReductionFunction: ReductionFunctionDefinition<'max'> = {
  'vec:max': vector => Math.max(...vector),
  'minLength': 1,
  'paddingValue': -Number.MAX_VALUE,
}
