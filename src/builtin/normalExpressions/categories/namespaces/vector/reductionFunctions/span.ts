import type { ReductionFunctionDefinition } from '.'

export const spanReductionFunction: ReductionFunctionDefinition<'span'> = {
  'vec:span': vector => vector.length === 0 ? 0 : Math.max(...vector) - Math.min(...vector),
  'minLength': 0,
}
