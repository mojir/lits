import type { ReductionFunctionDefinition } from '.'

export const prodReductionFunction: ReductionFunctionDefinition<'prod'> = {
  'vec:prod': vector => vector.reduce((acc, val) => acc * val, 1),
  'padding': 1,
  'minLength': 0,
}
