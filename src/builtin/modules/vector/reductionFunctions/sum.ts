import type { ReductionFunctionDefinition } from '.'

export const sumReductionFunction: ReductionFunctionDefinition<'sum'> = {
  sum: vector => vector.reduce((acc, val) => acc + val, 0),
  minLength: 0,
}
