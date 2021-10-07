import { builtin } from '../../src/builtin'
import { assertNameNotDefined } from '../../src/builtin/utils'
import { Context } from '../../src/evaluator/interface'

const contextStack: Context[] = [{ a: { value: 1 } }, {}]

describe(`builtin utils`, () => {
  test(`assertNameNotDefined`, () => {
    expect(() => assertNameNotDefined(undefined, contextStack, builtin)).not.toThrow()
    expect(() => assertNameNotDefined(`b`, contextStack, builtin)).not.toThrow()
    expect(() => assertNameNotDefined(`a`, contextStack, builtin)).toThrow()
    expect(() => assertNameNotDefined(`true`, contextStack, builtin)).toThrow()
    expect(() => assertNameNotDefined(`do`, contextStack, builtin)).toThrow()
    expect(() => assertNameNotDefined(`+`, contextStack, builtin)).toThrow()
  })
})
