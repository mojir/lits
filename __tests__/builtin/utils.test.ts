import { assertNameNotDefined } from '../../src/builtin/utils'
import { Context } from '../../src/evaluator/interface'

const contextStack: Context[] = [{ a: { value: 1 } }, {}]

describe(`builtin utils`, () => {
  test(`assertNameNotDefined`, () => {
    expect(() => assertNameNotDefined(undefined, contextStack)).not.toThrow()
    expect(() => assertNameNotDefined(`b`, contextStack)).not.toThrow()
    expect(() => assertNameNotDefined(`a`, contextStack)).toThrow()
    expect(() => assertNameNotDefined(`true`, contextStack)).toThrow()
    expect(() => assertNameNotDefined(`do`, contextStack)).toThrow()
    expect(() => assertNameNotDefined(`+`, contextStack)).toThrow()
  })
})
