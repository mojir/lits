import { builtin } from '../../src/builtin'
import { assertNameNotDefined } from '../../src/builtin/utils'
import { createContextStack } from '../../src/evaluator'
import { ContextStack } from '../../src/evaluator/interface'
import { TokenMeta } from '../../src/tokenizer/interface'

const contextStack: ContextStack = createContextStack([{ a: { value: 1 } }, {}])

describe(`builtin utils`, () => {
  test(`assertNameNotDefined`, () => {
    const meta: TokenMeta = { line: 1, column: 1, toString: () => `(1:1)` }
    expect(() => assertNameNotDefined(undefined, contextStack, builtin, meta)).not.toThrow()
    expect(() => assertNameNotDefined(`b`, contextStack, builtin, meta)).not.toThrow()
    expect(() => assertNameNotDefined(`a`, contextStack, builtin, meta)).toThrow()
    expect(() => assertNameNotDefined(`true`, contextStack, builtin, meta)).toThrow()
    expect(() => assertNameNotDefined(`do`, contextStack, builtin, meta)).toThrow()
    expect(() => assertNameNotDefined(`+`, contextStack, builtin, meta)).toThrow()
  })
})
