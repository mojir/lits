import { builtin } from '../../src/builtin'
import { assertNameNotDefined } from '../../src/builtin/utils'
import { createContextStack } from '../../src/evaluator'
import { ContextStack } from '../../src/evaluator/interface'
import { DebugInfo } from '../../src/tokenizer/interface'

const contextStack: ContextStack = createContextStack([{ a: { value: 1 } }, {}])

describe(`builtin utils`, () => {
  test(`assertNameNotDefined`, () => {
    const debugInfo: DebugInfo = { line: 1, column: 1, code: `` }
    expect(() => assertNameNotDefined(undefined, contextStack, builtin, debugInfo)).not.toThrow()
    expect(() => assertNameNotDefined(`b`, contextStack, builtin, debugInfo)).not.toThrow()
    expect(() => assertNameNotDefined(`a`, contextStack, builtin, debugInfo)).toThrow()
    expect(() => assertNameNotDefined(`true`, contextStack, builtin, debugInfo)).toThrow()
    expect(() => assertNameNotDefined(`do`, contextStack, builtin, debugInfo)).toThrow()
    expect(() => assertNameNotDefined(`+`, contextStack, builtin, debugInfo)).toThrow()
  })
})
