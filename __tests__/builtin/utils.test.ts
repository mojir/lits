import { builtin } from '../../src/builtin'
import { assertNameNotDefined } from '../../src/builtin/utils'
import { createContextStack } from '../../src/evaluator'
import { ContextStack } from '../../src/evaluator/interface'
import { SourceCodeInfo } from '../../src/tokenizer/interface'

const contextStack: ContextStack = createContextStack([{ a: { value: 1 } }, {}])

describe(`builtin utils`, () => {
  test(`assertNameNotDefined`, () => {
    const sourceCodeInfo: SourceCodeInfo = { line: 1, column: 1, toString: () => `(1:1)`, sourceCodeLine: null }
    expect(() => assertNameNotDefined(undefined, contextStack, builtin, sourceCodeInfo)).not.toThrow()
    expect(() => assertNameNotDefined(`b`, contextStack, builtin, sourceCodeInfo)).not.toThrow()
    expect(() => assertNameNotDefined(`a`, contextStack, builtin, sourceCodeInfo)).toThrow()
    expect(() => assertNameNotDefined(`true`, contextStack, builtin, sourceCodeInfo)).toThrow()
    expect(() => assertNameNotDefined(`do`, contextStack, builtin, sourceCodeInfo)).toThrow()
    expect(() => assertNameNotDefined(`+`, contextStack, builtin, sourceCodeInfo)).toThrow()
  })
})
