import { describe, expect, it } from 'vitest'
import { builtin } from '../../src/builtin'
import { assertNameNotDefined } from '../../src/builtin/utils'
import type { ContextStack } from '../../src/evaluator/ContextStack'
import { createContextStackWithGlobalContext } from '../testUtils'
import { LitsError } from '../../src/errors'

const contextStack: ContextStack = createContextStackWithGlobalContext({ a: { value: 1 } })

describe('builtin utils', () => {
  it('assertNameNotDefined', () => {
    expect(() => assertNameNotDefined(undefined, contextStack, builtin)).not.toThrow()
    expect(() => assertNameNotDefined('b', contextStack, builtin)).not.toThrow()
    expect(() => assertNameNotDefined('a', contextStack, builtin)).toThrow(LitsError)
    expect(() => assertNameNotDefined('true', contextStack, builtin)).toThrow(LitsError)
    expect(() => assertNameNotDefined('do', contextStack, builtin)).toThrow(LitsError)
    expect(() => assertNameNotDefined('+', contextStack, builtin)).toThrow(LitsError)
  })
})
