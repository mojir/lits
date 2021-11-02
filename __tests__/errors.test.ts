import { RecurSignal, UnexpectedTokenError, UserDefinedError } from '../src/errors'
import { Token } from '../src/tokenizer/interface'
describe(`errors`, () => {
  test(`RecurSignal`, () => {
    const err = new RecurSignal([100])
    expect(err).toBeInstanceOf(RecurSignal)
    expect(err.name).toBe(`RecurSignal`)
    expect(err.params).toEqual([100])
  })
  test(`UserDefinedError`, () => {
    const err = new UserDefinedError(`A message`, { line: 1, column: 1, toString: () => `(1:1)`, sourceCodeLine: null })
    expect(err).toBeInstanceOf(UserDefinedError)
    expect(err.name).toBe(`UserDefinedError`)
    expect(err.message).toBe(`A message (1:1)`)
  })
  test(`UnexpectedTokenError`, () => {
    const token: Token = {
      type: `name`,
      value: `xxx`,
      sourceCodeInfo: { line: 1, column: 1, toString: () => `(1:1)`, sourceCodeLine: null },
    }
    const err = new UnexpectedTokenError(`)`, token)
    expect(err).toBeInstanceOf(UnexpectedTokenError)
    expect(err.name).toBe(`UnexpectedTokenError`)
    expect(err.message).toBe(`Expected a ')'-token, got 'xxx' (1:1)`)
  })
})
