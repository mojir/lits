import { RecurSignal, UnexpectedNodeTypeError, UnexpectedTokenError, UserDefinedError } from '../src/errors'
import { AstNode } from '../src/parser/interface'
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
  test(`UnexpectedNodeTypeError with node`, () => {
    const node: AstNode = {
      type: `NormalExpression`,
      name: `+`,
      params: [],
      token: {
        type: `name`,
        sourceCodeInfo: { line: 1, column: 1, toString: () => `(1:1)`, sourceCodeLine: null },
        value: `X`,
      },
    }
    const err = new UnexpectedNodeTypeError(`Name`, node, node.token.sourceCodeInfo)
    expect(err).toBeInstanceOf(UnexpectedNodeTypeError)
    expect(err.name).toBe(`UnexpectedNodeTypeError`)
    expect(err.message).toBe(`Expected a Name node, got a NormalExpression node (1:1)`)
  })

  test(`UnexpectedNodeTypeError with undefined`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const err = new UnexpectedNodeTypeError(`Name`, undefined, {
      line: 1,
      column: 1,
      toString: () => `(1:1)`,
      sourceCodeLine: null,
    })
    expect(err).toBeInstanceOf(UnexpectedNodeTypeError)
    expect(err.name).toBe(`UnexpectedNodeTypeError`)
    expect(err.message).toBe(`Expected a Name node, got undefined (1:1)`)
  })
})
