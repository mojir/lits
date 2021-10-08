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
    const err = new UserDefinedError(`A message`)
    expect(err).toBeInstanceOf(UserDefinedError)
    expect(err.name).toBe(`UserDefinedError`)
    expect(err.message).toBe(`A message`)
  })
  test(`UnexpectedTokenError`, () => {
    const token: Token = {
      type: `name`,
      value: `xxx`,
    }
    const err = new UnexpectedTokenError(`)`, token)
    expect(err).toBeInstanceOf(UnexpectedTokenError)
    expect(err.name).toBe(`UnexpectedTokenError`)
    expect(err.message).toBe(`Expected a ")" token, got Token[name:"xxx"]`)
  })
  test(`UnexpectedNodeTypeError with node`, () => {
    const node: AstNode = {
      type: `NormalExpression`,
      name: `+`,
      params: [],
    }
    const err = new UnexpectedNodeTypeError(`Name`, node)
    expect(err).toBeInstanceOf(UnexpectedNodeTypeError)
    expect(err.name).toBe(`UnexpectedNodeTypeError`)
    expect(err.message).toBe(`Expected a Name node, got a NormalExpression node`)
  })

  test(`UnexpectedNodeTypeError with undefined`, () => {
    const err = new UnexpectedNodeTypeError(`Name`, undefined)
    expect(err).toBeInstanceOf(UnexpectedNodeTypeError)
    expect(err.name).toBe(`UnexpectedNodeTypeError`)
    expect(err.message).toBe(`Expected a Name node, got undefined`)
  })
})
