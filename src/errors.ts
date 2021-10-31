/* istanbul ignore file */

import { Arr } from './interface'
import { AstNode, NodeType } from './parser/interface'
import { Token, TokenMeta } from './tokenizer/interface'

export class RecurSignal extends Error {
  public params: Arr
  constructor(params: Arr) {
    super(`recur, params: ${params}`)
    Object.setPrototypeOf(this, RecurSignal.prototype)
    this.name = `RecurSignal`
    this.params = params
  }
}

export class LitsError extends Error {
  public line: number | null
  public column: number | null
  constructor(message: string, meta: TokenMeta) {
    super(`${message} ${meta}`)
    this.line = meta === `EOF` ? null : meta.line
    this.column = meta === `EOF` ? null : meta.column
    Object.setPrototypeOf(this, LitsError.prototype)
    this.name = `LitsError`
  }
}

export class UserDefinedError extends Error {
  public line: number | null
  public column: number | null
  constructor(message: string, meta: TokenMeta) {
    super(`${message} ${meta}`)
    this.line = meta === `EOF` ? null : meta.line
    this.column = meta === `EOF` ? null : meta.column
    Object.setPrototypeOf(this, UserDefinedError.prototype)
    this.name = `UserDefinedError`
  }
}

export class AssertionError extends Error {
  public line: number | null
  public column: number | null
  constructor(message: string, meta: TokenMeta) {
    super(`${message} ${meta}`)
    this.line = meta === `EOF` ? null : meta.line
    this.column = meta === `EOF` ? null : meta.column
    Object.setPrototypeOf(this, AssertionError.prototype)
    this.name = `AssertionError`
  }
}

export class UnexpectedTokenError extends Error {
  public line: number | null
  public column: number | null
  constructor(expectedToken: string, actualToken: Token) {
    super(
      `Expected a "${expectedToken}" token, got Token[${actualToken.type}:"${actualToken.value}"] ${actualToken.meta}`,
    )
    this.line = actualToken.meta === `EOF` ? null : actualToken.meta.line
    this.column = actualToken.meta === `EOF` ? null : actualToken.meta.column
    Object.setPrototypeOf(this, UnexpectedTokenError.prototype)
    this.name = `UnexpectedTokenError`
  }
}

export class UnexpectedNodeTypeError extends Error {
  public line: number | null
  public column: number | null
  constructor(expectedNodeType: NodeType | `ExpressionNode`, actualNode: AstNode | undefined, meta: TokenMeta) {
    super(`Expected a ${expectedNodeType} node, got ${actualNode ? `a ${actualNode.type} node` : `undefined`} ${meta}`)
    this.line = meta === `EOF` ? null : meta.line
    this.column = meta === `EOF` ? null : meta.column
    Object.setPrototypeOf(this, UnexpectedNodeTypeError.prototype)
    this.name = `UnexpectedNodeTypeError`
  }
}
