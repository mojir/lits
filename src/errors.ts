/* istanbul ignore file */

import { AstNode, NodeType } from './parser/interface'
import { Token } from './tokenizer/interface'

export class RecurSignal extends Error {
  public params: unknown[]
  constructor(params: unknown[]) {
    super(`recur, params: ${params}`)
    Object.setPrototypeOf(this, RecurSignal.prototype)
    this.name = `RecurSignal`
    this.params = params
  }
}

export class UserDefinedError extends Error {
  constructor(message: string) {
    super(message)
    Object.setPrototypeOf(this, UserDefinedError.prototype)
    this.name = `UserDefinedError`
  }
}

export class UnexpectedTokenError extends Error {
  constructor(expectedToken: string, actualToken: Token) {
    super(`Expected a "${expectedToken}" token, got Token[${actualToken.type}:"${actualToken.value}"]`)
    Object.setPrototypeOf(this, UnexpectedTokenError.prototype)
    this.name = `UnexpectedTokenError`
  }
}

export class UnexpectedNodeTypeError extends Error {
  constructor(expectedNodeType: NodeType | `ExpressionNode`, actualNode: AstNode | undefined) {
    super(`Expected a ${expectedNodeType} node, got ${actualNode ? `a ${actualNode.type} node` : `undefined`}`)
    Object.setPrototypeOf(this, UnexpectedNodeTypeError.prototype)
    this.name = `UnexpectedNodeTypeError`
  }
}
