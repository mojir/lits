/* istanbul ignore file */

import { AstNode, NodeType } from './parser/interface'
import { Token } from './tokenizer/interface'

// Ignoring file, code coverage not working as expected.
// File                          | % Stmts | % Branch | % Funcs | % Lines | Uncovered Line #s
// errors.ts                     |     100 |       50 |     100 |     100 | 5-16

export class ReturnFromSignal extends Error {
  public blockName: string
  public value: unknown
  constructor(blockName: string, value: unknown) {
    super(`return-from block "${blockName}" with value: ${value}`)
    Object.setPrototypeOf(this, ReturnFromSignal.prototype)
    this.name = 'ReturnFromSignal'
    this.blockName = blockName
    this.value = value
  }
}

export class ReturnSignal extends Error {
  public value: unknown
  constructor(value: unknown) {
    super(`return with value: ${value}`)
    Object.setPrototypeOf(this, ReturnSignal.prototype)
    this.name = 'ReturnSignal'
    this.value = value
  }
}

export class UserDefinedError extends Error {
  constructor(message: string) {
    super(message)
    Object.setPrototypeOf(this, UserDefinedError.prototype)
    this.name = 'UserDefinedError'
  }
}

export class UnexpectedTokenError extends Error {
  constructor(expectedToken: string, actualToken: Token) {
    super(`Expected a "${expectedToken}" token, got Token[${actualToken.type}:"${actualToken.value}"]`)
    Object.setPrototypeOf(this, UnexpectedTokenError.prototype)
    this.name = 'UnexpectedTokenError'
  }
}

export class UnexpectedNodeTypeError extends Error {
  constructor(expectedNodeType: NodeType, actualNode: AstNode | undefined) {
    super(`Expected a ${expectedNodeType} node, got ${actualNode ? `a ${actualNode.type} node` : 'undefined'}`)
    Object.setPrototypeOf(this, UnexpectedNodeTypeError.prototype)
    this.name = 'UnexpectedNodeTypeError'
  }
}
