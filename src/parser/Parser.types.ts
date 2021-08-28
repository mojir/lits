export type NodeType = 'Number' | 'String' | 'BasicExpression' | 'LetExpression' | 'Name'

interface GenericNode {
  type: NodeType
}

export interface NumberNode extends GenericNode {
  type: 'Number'
  value: number
}
export interface StringNode extends GenericNode {
  type: 'String'
  value: string
}
export interface NameNode extends GenericNode {
  type: 'Name'
  value: string
}
export interface BasicExpressionNode extends GenericNode {
  type: 'BasicExpression'
  name: string
  params: AstNode[]
}

export interface LetExpressionNode extends GenericNode {
  type: 'LetExpression'
  params: AstNode[]
  bindings: BasicExpressionNode[]
}

export type AstNode = NumberNode | StringNode | NameNode | BasicExpressionNode | LetExpressionNode

export type Ast = {
  type: 'Program'
  body: AstNode[]
}
