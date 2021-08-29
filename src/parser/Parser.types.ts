export type NodeType = 'Number' | 'String' | 'NormalExpression' | 'SpecialExpression' | 'Name'

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
export interface NormalExpressionNode extends GenericNode {
  type: 'NormalExpression'
  name: string
  params: AstNode[]
}

export interface SpecialExpressionNode extends GenericNode {
  type: 'SpecialExpression'
  params: AstNode[]
  bindings: NormalExpressionNode[]
}

export type AstNode = NumberNode | StringNode | NameNode | NormalExpressionNode | SpecialExpressionNode

export type Ast = {
  type: 'Program'
  body: AstNode[]
}
