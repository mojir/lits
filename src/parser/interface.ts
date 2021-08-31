import { ReservedName } from '../reservedNames'

type NodeType = 'Number' | 'String' | 'NormalExpression' | 'SpecialExpression' | 'Name' | 'ReservedName'
type SpecialExpressionName = 'let' | 'if' | 'setq'

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
export interface ReservedNameNode extends GenericNode {
  type: 'ReservedName'
  value: ReservedName
}
export interface NormalExpressionNode extends GenericNode {
  type: 'NormalExpression'
  name: string
  params: AstNode[]
}

export interface SpecialExpressionNode extends GenericNode {
  type: 'SpecialExpression'
  name: SpecialExpressionName
  params: AstNode[]
}

export type AstNode =
  | NumberNode
  | StringNode
  | ReservedNameNode
  | NameNode
  | NormalExpressionNode
  | SpecialExpressionNode

export type Ast = {
  type: 'Program'
  body: AstNode[]
}
