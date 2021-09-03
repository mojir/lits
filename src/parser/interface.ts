import { Token } from '..'
import { ReservedName } from '../reservedNames'

type NodeType = 'Number' | 'String' | 'NormalExpression' | 'SpecialExpression' | 'Name' | 'ReservedName'
type SpecialExpressionName = 'let' | 'if' | 'setq' | 'and' | 'or' | 'cond'

interface GenericNode {
  type: NodeType
}

type ExpressionNode = NormalExpressionNode | SpecialExpressionNode
export type ParseExpression = (tokens: Token[], position: number) => [number, ExpressionNode]
export type ParseParams = (tokens: Token[], position: number) => [number, AstNode[]]
export type ParseToken = (tokens: Token[], position: number) => [number, AstNode]

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
