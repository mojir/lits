import { FunctionArguments } from '../builtin/specialExpressions/utils'
import { ReservedName } from '../reservedNames'
import { Token } from '../tokenizer/interface'

export const functionSymbol = Symbol('function')
export type UserDefinedLispishFunction = {
  [functionSymbol]: true
  name: string | undefined
  arguments: FunctionArguments
  body: AstNode[]
}

export type BuiltinLispishFunction = {
  [functionSymbol]: true
  builtin: string
}

export type LispishFunction = UserDefinedLispishFunction | BuiltinLispishFunction

type NodeType =
  | 'Number'
  | 'String'
  | 'NormalExpression'
  | 'SpecialExpression'
  | 'ExpressionExpression'
  | 'Name'
  | 'Modifier'
  | 'ReservedName'
  | 'Binding'
  | 'Argument'
type SpecialExpressionName =
  | 'let'
  | 'if'
  | 'setq'
  | 'and'
  | 'or'
  | 'cond'
  | 'defun'
  | 'function'
  | 'lambda'
  | 'return-from'
  | 'return'
  | 'block'
export type ModifierName = '&rest' | '&optional'

interface GenericNode {
  type: NodeType
}

type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | ExpressionExpressionNode
export type ParseBinding = (tokens: Token[], position: number) => [number, BindingNode]
export type ParseArgument = (tokens: Token[], position: number) => [number, ArgumentNode | ModifierNode]
export type ParseExpression = (tokens: Token[], position: number) => [number, ExpressionNode]
export type ParseNormalExpression = (tokens: Token[], position: number) => [number, NormalExpressionNode]
export type ParseSpecialExpression = (tokens: Token[], position: number) => [number, SpecialExpressionNode]
export type ParseExpressionExpression = (tokens: Token[], position: number) => [number, ExpressionExpressionNode]
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
export interface ModifierNode extends GenericNode {
  type: 'Modifier'
  value: ModifierName
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

export interface BindingNode extends GenericNode {
  type: 'Binding'
  name: string
  value: AstNode
}

export interface ArgumentNode extends GenericNode {
  type: 'Argument'
  name: string
  defaultValue?: AstNode
}

export interface ExpressionExpressionNode extends GenericNode {
  type: 'ExpressionExpression'
  expression: ExpressionNode
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
  | ExpressionExpressionNode
  | ModifierNode

export type Ast = {
  type: 'Program'
  body: AstNode[]
}
