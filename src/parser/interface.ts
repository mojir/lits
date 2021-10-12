import { SpecialExpressionName } from '../builtin/interface'
import { Context } from '../evaluator/interface'
import { ReservedName } from '../reservedNames'
import { Token } from '../tokenizer/interface'

export const functionSymbol = Symbol(`function`)

export type EvaluatedFunctionArguments = {
  mandatoryArguments: string[]
  optionalArguments: Array<{
    name: string
    defaultValue?: unknown
  }>
  restArgument?: string
}

export type UserDefinedLispishFunction = {
  [functionSymbol]: true
  name: string | undefined
  arguments: EvaluatedFunctionArguments
  body: AstNode[]
  functionContext: Context
}

export type BuiltinLispishFunction = {
  [functionSymbol]: true
  builtin: string
}

export type LispishFunction = UserDefinedLispishFunction | BuiltinLispishFunction

export type NodeType =
  | `Number`
  | `String`
  | `NormalExpression`
  | `SpecialExpression`
  | `Name`
  | `Modifier`
  | `ReservedName`
  | `Binding`
  | `Argument`

export type ModifierName = `&rest` | `&opt` | `&bind`

interface GenericNode {
  type: NodeType
}

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode
export type ParseBindings = (tokens: Token[], position: number) => [number, BindingNode[]]
export type ParseArgument = (tokens: Token[], position: number) => [number, ArgumentNode | ModifierNode]
export type ParseExpression = (tokens: Token[], position: number) => [number, ExpressionNode]
export type ParseNormalExpression = (tokens: Token[], position: number) => [number, NormalExpressionNode]
export type ParseSpecialExpression = (tokens: Token[], position: number) => [number, SpecialExpressionNode]
export type ParseTokens = (tokens: Token[], position: number) => [number, AstNode[]]
export type ParseToken = (tokens: Token[], position: number) => [number, AstNode]

export interface NumberNode extends GenericNode {
  type: `Number`
  value: number
}
export interface StringNode extends GenericNode {
  type: `String`
  value: string
}
export interface NameNode extends GenericNode {
  type: `Name`
  value: string
}
export interface ModifierNode extends GenericNode {
  type: `Modifier`
  value: ModifierName
}
export interface ReservedNameNode extends GenericNode {
  type: `ReservedName`
  value: ReservedName
}

interface NormalExpressionNodeBase extends GenericNode {
  type: `NormalExpression`
  params: AstNode[]
}

export interface NormalExpressionNodeName extends NormalExpressionNodeBase {
  name: string
  expression?: ExpressionNode
}

interface NormalExpressionNodeExpression extends NormalExpressionNodeBase {
  name?: never
  expression: ExpressionNode
}

export type NormalExpressionNode = NormalExpressionNodeName | NormalExpressionNodeExpression

export interface BindingNode extends GenericNode {
  type: `Binding`
  name: string
  value: AstNode
}

export interface ArgumentNode extends GenericNode {
  type: `Argument`
  name: string
  defaultValue?: AstNode
}

export interface SpecialExpressionNode extends GenericNode {
  type: `SpecialExpression`
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
  | ModifierNode

export type Ast = {
  type: `Program`
  body: AstNode[]
}
