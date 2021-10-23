import { SpecialExpressionName } from '../builtin/interface'
import { Context } from '../evaluator/interface'
import { Any, Arr } from '../interface'
import { ReservedName } from '../reservedNames'
import { Token } from '../tokenizer/interface'

export const functionSymbol = Symbol(`function`)

export type EvaluatedFunctionArguments = {
  mandatoryArguments: string[]
  optionalArguments: Array<{
    name: string
    defaultValue?: Any
  }>
  restArgument?: string
}

export type UserDefinedFunction = {
  [functionSymbol]: true
  type: `user-defined`
  name: string | undefined
  arguments: EvaluatedFunctionArguments
  body: AstNode[]
  functionContext: Context
}

export type PartialFunction = {
  [functionSymbol]: true
  type: `partial`
  fn: Any
  params: Arr
}

export type CompFunction = {
  [functionSymbol]: true
  type: `comp`
  fns: Arr
}

export type ConstantlyFunction = {
  [functionSymbol]: true
  type: `constantly`
  value: Any
}

export type JuxtFunction = {
  [functionSymbol]: true
  type: `juxt`
  fns: Arr
}

export type ComplementFunction = {
  [functionSymbol]: true
  type: `complement`
  fn: Any
}

export type EveryPredFunction = {
  [functionSymbol]: true
  type: `every-pred`
  fns: Arr
}

export type SomePredFunction = {
  [functionSymbol]: true
  type: `some-pred`
  fns: Arr
}

export type FNilFunction = {
  [functionSymbol]: true
  type: `fnil`
  fn: Any
  params: Arr
}

export type BuiltinFunction = {
  [functionSymbol]: true
  type: `builtin`
  name: string
}

export type LispishFunction =
  | UserDefinedFunction
  | BuiltinFunction
  | PartialFunction
  | CompFunction
  | ConstantlyFunction
  | JuxtFunction
  | ComplementFunction
  | EveryPredFunction
  | SomePredFunction
  | FNilFunction

export type LispishFunctionType = LispishFunction[`type`]

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
  | `Partial`

export type ModifierName = `&rest` | `&opt` | `&let` | `&when` | `&while`

interface GenericNode {
  type: NodeType
}

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode
export type ParseBinding = (tokens: Token[], position: number) => [number, BindingNode]
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
