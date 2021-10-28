import { SpecialExpressionName } from '../builtin/interface'
import { Context } from '../evaluator/interface'
import { Any, Arr } from '../interface'
import { ReservedName } from '../reservedNames'
import { Token } from '../tokenizer/interface'

export const FUNCTION_SYMBOL = Symbol(`function`)

export type EvaluatedFunctionArguments = {
  mandatoryArguments: string[]
  optionalArguments: Array<{
    name: string
    defaultValue?: Any
  }>
  restArgument?: string
}

export type UserDefinedFunction = {
  [FUNCTION_SYMBOL]: true
  type: `user-defined`
  name: string | undefined
  arguments: EvaluatedFunctionArguments
  body: AstNode[]
  functionContext: Context
}

export type PartialFunction = {
  [FUNCTION_SYMBOL]: true
  type: `partial`
  fn: Any
  params: Arr
}

export type CompFunction = {
  [FUNCTION_SYMBOL]: true
  type: `comp`
  fns: Arr
}

export type ConstantlyFunction = {
  [FUNCTION_SYMBOL]: true
  type: `constantly`
  value: Any
}

export type JuxtFunction = {
  [FUNCTION_SYMBOL]: true
  type: `juxt`
  fns: Arr
}

export type ComplementFunction = {
  [FUNCTION_SYMBOL]: true
  type: `complement`
  fn: Any
}

export type EveryPredFunction = {
  [FUNCTION_SYMBOL]: true
  type: `every-pred`
  fns: Arr
}

export type SomePredFunction = {
  [FUNCTION_SYMBOL]: true
  type: `some-pred`
  fns: Arr
}

export type FNilFunction = {
  [FUNCTION_SYMBOL]: true
  type: `fnil`
  fn: Any
  params: Arr
}

export type BuiltinFunction = {
  [FUNCTION_SYMBOL]: true
  type: `builtin`
  name: string
}

export type LitsFunction =
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

export type LitsFunctionType = LitsFunction[`type`]

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
