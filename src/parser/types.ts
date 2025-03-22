import type { JsFunction } from '../Lits/Lits'
import type { SpecialExpressionType } from '../builtin'
import type { FunctionType, NodeType, NodeTypes } from '../constants/constants'
import type { Context } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type { ReservedSymbol } from '../tokenizer/reservedNames'
import type { ModifierName, SourceCodeInfo, Token } from '../tokenizer/token'
import type { TokenStream } from '../tokenizer/tokenize'
import type { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'

export interface ParseState {
  position: number
}

export interface EvaluatedFunction {
  arguments: BindingTarget[]
  body: Node[]
  context: Context
}

interface GenericLitsFunction {
  [FUNCTION_SYMBOL]: true
  sourceCodeInfo?: SourceCodeInfo
  functionType: FunctionType
}

export interface RegularExpression {
  [REGEXP_SYMBOL]: true
  sourceCodeInfo?: SourceCodeInfo
  s: string
  f: string
}

export interface NativeJsFunction extends GenericLitsFunction {
  functionType: 'NativeJsFunction'
  name: string | undefined // name
  nativeFn: JsFunction
}

export interface UserDefinedFunction extends GenericLitsFunction {
  functionType: 'UserDefined'
  name: string | undefined // name
  evaluatedfunction: EvaluatedFunction
}

export interface PartialFunction extends GenericLitsFunction {
  functionType: 'Partial'
  function: Any
  params: Arr
}

export interface CompFunction extends GenericLitsFunction {
  functionType: 'Comp'
  params: Arr
}

export interface ConstantlyFunction extends GenericLitsFunction {
  functionType: 'Constantly'
  value: Any
}

export interface JuxtFunction extends GenericLitsFunction {
  functionType: 'Juxt'
  params: Arr
}

export interface ComplementFunction extends GenericLitsFunction {
  functionType: 'Complement'
  function: Any
}

export interface EveryPredFunction extends GenericLitsFunction {
  functionType: 'EveryPred'
  params: Arr
}

export interface SomePredFunction extends GenericLitsFunction {
  functionType: 'SomePred'
  params: Arr
}

export interface FNullFunction extends GenericLitsFunction {
  functionType: 'Fnull'
  function: Any
  params: Arr
}

export interface BuiltinFunction extends GenericLitsFunction {
  functionType: 'Builtin'
  n: string // name
}

export type LitsFunction =
  | NativeJsFunction
  | UserDefinedFunction
  | BuiltinFunction
  | PartialFunction
  | CompFunction
  | ConstantlyFunction
  | JuxtFunction
  | ComplementFunction
  | EveryPredFunction
  | SomePredFunction
  | FNullFunction

export type LitsFunctionType = LitsFunction['functionType']

export type DebugData = {
  token: Token
  nameToken?: Token
}
export type Node<T extends NodeType = NodeType, Payload = unknown> = [T, Payload] | [T, Payload, SourceCodeInfo]

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode
export type ParseBinding = (tokens: TokenStream, parseState: ParseState) => BindingNode
export type ParseBindings = (tokens: TokenStream, parseState: ParseState) => BindingNode[]
export type ParseArgument = (tokens: TokenStream, parseState: ParseState) => ModifierNode
export type ParseExpression = (tokens: TokenStream, parseState: ParseState) => ExpressionNode
export type ParseTokensUntilClosingBracket = (tokens: TokenStream, parseState: ParseState) => Node[]
export type ParseToken = (tokens: TokenStream, parseState: ParseState) => Node

export type SpreadNode = Node<typeof NodeTypes.Spread, Node> // Payload should be array or object depending on context
export type NumberNode = Node<typeof NodeTypes.Number, number>
export type StringNode = Node<typeof NodeTypes.String, string>
export type SymbolNode = Node<typeof NodeTypes.Symbol, string>
export type ModifierNode = Node<typeof NodeTypes.Modifier, ModifierName>
export type ReservedSymbolNode = Node<typeof NodeTypes.ReservedSymbol, ReservedSymbol>
export type SpecialExpressionNode<T extends [SpecialExpressionType, ...unknown[]] = [SpecialExpressionType, ...unknown[]]> = Node<typeof NodeTypes.SpecialExpression, T> // [name, params]

export type NormalExpressionNodeWithName = Node<typeof NodeTypes.NormalExpression, [string, Node[]]> // [params, name]
export type NormalExpressionNodeExpression = Node<typeof NodeTypes.NormalExpression, [Node, Node[]]> // [name, node as function] node can be string number object or array
export type NormalExpressionNode = NormalExpressionNodeWithName | NormalExpressionNodeExpression
interface CommonBindingTarget {
  sourceCodeInfo: SourceCodeInfo | undefined
  default?: Node
}

export type SymbolBindingTarget = CommonBindingTarget & {
  type: 'symbol'
  name: string
}

export type RestBindingTarget = CommonBindingTarget & {
  type: 'rest'
  name: string
}

export type ObjectBindingTarget = CommonBindingTarget & {
  type: 'object'
  elements: Record<string, BindingTarget>
}

export type ArrayBindingTarget = CommonBindingTarget & {
  type: 'array'
  elements: (BindingTarget | null)[]
}

export type BindingTarget = SymbolBindingTarget | RestBindingTarget | ObjectBindingTarget | ArrayBindingTarget

export type BindingNode = Node<typeof NodeTypes.Binding, [BindingTarget, Node]> // [target, value]

type AstBody = Node[]
export interface Ast {
  body: AstBody // body
  hasDebugData: boolean
}
