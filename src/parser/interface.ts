import type { SpecialExpressionName } from '../builtin/interface'
import type { Condition } from '../builtin/specialExpressions/cond'
import type { LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { Arity, FunctionOverload } from '../builtin/utils'
import type { Context } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type { ReservedName } from '../reservedNames'
import type { SourceCodeInfo, Token, TokenStream } from '../tokenizer/interface'
import type { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'
import type { AstNodeType, FunctionType } from '../constants/constants'
import type { JsFunction, LazyValue } from '../Lits/Lits'

export interface EvaluatedFunctionArguments {
  mandatoryArguments: string[]
  restArgument?: string
}

export interface EvaluatedFunctionOverload {
  as: EvaluatedFunctionArguments
  b: AstNode[]
  a: Arity
  f: Context
}

export type ExtraData = Record<string, LazyValue>

interface GenericLitsFunction {
  [FUNCTION_SYMBOL]: true
  sourceCodeInfo?: SourceCodeInfo
  t: FunctionType
}

export interface RegularExpression {
  [REGEXP_SYMBOL]: true
  sourceCodeInfo?: SourceCodeInfo
  s: string
  f: string
}

export interface NativeJsFunction extends GenericLitsFunction {
  t: FunctionType.NativeJsFunction
  n: string | undefined // name
  f: JsFunction
}

export interface UserDefinedFunction extends GenericLitsFunction {
  t: FunctionType.UserDefined
  n: string | undefined // name
  o: EvaluatedFunctionOverload[]
  x?: ExtraData
}

export interface PartialFunction extends GenericLitsFunction {
  t: FunctionType.Partial
  f: Any
  p: Arr
}

export interface CompFunction extends GenericLitsFunction {
  t: FunctionType.Comp
  f: Arr
}

export interface ConstantlyFunction extends GenericLitsFunction {
  t: FunctionType.Constantly
  v: Any
}

export interface JuxtFunction extends GenericLitsFunction {
  t: FunctionType.Juxt
  f: Arr
}

export interface ComplementFunction extends GenericLitsFunction {
  t: FunctionType.Complement
  f: Any
}

export interface EveryPredFunction extends GenericLitsFunction {
  t: FunctionType.EveryPred
  f: Arr
}

export interface SomePredFunction extends GenericLitsFunction {
  t: FunctionType.SomePred
  f: Arr
}

export interface FNilFunction extends GenericLitsFunction {
  t: FunctionType.Fnil
  f: Any
  p: Arr
}

export interface BuiltinFunction extends GenericLitsFunction {
  t: FunctionType.Builtin
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
  | FNilFunction

export type LitsFunctionType = LitsFunction['t']

export type ModifierName = '&' | '&let' | '&when' | '&while'

interface GenericNode {
  t: AstNodeType // type
  tkn?: Token
}

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode
export type ParseBinding = (tokens: TokenStream, position: number) => [number, BindingNode]
export type ParseBindings = (tokens: TokenStream, position: number) => [number, BindingNode[]]
export type ParseArgument = (tokens: TokenStream, position: number) => [number, ArgumentNode | ModifierNode]
export type ParseExpression = (tokens: TokenStream, position: number) => [number, ExpressionNode]
export type ParseTokens = (tokens: TokenStream, position: number) => [number, AstNode[]]
export type ParseToken = (tokens: TokenStream, position: number) => [number, AstNode]

export interface NumberNode extends GenericNode {
  t: AstNodeType.Number // type
  v: number // value
}
export interface StringNode extends GenericNode {
  t: AstNodeType.String // type
  v: string // value
}
export interface NameNode extends GenericNode {
  t: AstNodeType.Name // type
  v: string // value
}
export interface ModifierNode extends GenericNode {
  t: AstNodeType.Modifier // type
  v: ModifierName
}
export interface ReservedNameNode extends GenericNode {
  t: AstNodeType.ReservedName // type
  v: ReservedName // reservedName
}

interface NormalExpressionNodeBase extends GenericNode {
  t: AstNodeType.NormalExpression // type
  p: AstNode[] // params
}

export interface NormalExpressionNodeWithName extends NormalExpressionNodeBase {
  n: string // name
  e?: ExpressionNode // expressionNode
}

interface NormalExpressionNodeExpression extends NormalExpressionNodeBase {
  n?: never // name
  e: ExpressionNode // expressionNode
}

export type NormalExpressionNode = NormalExpressionNodeWithName | NormalExpressionNodeExpression

export interface BindingNode extends GenericNode {
  t: AstNodeType.Binding // type
  n: string // name
  v: AstNode // value
}

export interface ArgumentNode extends GenericNode {
  t: AstNodeType.Argument // type
  n: string // name
  d?: AstNode // defaultValue
}

export interface SpecialExpressionNode extends GenericNode {
  t: AstNodeType.SpecialExpression // type
  n: SpecialExpressionName // name
  p: AstNode[] // params
  b?: BindingNode // binding
  bs?: BindingNode[] // bindings
  c?: Condition[] // conditions
  f?: AstNode // functionName
  o?: FunctionOverload[] // overloads
  l?: LoopBindingNode[] // loopBindings
  m?: AstNode // messageNode
  te?: AstNode // tryExpression
  e?: NameNode // error
  ce?: AstNode // catchExpression
}

export type AstNode =
  | NumberNode
  | StringNode
  | ReservedNameNode
  | NameNode
  | NormalExpressionNode
  | ModifierNode
  | SpecialExpressionNode

type AstBody = AstNode[]
export interface Ast {
  b: AstBody // body
}
