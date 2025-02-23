import type { JsFunction, LazyValue } from '../Lits/Lits'
import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import type { Arity } from '../builtin/utils'
import type { AstNodeType, FunctionType } from '../constants/constants'
import type { Context } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type { PostfixReservedName } from '../tokenizer/postfix/postfixReservedNames'
import type { SourceCodeInfo, TokenStream } from '../tokenizer/interface'
import type { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'
import type { Token } from '../tokenizer/tokens'
import type { ModifierName } from '../tokenizer/postfix/postfixTokens'

export interface ParseState {
  position: number
  infix: boolean
  parseToken: ParseToken
}
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

export type DebugData = {
  token: Token
  lastToken: Token
  nameToken?: Token
}
export interface GenericNode {
  t: AstNodeType // type
  p: AstNode[] // params
  n: string | undefined // name
  debugData: DebugData | undefined
}

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode
export type ParseBinding = (tokens: TokenStream, parseState: ParseState) => BindingNode
export type ParseBindings = (tokens: TokenStream, parseState: ParseState) => BindingNode[]
export type ParseArgument = (tokens: TokenStream, parseState: ParseState) => ArgumentNode | ModifierNode
export type ParseExpression = (tokens: TokenStream, parseState: ParseState) => ExpressionNode
export type ParseTokensUntilClosingBracket = (tokens: TokenStream, parseState: ParseState) => AstNode[]
export type ParseToken = (tokens: TokenStream, parseState: ParseState) => AstNode

export interface NumberNode extends GenericNode {
  t: AstNodeType.Number // type
  v: number // value
}
export interface StringNode extends GenericNode {
  t: AstNodeType.String // type
  v: string // value
}
export interface SymbolNode extends GenericNode {
  t: AstNodeType.Name // type
  v: string // value
}
export interface ModifierNode extends GenericNode {
  t: AstNodeType.Modifier // type
  v: ModifierName
}
export interface ReservedSymbolNode extends GenericNode {
  t: AstNodeType.ReservedName // type
  v: PostfixReservedName // reservedName
}

interface CommonNormalExpressionNode extends GenericNode {
  t: AstNodeType.NormalExpression // type
}

export interface CommonSpecialExpressionNode<T extends SpecialExpressionName> extends GenericNode {
  t: AstNodeType.SpecialExpression // type
  n: T // name
}

export interface NormalExpressionNodeWithName extends CommonNormalExpressionNode {
  n: string // name
}

interface NormalExpressionNodeExpression extends CommonNormalExpressionNode {
  n: undefined // name not present. E.g. ([1 2 3] 2)
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

export interface CommentNode extends GenericNode {
  t: AstNodeType.Comment // type
  v: string // value
}

export type AstNode =
  | NumberNode
  | StringNode
  | ReservedSymbolNode
  | SymbolNode
  | CommentNode
  | NormalExpressionNode
  | ModifierNode
  | SpecialExpressionNode

type AstBody = AstNode[]
export interface Ast {
  b: AstBody // body
  hasDebugData: boolean
}
