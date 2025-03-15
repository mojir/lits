import type { JsFunction, LazyValue } from '../Lits/Lits'
import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import type { Arity } from '../builtin/utils'
import type { AstNodeType, FunctionType } from '../constants/constants'
import type { Context } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type { TokenStream } from '../tokenizer/tokenize'
import type { ModifierName, SourceCodeInfo, Token } from '../tokenizer/token'
import type { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'

export interface ParseState {
  position: number
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

export interface FNullFunction extends GenericLitsFunction {
  t: FunctionType.Fnull
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
  | FNullFunction

export type LitsFunctionType = LitsFunction['t']

export type DebugData = {
  token: Token
  nameToken?: Token
}
export interface GenericNode {
  type: AstNodeType // type
  params: AstNode[] // params
  name: string | undefined // name
  token: Token | undefined
}

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode
export type ParseBinding = (tokens: TokenStream, parseState: ParseState) => BindingNode
export type ParseBindings = (tokens: TokenStream, parseState: ParseState) => BindingNode[]
export type ParseArgument = (tokens: TokenStream, parseState: ParseState) => ArgumentNode | ModifierNode
export type ParseExpression = (tokens: TokenStream, parseState: ParseState) => ExpressionNode
export type ParseTokensUntilClosingBracket = (tokens: TokenStream, parseState: ParseState) => AstNode[]
export type ParseToken = (tokens: TokenStream, parseState: ParseState) => AstNode

export interface NumberNode extends GenericNode {
  type: 'Number' // type
  value: number // value
}
export interface StringNode extends GenericNode {
  type: 'String' // type
  value: string // value
}
export interface SymbolNode extends GenericNode {
  type: 'Symbol' // type
  value: string // value
}
export interface ModifierNode extends GenericNode {
  type: 'Modifier' // type
  value: ModifierName
}
export interface ReservedSymbolNode extends GenericNode {
  type: 'ReservedSymbol' // type
  value: string
}

interface CommonNormalExpressionNode extends GenericNode {
  type: 'NormalExpression' // type
}

export interface CommonSpecialExpressionNode<T extends SpecialExpressionName> extends GenericNode {
  type: 'SpecialExpression' // type
  name: T // name
}

export interface NormalExpressionNodeWithName extends CommonNormalExpressionNode {
  name: string // name
}

interface NormalExpressionNodeExpression extends CommonNormalExpressionNode {
  name: undefined // name not present. E.g. ([1 2 3] 2)
}

export type NormalExpressionNode = NormalExpressionNodeWithName | NormalExpressionNodeExpression

export interface BindingNode extends GenericNode {
  type: 'Binding' // type
  name: string // name
  value: AstNode // value
}

export interface ArgumentNode extends GenericNode {
  type: 'Argument' // type
  name: string // name
  default?: AstNode // defaultValue
}

export interface CommentNode extends GenericNode {
  type: 'Comment' // type
  value: string // value
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
  body: AstBody // body
  hasDebugData: boolean
}
