import type { JsFunction } from '../Lits/Lits'
import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import type { AstNodeType, FunctionType } from '../constants/constants'
import type { Context } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type { TokenStream } from '../tokenizer/tokenize'
import type { ModifierName, SourceCodeInfo, Token } from '../tokenizer/token'
import type { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'

export interface ParseState {
  position: number
}

export interface EvaluatedFunction {
  arguments: BindingTarget[]
  body: AstNode[]
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
export interface GenericNode {
  type: AstNodeType
  sourceCodeInfo?: SourceCodeInfo | undefined
}

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode
export type ParseBinding = (tokens: TokenStream, parseState: ParseState) => BindingNode
export type ParseBindings = (tokens: TokenStream, parseState: ParseState) => BindingNode[]
export type ParseArgument = (tokens: TokenStream, parseState: ParseState) => ArgumentNode | ModifierNode
export type ParseExpression = (tokens: TokenStream, parseState: ParseState) => ExpressionNode
export type ParseTokensUntilClosingBracket = (tokens: TokenStream, parseState: ParseState) => AstNode[]
export type ParseToken = (tokens: TokenStream, parseState: ParseState) => AstNode

export interface SpreadNode extends GenericNode {
  type: 'Spread'
  value: AstNode // An array node or object node
}

export interface NumberNode extends GenericNode {
  type: 'Number'
  value: number
}
export interface StringNode extends GenericNode {
  type: 'String'
  value: string // value
}
export interface SymbolNode extends GenericNode {
  type: 'Symbol'
  value: string // value
}
export interface ModifierNode extends GenericNode {
  type: 'Modifier'
  value: ModifierName
}
export interface ReservedSymbolNode extends GenericNode {
  type: 'ReservedSymbol'
  value: string
}

interface CommonNormalExpressionNode extends GenericNode {
  type: 'NormalExpression'
  params: AstNode[] // params
}

export interface CommonSpecialExpressionNode<T extends SpecialExpressionName> extends GenericNode {
  type: 'SpecialExpression'
  name: T // name
  params: AstNode[] // params
}

export interface NormalExpressionNodeWithName extends CommonNormalExpressionNode {
  name: string // name
}

interface NormalExpressionNodeExpression extends CommonNormalExpressionNode {
  name: undefined // name not present. E.g. ([1 2 3] 2)
}

export type NormalExpressionNode = NormalExpressionNodeWithName | NormalExpressionNodeExpression

interface CommonBindingTarget {
  sourceCodeInfo: SourceCodeInfo | undefined
  default?: AstNode
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

export interface BindingNode extends GenericNode {
  type: 'Binding'
  target: BindingTarget
  value: AstNode // value
}

export interface ArgumentNode extends GenericNode {
  type: 'Argument'
  name: string // name
  default?: AstNode // defaultValue
}

export interface CommentNode extends GenericNode {
  type: 'Comment'
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
  | SpreadNode

type AstBody = AstNode[]
export interface Ast {
  body: AstBody // body
  hasDebugData: boolean
}
