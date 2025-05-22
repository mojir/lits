import type { JsFunction } from '../Lits/Lits'
import type { SpecialExpressionType } from '../builtin'
import type { ParamCount } from '../builtin/interface'
import type { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import type { FunctionType, NodeType, NodeTypes } from '../constants/constants'
import type { Context } from '../evaluator/interface'
import type { Any, Arr, Coll } from '../interface'
import type { ReservedSymbol } from '../tokenizer/reservedNames'
import type { SourceCodeInfo, Token } from '../tokenizer/token'
import type { FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'

export interface ParseState {
  position: number
}

export type EvaluatedFunction = [BindingTarget[], Node[], Context]

interface GenericLitsFunction {
  [FUNCTION_SYMBOL]: true
  sourceCodeInfo?: SourceCodeInfo
  functionType: FunctionType
  paramCount: ParamCount
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
  function: FunctionLike
  params: Arr
  placeholders: number[] // indexes of the placeholders
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
  function: FunctionLike
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
  function: FunctionLike
  params: Arr
}

export interface NormalBuiltinFunction extends GenericLitsFunction {
  functionType: 'Builtin'
  normalBuitinSymbolType: number
}

export interface SpecialBuiltinFunction extends GenericLitsFunction {
  functionType: 'SpecialBuiltin'
  specialBuiltinSymbolType:
    | typeof specialExpressionTypes['&&']
    | typeof specialExpressionTypes['||']
    | typeof specialExpressionTypes['array']
    | typeof specialExpressionTypes['object']
    | typeof specialExpressionTypes['defined?']
    | typeof specialExpressionTypes['recur']
    | typeof specialExpressionTypes['throw']
    | typeof specialExpressionTypes['??']
}

export type LitsFunction =
  | NativeJsFunction
  | UserDefinedFunction
  | NormalBuiltinFunction
  | SpecialBuiltinFunction
  | PartialFunction
  | CompFunction
  | ConstantlyFunction
  | JuxtFunction
  | ComplementFunction
  | EveryPredFunction
  | SomePredFunction
  | FNullFunction

export type LitsFunctionType = LitsFunction['functionType']

export type FunctionLike = LitsFunction | Coll | number

export type DebugData = {
  token: Token
  nameToken?: Token
}
export type Node<T extends NodeType = NodeType, Payload = unknown> = [T, Payload] | [T, Payload, SourceCodeInfo]

export type ExpressionNode = NormalExpressionNode | SpecialExpressionNode | NumberNode | StringNode

export type SpreadNode = Node<typeof NodeTypes.Spread, Node> // Payload should be array or object depending on context
export type NumberNode = Node<typeof NodeTypes.Number, number>
export type StringNode = Node<typeof NodeTypes.String, string>
export type UserDefinedSymbolNode = Node<typeof NodeTypes.UserDefinedSymbol, string>
export type NormalBuiltinSymbolNode = Node<typeof NodeTypes.NormalBuiltinSymbol, number>
export type SpecialBuiltinSymbolNode = Node<typeof NodeTypes.SpecialBuiltinSymbol, SpecialExpressionType>
export type SymbolNode = UserDefinedSymbolNode | NormalBuiltinSymbolNode | SpecialBuiltinSymbolNode
export type ReservedSymbolNode = Node<typeof NodeTypes.ReservedSymbol, ReservedSymbol>
export type SpecialExpressionNode<T extends [SpecialExpressionType, ...unknown[]] = [SpecialExpressionType, ...unknown[]]> = Node<typeof NodeTypes.SpecialExpression, T> // [name, params]

export type NormalExpressionNodeWithName = Node<typeof NodeTypes.NormalExpression, [NormalBuiltinSymbolNode | UserDefinedSymbolNode, Node[]]> // [params, name]
export type NormalExpressionNodeExpression = Node<typeof NodeTypes.NormalExpression, [Node, Node[]]> // [name, node as function] node can be string number object or array
export type NormalExpressionNode = NormalExpressionNodeWithName | NormalExpressionNodeExpression
export const bindingTargetTypes = {
  symbol: 11,
  rest: 12,
  object: 13,
  array: 14,
} as const

export type BindingTargetType = typeof bindingTargetTypes[keyof typeof bindingTargetTypes]

type GenericTarget<T extends BindingTargetType, Payload extends unknown[]> = [T, Payload] | [T, Payload, SourceCodeInfo]

export type SymbolBindingTarget = GenericTarget<typeof bindingTargetTypes.symbol, [SymbolNode, Node | undefined /* default value */]>
export type RestBindingTarget = GenericTarget<typeof bindingTargetTypes.rest, [string, Node | undefined /* default value */]>
export type ObjectBindingTarget = GenericTarget<typeof bindingTargetTypes.object, [Record<string, BindingTarget>, Node | undefined /* default value */]>
export type ArrayBindingTarget = GenericTarget<typeof bindingTargetTypes.array, [(BindingTarget | null)[], Node | undefined /* default value */]>

export type BindingTarget = SymbolBindingTarget | RestBindingTarget | ObjectBindingTarget | ArrayBindingTarget

export type BindingNode = Node<typeof NodeTypes.Binding, [BindingTarget, Node]> // [target, value]

type AstBody = Node[]
export interface Ast {
  body: AstBody // body
  hasDebugData: boolean
}
