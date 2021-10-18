import { Context, EvaluateAstNode, ExecuteFunction } from '../evaluator/interface'
import {
  ParseArgument,
  ParseBindings,
  ParseExpression,
  ParseTokens,
  ParseToken,
  SpecialExpressionNode,
} from '../parser/interface'
import { Token } from '../tokenizer/interface'
import { NormalExpressionNode } from '../parser/interface'
import { Arr } from '../interface'

export type NormalExpressionEvaluator = (
  params: Arr,
  contextStack: Context[],
  { executeFunction }: { executeFunction: ExecuteFunction },
) => unknown
type ValidateNode = (node: NormalExpressionNode) => void

type BuiltinNormalExpression = {
  evaluate: NormalExpressionEvaluator
  validate?: ValidateNode
}

type Parsers = {
  parseExpression: ParseExpression
  parseTokens: ParseTokens
  parseToken: ParseToken
  parseBindings: ParseBindings
  parseArgument: ParseArgument
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression>
export type BuiltinSpecialExpressions = Record<string, BuiltinSpecialExpression>

type EvaluateHelpers = {
  evaluateAstNode: EvaluateAstNode
  builtin: Builtin
}
export type BuiltinSpecialExpression<T extends SpecialExpressionNode = SpecialExpressionNode> = {
  parse: (tokens: Token[], position: number, parsers: Parsers) => [number, T]
  evaluate: (node: T, contextStack: Context[], helpers: EvaluateHelpers) => unknown
  validate?: (node: T) => void
}

export type SpecialExpressionName =
  | `and`
  | `block`
  | `cond`
  | `defn`
  | `defns`
  | `fn`
  | `function`
  | `if`
  | `if-not`
  | `let`
  | `or`
  | `do`
  | `loop`
  | `recur`
  | `return-from`
  | `return`
  | `def`
  | `defs`
  | `throw`
  | `try`
  | `when`
  | `partial`
  | `time!`

export type Builtin = {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
