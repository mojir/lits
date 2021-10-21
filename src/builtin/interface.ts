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
import { Any, Arr } from '../interface'

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  contextStack: Context[],
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T
type ValidateNode = (node: NormalExpressionNode) => void

type BuiltinNormalExpression<T> = {
  evaluate: NormalExpressionEvaluator<T>
  validate?: ValidateNode
}

type Parsers = {
  parseExpression: ParseExpression
  parseTokens: ParseTokens
  parseToken: ParseToken
  parseBindings: ParseBindings
  parseArgument: ParseArgument
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression<Any>>
export type BuiltinSpecialExpressions = Record<string, BuiltinSpecialExpression<Any>>

type EvaluateHelpers = {
  evaluateAstNode: EvaluateAstNode
  builtin: Builtin
}
export type BuiltinSpecialExpression<T, N extends SpecialExpressionNode = SpecialExpressionNode> = {
  parse: (tokens: Token[], position: number, parsers: Parsers) => [number, N]
  evaluate: (node: N, contextStack: Context[], helpers: EvaluateHelpers) => T
  validate?: (node: N) => void
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
  | `if-let`
  | `when-let`
  | `when-not`
  | `when-first`

export type Builtin = {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
