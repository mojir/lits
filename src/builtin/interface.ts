import { ContextStack, EvaluateAstNode, ExecuteFunction } from '../evaluator/interface'
import {
  ParseArgument,
  ParseBindings,
  ParseExpression,
  ParseTokens,
  ParseToken,
  SpecialExpressionNode,
  ParseBinding,
} from '../parser/interface'
import { Token, TokenMeta } from '../tokenizer/interface'
import { NormalExpressionNode } from '../parser/interface'
import { Any, Arr } from '../interface'

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  meta: TokenMeta,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T
type ValidateNode = (node: NormalExpressionNode) => void

type BuiltinNormalExpression<T> = {
  evaluate: NormalExpressionEvaluator<T>
  validate?: ValidateNode
}

export type Parsers = {
  parseExpression: ParseExpression
  parseTokens: ParseTokens
  parseToken: ParseToken
  parseBinding: ParseBinding
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
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  validate?: (node: N) => void
}

export type SpecialExpressionName =
  | `and`
  | `block`
  | `cond`
  | `def`
  | `defn`
  | `defns`
  | `defs`
  | `do`
  | `for`
  | `fn`
  | `function`
  | `if-let`
  | `if-not`
  | `if`
  | `let`
  | `loop`
  | `or`
  | `partial`
  | `recur`
  | `return-from`
  | `return`
  | `throw`
  | `time!`
  | `try`
  | `when-first`
  | `when-let`
  | `when-not`
  | `when`

export type Builtin = {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
