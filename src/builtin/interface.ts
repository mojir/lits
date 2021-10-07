import { Context, EvaluateAstNode, EvaluateLispishFunction } from '../evaluator/interface'
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

type Evaluate = (
  params: unknown[],
  contextStack: Context[],
  { evaluateLispishFunction }: { evaluateLispishFunction: EvaluateLispishFunction },
) => unknown
type ValidateNode = (node: NormalExpressionNode) => void

type BuiltinNormalExpression = {
  evaluate: Evaluate
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
export type BuiltinSpecialExpression = {
  parse: (tokens: Token[], position: number, parsers: Parsers) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: Context[], helpers: EvaluateHelpers) => unknown
  validate?: (node: SpecialExpressionNode) => void
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
  | `unless`
  | `when`

export type Builtin = {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
