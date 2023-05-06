import { ContextStack, EvaluateAstNode, ExecuteFunction, LookUpResult } from '../evaluator/interface'
import {
  ParseArgument,
  ParseBindings,
  ParseExpression,
  ParseTokens,
  ParseToken,
  ParseBinding,
  NameNode,
  SpecialExpressionNode,
} from '../parser/interface'
import { Token, DebugInfo } from '../tokenizer/interface'
import { NormalExpressionNode } from '../parser/interface'
import { Any, Arr } from '../interface'
import { AnalyzeAst, AnalyzeResult } from '../analyze/interface'

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  debugInfo: DebugInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T
type ValidateNode = (node: NormalExpressionNode) => void

type BuiltinNormalExpression<T> = {
  evaluate: NormalExpressionEvaluator<T>
  validate?: ValidateNode
}

export type ParserHelpers = {
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
  lookUp(nameNode: NameNode, contextStack: ContextStack): LookUpResult
}
export type BuiltinSpecialExpression<T> = {
  parse: (tokens: Token[], position: number, parsers: ParserHelpers) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  validate?: (node: SpecialExpressionNode) => void
  analyze(
    node: SpecialExpressionNode,
    contextStack: ContextStack,
    params: { analyzeAst: AnalyzeAst; builtin: Builtin },
  ): AnalyzeResult
}

export type SpecialExpressionName =
  | `and`
  | `block`
  | `comment`
  | `cond`
  | `def`
  | `defn`
  | `defns`
  | `defs`
  | `do`
  | `doseq`
  | `fn`
  | `for`
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
  | `declared?`
  | `??`

export type Builtin = {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
