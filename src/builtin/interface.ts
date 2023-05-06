import { EvaluateAstNode, ExecuteFunction } from '../evaluator/interface'
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
import { Any, Arr } from '../interface'
import { FindUndefinedSymbols, UndefinedSymbolEntry } from '../analyze/undefinedSymbols/interface'
import { ContextStack } from '../ContextStack'
import { LookUpResult } from '../lookup/interface'

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  debugInfo: DebugInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T
type ValidateArity = (arity: number, debugInfo: DebugInfo | undefined) => void

type BuiltinNormalExpression<T> = {
  evaluate: NormalExpressionEvaluator<T>
  validateArity: ValidateArity
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
  validateArity: ValidateArity
  findUndefinedSymbols(
    node: SpecialExpressionNode,
    contextStack: ContextStack,
    params: { findUndefinedSymbols: FindUndefinedSymbols; builtin: Builtin },
  ): Set<UndefinedSymbolEntry>
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
