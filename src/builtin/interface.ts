import type { GetUndefinedSymbols, UndefinedSymbols } from '../getUndefinedSymbols'
import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateAstNode, ExecuteFunction } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type {
  ParseArgument,
  ParseBinding,
  ParseBindings,
  ParseExpression,
  ParseToken,
  ParseTokensUntilClosingBracket,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import type { BuiltinSpecialExpressions, SpecialExpressionNode } from '.'

export type Count = number | { min?: number, max?: number, even?: true, odd?: true }

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T

interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  paramCount: Count
  aliases?: string[]
}

export interface ParserHelpers {
  parseExpression: ParseExpression
  parseTokensUntilClosingBracket: ParseTokensUntilClosingBracket
  parseToken: ParseToken
  parseBinding: ParseBinding
  parseBindings: ParseBindings
  parseArgument: ParseArgument
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression<Any>>

interface EvaluateHelpers {
  evaluateAstNode: EvaluateAstNode
  builtin: Builtin
}
export interface BuiltinSpecialExpression<T, N extends SpecialExpressionNode> {
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  paramCount: Count
  getUndefinedSymbols: (
    node: N,
    contextStack: ContextStack,
    params: { getUndefinedSymbols: GetUndefinedSymbols, builtin: Builtin, evaluateAstNode: EvaluateAstNode },
  ) => UndefinedSymbols
}

export interface Builtin {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
