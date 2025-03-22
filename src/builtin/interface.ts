import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateNode, ExecuteFunction } from '../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../getUndefinedSymbols'
import type { Any, Arr } from '../interface'
import type {
  ParseArgument,
  ParseBinding,
  ParseBindings,
  ParseExpression,
  ParseToken,
  ParseTokensUntilClosingBracket,
  SpecialExpressionNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import type { SpecialExpressions } from '.'

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

export interface EvaluateHelpers {
  evaluateNode: EvaluateNode
  builtin: Builtin
  getUndefinedSymbols: GetUndefinedSymbols
}
export interface BuiltinSpecialExpression<T, N extends SpecialExpressionNode> {
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  paramCount: Count
  getUndefinedSymbols: (
    node: N,
    contextStack: ContextStack,
    params: { getUndefinedSymbols: GetUndefinedSymbols, builtin: Builtin, evaluateNode: EvaluateNode },
  ) => UndefinedSymbols
}

export interface Builtin {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: SpecialExpressions
}
