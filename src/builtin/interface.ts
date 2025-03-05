import type { FindUnresolvedIdentifiers, UnresolvedIdentifiers } from '../analyze'
import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateAstNode, ExecuteFunction } from '../evaluator/interface'
import type { Any, Arr } from '../interface'
import type {
  ParseArgument,
  ParseBinding,
  ParseBindings,
  ParseExpression,
  ParseState,
  ParseToken,
  ParseTokensUntilClosingBracket,
} from '../parser/interface'
import type { SourceCodeInfo, TokenStream } from '../tokenizer/interface'
import type { Token } from '../tokenizer/tokens'

import type { BuiltinSpecialExpressions, SpecialExpressionNode } from '.'

export type Count = number | { min?: number, max?: number, even?: boolean, odd?: boolean }

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T

interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  paramCount: Count
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
  polishParse: (tokenStream: TokenStream, parseState: ParseState, firstToken: Token, parsers: ParserHelpers) => N
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  paramCount: Count
  findUnresolvedIdentifiers: (
    node: N,
    contextStack: ContextStack,
    params: { findUnresolvedIdentifiers: FindUnresolvedIdentifiers, builtin: Builtin, evaluateAstNode: EvaluateAstNode },
  ) => UnresolvedIdentifiers
}

export interface Builtin {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
