import type { EvaluateAstNode, ExecuteFunction } from '../evaluator/interface'
import type {
  NormalExpressionNode,
  ParseArgument,
  ParseBinding,
  ParseBindings,
  ParseExpression,
  ParseToken,
  ParseTokens,
  SpecialExpressionNode,
} from '../parser/interface'
import type { SourceCodeInfo, TokenStream } from '../tokenizer/interface'
import type { Any, Arr } from '../interface'
import type { AnalyzeAst, AnalyzeResult } from '../analyze/interface'
import type { ContextStack } from '../evaluator/ContextStack'

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T
type ValidateNode = (node: NormalExpressionNode) => void

interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  validate?: ValidateNode
}

export interface ParserHelpers {
  parseExpression: ParseExpression
  parseTokens: ParseTokens
  parseToken: ParseToken
  parseBinding: ParseBinding
  parseBindings: ParseBindings
  parseArgument: ParseArgument
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression<Any>>
export type BuiltinSpecialExpressions = Record<string, BuiltinSpecialExpression<Any>>

interface EvaluateHelpers {
  evaluateAstNode: EvaluateAstNode
  builtin: Builtin
}
export interface BuiltinSpecialExpression<T> {
  parse: (tokenStream: TokenStream, position: number, parsers: ParserHelpers) => [number, SpecialExpressionNode]
  evaluate: (node: SpecialExpressionNode, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  validate?: (node: SpecialExpressionNode) => void
  analyze: (
    node: SpecialExpressionNode,
    contextStack: ContextStack,
    params: { analyzeAst: AnalyzeAst, builtin: Builtin },
  ) => AnalyzeResult
}

export type SpecialExpressionName =
  | 'and'
  | 'block'
  | 'comment'
  | 'cond'
  | 'def'
  | 'defn'
  | 'defns'
  | 'defs'
  | 'do'
  | 'doseq'
  | 'fn'
  | 'for'
  | 'function'
  | 'if-let'
  | 'if-not'
  | 'if'
  | 'let'
  | 'loop'
  | 'or'
  | 'partial'
  | 'recur'
  | 'return-from'
  | 'return'
  | 'throw'
  | 'time!'
  | 'try'
  | 'when-first'
  | 'when-let'
  | 'when-not'
  | 'when'
  | 'declared?'
  | '??'

export interface Builtin {
  normalExpressions: BuiltinNormalExpressions
  specialExpressions: BuiltinSpecialExpressions
}
