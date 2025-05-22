import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateNode, ExecuteFunction } from '../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../getUndefinedSymbols'
import type { Any, Arr } from '../interface'
import type {
  SpecialExpressionNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import type { SpecialExpressions } from '.'

export type ParamCount = number | { min?: number, max?: number, even?: true, odd?: true }

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T

export interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  paramCount: ParamCount
  aliases?: string[]
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression<Any>>
export type BuiltingAllNormalExpressions = BuiltinNormalExpression<Any>[]

export interface EvaluateHelpers {
  evaluateNode: EvaluateNode
  builtin: Builtin
  getUndefinedSymbols: GetUndefinedSymbols
}
export interface BuiltinSpecialExpression<T, N extends SpecialExpressionNode> {
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => T
  evaluateAsNormalExpression?: NormalExpressionEvaluator<T>
  paramCount: ParamCount
  getUndefinedSymbols: (
    node: N,
    contextStack: ContextStack,
    params: { getUndefinedSymbols: GetUndefinedSymbols, builtin: Builtin, evaluateNode: EvaluateNode },
  ) => UndefinedSymbols
}

export interface Builtin {
  normalExpressions: BuiltinNormalExpressions
  allNormalExpressions: BuiltingAllNormalExpressions
  specialExpressions: SpecialExpressions
}
