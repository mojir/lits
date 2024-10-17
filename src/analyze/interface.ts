import type { Builtin } from '../builtin/interface'
import type { ContextStack } from '../evaluator/ContextStack'
import type { AstNode } from '../parser/interface'
import type { Token } from '../tokenizer/interface'

export interface UndefinedSymbolEntry {
  symbol: string
  token: Token | undefined
}

export interface AnalyzeResult {
  undefinedSymbols: Set<UndefinedSymbolEntry>
}
export type AnalyzeAst = (astNode: AstNode | AstNode[], contextStack: ContextStack, builtin: Builtin) => AnalyzeResult
