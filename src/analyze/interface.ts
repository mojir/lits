import { Builtin } from '../builtin/interface'
import { ContextStack } from '../evaluator/interface'
import { AstNode } from '../parser/interface'
import { Token } from '../tokenizer/interface'

export type UndefinedSymbolEntry = {
  symbol: string
  token: Token | undefined
}

export type AnalyzeResult = {
  undefinedSymbols: Set<UndefinedSymbolEntry>
}
export type AnalyzeAst = (astNode: AstNode | AstNode[], contextStack: ContextStack, builtin: Builtin) => AnalyzeResult
