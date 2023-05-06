import { Builtin } from '../../builtin/interface'
import { ContextStack } from '../../ContextStack'
import { AstNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'

export type UndefinedSymbolEntry = {
  symbol: string
  token: Token | undefined
}

export type FindUndefinedSymbols = (
  astNode: AstNode | AstNode[],
  contextStack: ContextStack,
  builtin: Builtin,
) => Set<UndefinedSymbolEntry>
