import type { LitsParams } from '../Lits/Lits'
import { builtin } from '../builtin'
import type { Builtin } from '../builtin/interface'
import { type ContextStack, createContextStack } from '../evaluator/ContextStack'
import type { Ast, AstNode } from '../parser/interface'
import type { Token } from '../tokenizer/token'
import { findUnresolvedSymbols } from './findUnresolvedSymbols'

export interface UnresolvedSymbol {
  symbol: string
  token: Token | undefined
}

// A set of unresolved symbold
export type UnresolvedSymbols = Set<UnresolvedSymbol>

// The result of analyzing an AST
export interface Analysis {
  unresolvedSymbols: UnresolvedSymbols
}

// A function that finds unresolved symbold in an AST node or array of AST nodes
export type FindUnresolvedSymbols = (ast: Ast | AstNode[], contextStack: ContextStack, builtin: Builtin) => UnresolvedSymbols

export function analyze(ast: Ast, params: LitsParams): Analysis {
  return {
    unresolvedSymbols: findUnresolvedSymbols(ast, createContextStack(params), builtin),
  }
}
