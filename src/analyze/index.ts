import type { LitsParams } from '../Lits/Lits'
import { builtin } from '../builtin'
import type { Builtin } from '../builtin/interface'
import { type ContextStack, createContextStack } from '../evaluator/ContextStack'
import type { Ast, AstNode } from '../parser/interface'
import type { Token } from '../tokenizer/tokens'
import { findUnresolvedIdentifiers } from './findUnresolvedIdentifiers'

export interface UnresolvedIdentifier {
  symbol: string
  token: Token | undefined
}

// A set of unresolved identifiers
export type UnresolvedIdentifiers = Set<UnresolvedIdentifier>

// The result of analyzing an AST
export interface Analysis {
  unresolvedIdentifiers: UnresolvedIdentifiers
}

// A function that finds unresolved identifiers in an AST node or array of AST nodes
export type FindUnresolvedIdentifiers = (ast: Ast | AstNode[], contextStack: ContextStack, builtin: Builtin) => UnresolvedIdentifiers

export function analyze(ast: Ast, params: LitsParams): Analysis {
  return {
    unresolvedIdentifiers: findUnresolvedIdentifiers(ast, createContextStack(params), builtin),
  }
}
