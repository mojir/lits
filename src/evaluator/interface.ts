import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

export type ContextEntry = { value: unknown; constant: boolean }
export type Context = Record<string, ContextEntry>

export type EvaluateAstNode = (node: AstNode, contextStack: Context[]) => unknown
export type EvaluateLispishFunction = (
  lispishFunction: LispishFunction,
  params: unknown[],
  contextStack: Context[],
) => unknown
