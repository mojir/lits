import { AstNode } from '../parser/interface'

export type Context = Record<string, unknown>
export type EvaluateAstNode = (node: AstNode, contextStack: Context[]) => unknown
