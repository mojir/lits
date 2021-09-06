import { AstNode } from '../parser/interface'

export type VariableScope = Record<string, unknown>
export type Context = {
  variables: VariableScope
}
export type EvaluateAstNode = (node: AstNode, contextStack: Context[]) => unknown
