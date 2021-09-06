import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

export type VariableScope = Record<string, unknown>
export type FunctionScope = Record<string, LispishFunction>
export type Context = {
  variables: VariableScope
  functions: FunctionScope
}
export type EvaluateAstNode = (node: AstNode, contextStack: Context[]) => unknown
export type EvaluateLispishFunction = (
  lispishFunction: LispishFunction,
  params: unknown[],
  contextStack: Context[],
) => unknown
