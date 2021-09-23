import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

export type VariableScope = Record<string, { value: unknown; const: boolean }>
export type FunctionScope = Record<string, { fun: LispishFunction; const: boolean }>
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
