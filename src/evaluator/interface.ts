import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

type VariableScope = Record<string, { value: unknown; constant: boolean }>
type FunctionScope = Record<string, { fun: LispishFunction; constant: boolean }>
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
