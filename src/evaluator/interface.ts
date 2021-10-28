import { Any, Arr } from '../interface'
import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

export type ContextEntry = { value: Any }
export type Context = Record<string, ContextEntry>
export interface ContextStack {
  withContext(context: Context): ContextStack
  globalContext: Context
  stack: Context[]
}

export type EvaluateAstNode = (node: AstNode, contextStack: ContextStack) => Any
export type ExecuteLispishFunction = (lispishFunction: LispishFunction, params: Arr, contextStack: ContextStack) => Any
export type ExecuteFunction = (fn: Any, params: Arr, contextStack: ContextStack) => Any
