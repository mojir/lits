import { Any, Arr } from '../interface'
import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

export type ContextEntry = { value: Any }
export type Context = Record<string, ContextEntry>

export type EvaluateAstNode = (node: AstNode, contextStack: Context[]) => Any
export type ExecuteLispishFunction = (lispishFunction: LispishFunction, params: Arr, contextStack: Context[]) => Any
export type ExecuteFunction = (fn: Any, params: Arr, contextStack: Context[]) => Any
