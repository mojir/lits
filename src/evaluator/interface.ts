import { Any, Arr } from '../interface'
import { LitsFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'
import { TokenMeta } from '../tokenizer/interface'

export type ContextEntry = { value: Any }
export type Context = Record<string, ContextEntry>
export interface ContextStack {
  withContext(context: Context): ContextStack
  globalContext: Context
  stack: Context[]
}

export type EvaluateAstNode = (node: AstNode, contextStack: ContextStack) => Any
export type ExecuteLitsFunction = (litsFunction: LitsFunction, params: Arr, contextStack: ContextStack) => Any
export type ExecuteFunction = (fn: Any, params: Arr, meta: TokenMeta, contextStack: ContextStack) => Any
