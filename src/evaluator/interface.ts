import { Any, Arr } from '../interface'
import { AstNode } from '../parser/interface'
import { SourceCodeInfo } from '../tokenizer/interface'

export type ContextEntry = { value: Any }
export type Context = Record<string, ContextEntry>
export interface ContextStack {
  withContext(context: Context): ContextStack
  globalContext: Context
  stack: Context[]
}

export type EvaluateAstNode = (node: AstNode, contextStack: ContextStack) => Any
export type ExecuteFunction = (fn: Any, params: Arr, sourceCodeInfo: SourceCodeInfo, contextStack: ContextStack) => Any
