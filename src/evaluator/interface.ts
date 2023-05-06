import { Any, Arr } from '../interface'
import { AstNode, BuiltinFunction } from '../parser/interface'
import { DebugInfo } from '../tokenizer/interface'

export type ContextEntry = { value: Any }
export type Context = Record<string, ContextEntry>
export interface ContextStack {
  withContext(context: Context): ContextStack
  globalContext: Context
  stack: Context[]
}

export type EvaluateAstNode = (node: AstNode, contextStack: ContextStack) => Any
export type ExecuteFunction = (fn: Any, params: Arr, contextStack: ContextStack, debugInfo?: DebugInfo) => Any

export type LookUpResult = {
  contextEntry: ContextEntry | null
  builtinFunction: BuiltinFunction | null
  specialExpression: true | null
}
