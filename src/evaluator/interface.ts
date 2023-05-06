import { ContextStack } from '../ContextStack'
import { Any, Arr } from '../interface'
import { AstNode } from '../parser/interface'
import { DebugInfo } from '../tokenizer/interface'

export type EvaluateAstNode = (node: AstNode, contextStack: ContextStack) => Any
export type ExecuteFunction = (fn: Any, params: Arr, contextStack: ContextStack, debugInfo?: DebugInfo) => Any
