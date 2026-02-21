import type { Any, Arr } from '../interface'
import type { AstNode, FunctionLike } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { isUnknownRecord } from '../typeGuards'
import type { ContextStack } from './ContextStack'

export interface ContextEntry { value: Any }
export type Context = Record<string, ContextEntry>

export type EvaluateNode = (node: AstNode, contextStack: ContextStack) => Any
export type ExecuteFunction = (fn: FunctionLike, params: Arr, contextStack: ContextStack, sourceCodeInfo?: SourceCodeInfo) => Any

export type LookUpResult = ContextEntry | null

export function isContextEntry(value: unknown): value is ContextEntry {
  return isUnknownRecord(value) && value.value !== undefined
}
