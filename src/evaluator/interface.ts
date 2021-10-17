import { Arr } from '../interface'
import { LispishFunction } from '../parser/interface'
import { AstNode } from '../parser/interface'

export type ContextEntry = { value: unknown }
export type Context = Record<string, ContextEntry>

export type EvaluateAstNode = (node: AstNode, contextStack: Context[]) => unknown
export type EvaluateLispishFunction = (
  lispishFunction: LispishFunction,
  params: Arr,
  contextStack: Context[],
) => unknown

export type EvaluateFunction = (fn: unknown, params: unknown[], contextStack: Context[]) => unknown
