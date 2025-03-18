import { LitsError } from '../errors'
import type { ContextStack } from '../evaluator/ContextStack'
import type { AstNode, BindingNode, BindingTarget } from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import { isReservedSymbol } from '../tokenizer/reservedNames'
import type { Builtin } from './interface'
import type { SpecialExpressionName } from '.'

export interface Function {
  arguments: FunctionArgument[]
  bindingNodes: BindingNode[]
  body: AstNode[]
}

export type FunctionArgument = BindingTarget & {
  rest?: true
}

export function assertNameNotDefined<T>(
  name: T,
  contextStack: ContextStack,
  builtin: Builtin,
  sourceCodeInfo?: SourceCodeInfo,
): asserts name is T {
  if (typeof name !== 'string')
    return

  if (builtin.specialExpressions[name as SpecialExpressionName])
    throw new LitsError(`Cannot define variable ${name}, it's a special expression.`, sourceCodeInfo)

  if (builtin.normalExpressions[name])
    throw new LitsError(`Cannot define variable ${name}, it's a builtin function.`, sourceCodeInfo)

  if (isReservedSymbol(name))
    throw new LitsError(`Cannot define variable ${name}, it's a reserved name.`, sourceCodeInfo)

  if (contextStack.globalContext[name])
    throw new LitsError(`Name already defined "${name}".`, sourceCodeInfo)
}
