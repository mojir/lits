import { ContextStack } from '../ContextStack'
import { LitsError } from '../errors'
import { AstNode, BindingNode } from '../parser/interface'
import { reservedNamesRecord } from '../reservedNames'
import { DebugInfo } from '../tokenizer/interface'
import { Builtin } from './interface'

export type Arity = number | { min: number }

export type FunctionOverload = {
  arguments: FunctionArguments
  arity: Arity
  body: AstNode[]
}

export type FunctionArguments = {
  mandatoryArguments: string[]
  restArgument?: string
  bindings: BindingNode[]
}

export function assertNameNotDefined<T>(
  name: T,
  contextStack: ContextStack,
  builtin: Builtin,
  debugInfo?: DebugInfo,
): asserts name is T {
  if (typeof name !== `string`) {
    return
  }
  if (builtin.specialExpressions[name]) {
    throw new LitsError(`Cannot define variable ${name}, it's a special expression.`, debugInfo)
  }

  if (builtin.normalExpressions[name]) {
    throw new LitsError(`Cannot define variable ${name}, it's a builtin function.`, debugInfo)
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  if ((reservedNamesRecord as any)[name]) {
    throw new LitsError(`Cannot define variable ${name}, it's a reserved name.`, debugInfo)
  }

  if (contextStack.globalContext[name]) {
    throw new LitsError(`Name already defined "${name}".`, debugInfo)
  }
}
