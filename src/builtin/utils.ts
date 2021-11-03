import { LitsError } from '../errors'
import { ContextStack } from '../evaluator/interface'
import { AstNode, BindingNode } from '../parser/interface'
import { reservedNamesRecord } from '../reservedNames'
import { SourceCodeInfo } from '../tokenizer/interface'
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
  sourceCodeInfo: SourceCodeInfo,
): asserts name is T {
  if (typeof name !== `string`) {
    return
  }
  if (builtin.specialExpressions[name]) {
    throw new LitsError(`Cannot define variable ${name}, it's a special expression.`, sourceCodeInfo)
  }

  if (builtin.normalExpressions[name]) {
    throw new LitsError(`Cannot define variable ${name}, it's a builtin function.`, sourceCodeInfo)
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  if ((reservedNamesRecord as any)[name]) {
    throw new LitsError(`Cannot define variable ${name}, it's a reserved name.`, sourceCodeInfo)
  }

  if (contextStack.globalContext[name]) {
    throw new LitsError(`Name already defined "${name}".`, sourceCodeInfo)
  }
}
