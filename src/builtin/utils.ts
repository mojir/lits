import { LitsError } from '../errors'
import type { ContextStack } from '../evaluator/ContextStack'
import type { AstNode, BindingNode } from '../parser/interface'
import { postfixReservedNamesRecord } from '../tokenizer/postfix/postfixReservedNames'
import type { SourceCodeInfo } from '../tokenizer/interface'
import type { Builtin } from './interface'
import type { SpecialExpressionName } from '.'

export type Arity = number | { min: number }

export interface FunctionOverload {
  as: FunctionArguments
  a: Arity
  b: AstNode[]
}

export interface FunctionArguments {
  m: string[]
  r?: string
  b: BindingNode[]
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

  // eslint-disable-next-line ts/no-unsafe-member-access
  if ((postfixReservedNamesRecord as any)[name])
    throw new LitsError(`Cannot define variable ${name}, it's a reserved name.`, sourceCodeInfo)

  if (contextStack.globalContext[name])
    throw new LitsError(`Name already defined "${name}".`, sourceCodeInfo)
}
