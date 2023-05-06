import { builtin } from '../builtin'
import { ContextStack } from '../ContextStack'
import { BuiltinFunction, FUNCTION_SYMBOL, NameNode } from '../parser/interface'
import { LookUpResult } from './interface'

export function lookUp(node: NameNode, contextStack: ContextStack): LookUpResult {
  const value = node.value
  const debugInfo = node.token?.debugInfo

  for (const context of contextStack.stack) {
    const variable = context[value]
    if (variable) {
      return {
        builtinFunction: null,
        contextEntry: variable,
        specialExpression: null,
      }
    }
  }
  if (builtin.normalExpressions[value]) {
    const builtinFunction: BuiltinFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo,
      type: `builtin`,
      name: value,
    }
    return {
      builtinFunction,
      contextEntry: null,
      specialExpression: null,
    }
  }

  if (builtin.specialExpressions[value]) {
    return {
      specialExpression: true,
      builtinFunction: null,
      contextEntry: null,
    }
  }

  return {
    specialExpression: null,
    builtinFunction: null,
    contextEntry: null,
  }
}
