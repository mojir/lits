import {
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
} from '../parser/interface'
import get from 'lodash/get'
import { Ast } from '../parser/interface'
import { builtin } from '../builtin'
import { reservedNames } from '../reservedNames'
import { asNotUndefined } from '../utils'
import { Context, EvaluateAstNode } from './interface'

export function evaluate(ast: Ast, context: Context, globalContext: Context): unknown {
  const frozenContext = { ...context }
  Object.freeze(frozenContext)

  // First element is the global context. E.g. setq will assign to this if no local variable is available
  // Second element is the context sent in from outside (this should never be mutated)
  const contextStack = [globalContext, frozenContext]

  let result: unknown
  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }
  return result
}

export const evaluateAstNode: EvaluateAstNode = (node, contextStack) => {
  switch (node.type) {
    case 'Number':
      return evaluateNumber(node)
    case 'String':
      return evaluateString(node)
    case 'Name':
      return evaluateName(node, contextStack)
    case 'ReservedName':
      return evaluateReservedName(node)
    case 'NormalExpression':
      return evaluateNormalExpression(node, contextStack)
    case 'SpecialExpression':
      return evaluateSpecialExpression(node, contextStack)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateReservedName(node: ReservedNameNode): unknown {
  return asNotUndefined(reservedNames[node.value]).value
}

function evaluateName(node: NameNode, contextStack: Context[]): unknown {
  const path = node.value
  const dotPosition = path.indexOf('.')
  const bracketPosition = path.indexOf('[')
  const index =
    dotPosition === -1 ? bracketPosition : bracketPosition === -1 ? dotPosition : Math.min(dotPosition, bracketPosition)
  const contextPrefix = index === -1 ? path : path.substring(0, index)
  for (const context of contextStack) {
    if (Object.getOwnPropertyDescriptor(context, contextPrefix)) {
      return get(context, path)
    }
  }
  throw Error(`Undefined identifier ${path}`)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): unknown {
  const normalExpressionEvaluator = asNotUndefined(builtin.normalExpressions[node.name]).evaluate

  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  try {
    return normalExpressionEvaluator(params)
  } catch (e: unknown) {
    if (e instanceof Error) {
      throw Error(e.message + '\n' + JSON.stringify(node, null, 2))
    }
    throw Error(e + '\n' + JSON.stringify(node, null, 2))
  }
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): unknown {
  const specialExpressionEvaluator = builtin.specialExpressions[node.name]?.evaluate
  if (specialExpressionEvaluator) {
    return specialExpressionEvaluator(node, contextStack, evaluateAstNode)
  }
  throw Error(`Unrecognized special expression node: ${node.name}`)
}
