import {
  AstNode,
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
} from '../parser/interface'
import get from 'lodash/get'
import { Ast } from '../parser/interface'
import { normalExpressions, specialExpressions } from '../builtin'
import { reservedName } from '../reservedName'
import { asNotUndefined } from '../utils'
export type Context = Record<string, unknown>

export function evaluateProgram(ast: Ast, globalContext: Context): unknown {
  let result: unknown
  const contextStack = [{}, globalContext]
  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }
  return result
}

export function evaluateAstNode(node: AstNode, contextStack: Context[]): unknown {
  switch (node.type) {
    case 'Number':
      return evaluateNumber(node)
    case 'String':
      return evaluateString(node)
    case 'Name':
      return evaluateName(node, contextStack)
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

function evaluateName(node: NameNode, contextStack: Context[]): unknown {
  const keyWord = reservedName[node.value]
  if (keyWord) {
    return keyWord.value
  }

  const path = node.value
  const dotPosition = path.indexOf('.')
  const bracketPosition = path.indexOf('[')
  const index =
    dotPosition === -1 ? bracketPosition : bracketPosition === -1 ? dotPosition : Math.min(dotPosition, bracketPosition)
  const contextPrefix = index === -1 ? path : path.substring(0, index)
  for (const context of contextStack) {
    if (context[contextPrefix] !== undefined) {
      return get(context, path)
    }
  }
  throw Error(`Undefined identifier ${path}`)
}

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): unknown {
  const evaluate = asNotUndefined(normalExpressions[node.name]).evaluate

  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  try {
    return evaluate(params)
  } catch (e: unknown) {
    if (e instanceof Error) {
      throw Error(e.message + '\n' + JSON.stringify(node, null, 2))
    }
    throw Error(e + '\n' + JSON.stringify(node, null, 2))
  }
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): unknown {
  const specialExpressionEvaluator = specialExpressions[node.name]?.evaluate
  if (specialExpressionEvaluator) {
    return specialExpressionEvaluator(node, contextStack)
  }
  throw Error(`Unrecognized special expression node: ${node.name}`)
}
