import {
  AstNode,
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
    case 'ReservedName':
      return evaluateReservedName(node)
    case 'NormalExpression':
      return evaluateNormalExpression(node, contextStack)
    case 'SpecialExpression':
      return evaluateSpecialExpression(node, contextStack)
  }
}

export function optimizeExpression(
  node: NormalExpressionNode | SpecialExpressionNode,
  contextStack: Context[],
): AstNode {
  switch (node.type) {
    case 'NormalExpression':
      return optimizeNormalExpression(node, contextStack)
    case 'SpecialExpression':
      return optimizeSpecialExpression(node, contextStack)
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
  const evaluate = asNotUndefined(builtin.normalExpressions[node.name]).evaluate

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
  const specialExpressionEvaluator = builtin.specialExpressions[node.name]?.evaluate
  if (specialExpressionEvaluator) {
    return specialExpressionEvaluator(node, contextStack)
  }
  throw Error(`Unrecognized special expression node: ${node.name}`)
}

function optimizeNormalExpression(node: NormalExpressionNode, contextStack: Context[]): AstNode {
  if (!node.preEvaluate) {
    return node
  }
  try {
    const value = evaluateNormalExpression(node, contextStack)
    return createNodeFromValue(value)
  } catch {
    return node
  }
}

function optimizeSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): AstNode {
  try {
    const value = evaluateSpecialExpression(node, contextStack)
    return createNodeFromValue(value)
  } catch {
    return node
  }
}

function createNodeFromValue(value: unknown): AstNode {
  if (value === undefined) {
    const node: ReservedNameNode = {
      preEvaluate: true,
      type: 'ReservedName',
      value: 'undefined',
    }
    return node
  }

  if (value === null) {
    const node: ReservedNameNode = {
      preEvaluate: true,
      type: 'ReservedName',
      value: 'null',
    }
    return node
  }

  if (value === true) {
    const node: ReservedNameNode = {
      preEvaluate: true,
      type: 'ReservedName',
      value: 'true',
    }
    return node
  }

  if (value === false) {
    const node: ReservedNameNode = {
      preEvaluate: true,
      type: 'ReservedName',
      value: 'false',
    }
    return node
  }

  if (typeof value === 'number') {
    const node: NumberNode = {
      preEvaluate: true,
      type: 'Number',
      value,
    }
    return node
  }

  if (typeof value === 'string') {
    const node: StringNode = {
      preEvaluate: true,
      type: 'String',
      value,
    }
    return node
  }

  if (Array.isArray(value)) {
    const node: NormalExpressionNode = {
      type: 'NormalExpression',
      name: 'array',
      params: value.map(createNodeFromValue),
      preEvaluate: true,
    }
    return node
  }

  if (typeof value === 'object' && value !== null) {
    const node: NormalExpressionNode = {
      type: 'NormalExpression',
      name: 'object',
      params: Object.entries(value)
        .map(entry => [createNodeFromValue(entry[0]), createNodeFromValue(entry[1])])
        .flat(),
      preEvaluate: true,
    }
    return node
  }

  throw Error(`Could not create node from value: ${value}`)
}
