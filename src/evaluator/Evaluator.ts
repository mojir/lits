import {
  AstNode,
  NormalExpressionNode,
  SpecialExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
} from '../parser/Parser.types'
import get from 'lodash/get'
import { Ast } from '../parser/Parser.types'
import { stdLibEvaluators } from '../stdLib/stdLib'
export type Context = Record<string, EvaluationResult>

const reservedName: Record<string, () => unknown> = {
  true: () => true,
  false: () => false,
  null: () => null,
  undefined: () => undefined,
}

type EvaluationResult = unknown

export function evaluateAst(ast: Ast, globalContext: Context): EvaluationResult {
  let result: EvaluationResult
  const contextStack = [{}, globalContext]
  for (const node of ast.body) {
    result = evaluateAstNode(node, contextStack)
  }
  return result
}

function evaluateAstNode(node: AstNode, contextStack: Context[]): EvaluationResult {
  switch (node.type) {
    case 'Number':
      return evaluateNumber(node)
    case 'String':
      return evaluateString(node)
    case 'NormalExpression':
      return evaluateNormalExpression(node, contextStack)
    case 'SpecialExpression':
      return evaluateSpecialExpression(node, contextStack)
    case 'Name':
      return evaluateName(node, contextStack)
  }
}

function evaluateNumber(node: NumberNode): number {
  return node.value
}

function evaluateString(node: StringNode): string {
  return node.value
}

function evaluateName(node: NameNode, contextStack: Context[]): unknown {
  const keyWordFn = reservedName[node.value]
  if (keyWordFn) {
    return keyWordFn()
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

function evaluateNormalExpression(node: NormalExpressionNode, contextStack: Context[]): EvaluationResult {
  const func = stdLibEvaluators[node.name]
  if (!func) {
    throw Error(`Unrecognized name ${node.name}`)
  }

  // Special case, we don't want to evaluate parameter until we know what if-branch to evaluate
  if (node.name === 'if') {
    return evaluateAstNode(getIfNode(node.params, contextStack), contextStack)
  }

  // Special case, setq should set data on global scope
  if (node.name === 'setq') {
    const name = assertNameNode(node.params[0]).value
    if (reservedName[name]) {
      throw SyntaxError(`Cannot set symbol name to "${name}", it's a reserved name`)
    }

    const value = evaluateAstNode(assertAstNode(node.params[1]), contextStack)

    // The second last stack entry is the "global" scope
    const globalContext = contextStack[contextStack.length - 2] as Context
    globalContext[name] = value

    return value
  }
  const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
  try {
    return func(params)
  } catch (e: unknown) {
    if (e instanceof Error) {
      throw Error(e.message + '\n' + JSON.stringify(node, null, 2))
    }
    throw Error(e + '\n' + JSON.stringify(node, null, 2))
  }
}

function evaluateSpecialExpression(node: SpecialExpressionNode, contextStack: Context[]): EvaluationResult {
  const locals: Context = {}
  for (const binding of node.bindings) {
    const bindingNode = binding.params[0]
    if (bindingNode === undefined) {
      throw Error(`binding node undefined`)
    }
    locals[binding.name] = evaluateAstNode(bindingNode, contextStack)
  }
  const astNode = node.params[0]
  if (astNode === undefined) {
    throw Error(`let didn't have a node to evaluate`)
  }

  const newContextStack = [locals, ...contextStack]
  return evaluateAstNode(astNode, newContextStack)
}

function getIfNode(ifNodeParams: AstNode[], contextStack: Context[]): AstNode {
  return evaluateAstNode(assertAstNode(ifNodeParams[0]), contextStack)
    ? assertAstNode(ifNodeParams[1])
    : assertAstNode(ifNodeParams[2])
}

function assertAstNode(node: AstNode | undefined): AstNode {
  if (node === undefined) {
    throw Error('Expected an AST node, got undefined')
  }
  return node
}

function assertNameNode(node: AstNode | undefined): NameNode {
  if (node === undefined || node.type !== 'Name') {
    throw Error('Expected an AST node, got undefined')
  }
  return node
}
