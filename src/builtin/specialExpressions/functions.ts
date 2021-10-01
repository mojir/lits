import { UnexpectedNodeTypeError, UnexpectedTokenError } from '../../errors'
import { AstNode, functionSymbol, LispishFunction, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertString } from '../../utils'
import { SpecialExpression } from '../interface'
import { FunctionArguments, parseFunctionArguments } from '../utils'

interface DefunSpecialExpressionNode extends SpecialExpressionNode {
  name: 'defun'
  functionName: AstNode
  arguments: FunctionArguments
  body: AstNode[]
}

interface CreateFunctionSpecialExpressionNode extends SpecialExpressionNode {
  name: 'create-function'
  functionName: AstNode
  arguments: FunctionArguments
  body: AstNode[]
}

function createParser(expressionName: 'defun' | 'create-function'): SpecialExpression['parse'] {
  return (tokens, position, { parseToken, parseArgument }) => {
    const [newPosition, functionName] = parseToken(tokens, position)
    if (expressionName === 'defun' && functionName.type !== 'Name') {
      throw new UnexpectedNodeTypeError('Name', functionName)
    }

    position = newPosition

    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw new UnexpectedTokenError(')', token)
    }

    position += 1

    const [nextPosition, functionArguments] = parseFunctionArguments(tokens, position, parseArgument)
    position = nextPosition

    token = asNotUndefined(tokens[position])
    const body: AstNode[] = []
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      body.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    if (body.length === 0) {
      throw Error('Missing body in special expression "defun"')
    }

    const node: DefunSpecialExpressionNode | CreateFunctionSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: expressionName,
      functionName,
      params: [],
      arguments: functionArguments,
      body,
    }

    return [position + 1, node]
  }
}

function createEvaluator(expressionName: 'defun' | 'create-function'): SpecialExpression['evaluate'] {
  return (node, contextStack, evaluateAstNode): undefined => {
    castExpressionNode(node)
    const name =
      expressionName === 'defun'
        ? (node.functionName as NameNode).value
        : evaluateAstNode(node.functionName, contextStack)

    assertString(name)

    const lispishFunction: LispishFunction = {
      [functionSymbol]: true,
      name,
      arguments: node.arguments,
      body: node.body,
    }

    // The second last stack entry is the "global" scope
    const context = asNotUndefined(contextStack[contextStack.length - 2], 'Could not find global scope')

    context.functions[name] = { fun: lispishFunction, constant: false }
    return undefined
  }
}

export const defunSpecialExpression: SpecialExpression = {
  parse: createParser('defun'),
  evaluate: createEvaluator('defun'),
}

export const createFunctionSpecialExpression: SpecialExpression = {
  parse: createParser('create-function'),
  evaluate: createEvaluator('create-function'),
}

function castExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is DefunSpecialExpressionNode | CreateFunctionSpecialExpressionNode {
  return
}
