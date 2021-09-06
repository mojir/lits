import { AstNode, functionSymbol, LispishFunction, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface DefunSpecialExpressionNode extends SpecialExpressionNode {
  name: 'defun'
  functionName: NameNode
  arguments: NameNode[]
  body: AstNode[]
}

export const defunSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const [newPosition, functionName] = parseToken(tokens, position)
    if (functionName.type !== 'Name') {
      throw Error('Expected a name node')
    }

    position = newPosition

    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected list of arguments`)
    }

    position += 1

    token = asNotUndefined(tokens[position])
    const functionArguments: NameNode[] = []
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, nameNode] = parseToken(tokens, position)
      if (nameNode.type !== 'Name') {
        throw Error('Expected a name node')
      }
      functionArguments.push(nameNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    position += 1

    token = asNotUndefined(tokens[position])
    const body: AstNode[] = []
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      body.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }

    const node: DefunSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'defun',
      functionName,
      params: [],
      arguments: functionArguments,
      body,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, _evaluateAstNode): undefined => {
    assertDefunExpressionNode(node)
    const lispishFunction: LispishFunction = {
      [functionSymbol]: true,
      name: node.functionName.value,
      arguments: node.arguments.map(arg => arg.value),
      body: node.body,
    }

    // The second last stack entry is the "global" scope
    const context = contextStack[contextStack.length - 2]
    if (!context) {
      throw Error(`Couldn't find global context`)
    }

    context.functions[node.functionName.value] = lispishFunction
    return undefined
  },
  validate: node => {
    assertDefunExpressionNode(node)
  },
}

function assertDefunExpressionNode(node: SpecialExpressionNode): asserts node is DefunSpecialExpressionNode {
  if (node.name !== 'defun') {
    throw Error('Expected defun special expression node')
  }
}
