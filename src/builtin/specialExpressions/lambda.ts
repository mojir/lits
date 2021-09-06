import {
  AstNode,
  functionSymbol,
  LispishFunction,
  NameNode,
  SpecialExpressionNode,
  UserDefinedLispishFunction,
} from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface LambdaSpecialExpressionNode extends SpecialExpressionNode {
  name: 'lambda'
  arguments: NameNode[]
  body: AstNode[]
}

export const lambdaSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
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

    const node: LambdaSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'lambda',
      params: [],
      arguments: functionArguments,
      body,
    }

    return [position + 1, node]
  },
  evaluate: (node): UserDefinedLispishFunction => {
    assertLambdaExpressionNode(node)
    const lispishFunction: LispishFunction = {
      [functionSymbol]: true,
      name: undefined,
      arguments: node.arguments.map(arg => arg.value),
      body: node.body,
    }

    return lispishFunction
  },
  validate: node => {
    assertLambdaExpressionNode(node)
  },
}

function assertLambdaExpressionNode(node: SpecialExpressionNode): asserts node is LambdaSpecialExpressionNode {
  if (node.name !== 'lambda') {
    throw Error('Expected lambda special expression node')
  }
}
