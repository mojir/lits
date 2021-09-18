import {
  AstNode,
  functionSymbol,
  LispishFunction,
  SpecialExpressionNode,
  UserDefinedLispishFunction,
} from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'
import { FunctionArguments, parseFunctionArguments } from './utils'

interface LambdaSpecialExpressionNode extends SpecialExpressionNode {
  name: 'lambda'
  arguments: FunctionArguments
  body: AstNode[]
}

export const lambdaSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken, parseArgument }) => {
    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected list of arguments`)
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
      throw Error('Missing lambda body')
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
    castLambdaExpressionNode(node)

    const lispishFunction: LispishFunction = {
      [functionSymbol]: true,
      name: undefined,
      arguments: node.arguments,
      body: node.body,
    }

    return lispishFunction
  },
}

function castLambdaExpressionNode(_node: SpecialExpressionNode): asserts _node is LambdaSpecialExpressionNode {
  return
}
