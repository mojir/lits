import {
  AstNode,
  functionSymbol,
  LispishFunction,
  NameNode,
  ModifierNode,
  SpecialExpressionNode,
  UserDefinedLispishFunction,
} from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'
import { parseFunctionArguments } from './utils'

interface LambdaSpecialExpressionNode extends SpecialExpressionNode {
  name: 'lambda'
  arguments: Array<NameNode | ModifierNode>
  optionalParamsIndex: number | undefined
  body: AstNode[]
}

export const lambdaSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected list of arguments`)
    }

    position += 1

    const [nextPosition, functionArguments, optionalParamsIndex] = parseFunctionArguments(tokens, position, parseToken)
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
      optionalParamsIndex,
      body,
    }

    return [position + 1, node]
  },
  evaluate: (node): UserDefinedLispishFunction => {
    castLambdaExpressionNode(node)
    const restParams =
      node.arguments[node.arguments.length - 1]?.type === 'Modifier' &&
      node.arguments[node.arguments.length - 1]?.value === '&rest'

    const lispishFunction: LispishFunction = {
      [functionSymbol]: true,
      name: undefined,
      arguments: node.arguments.map(arg => arg.value),
      restParams,
      optionalParamsIndex: node.optionalParamsIndex,
      body: node.body,
    }

    return lispishFunction
  },
}

function castLambdaExpressionNode(_node: SpecialExpressionNode): asserts _node is LambdaSpecialExpressionNode {
  return
}
