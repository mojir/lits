import { NameNode, ParseToken, RestNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined } from '../../utils'

export function parseFunctionArguments(
  tokens: Token[],
  position: number,
  parseToken: ParseToken,
): [number, Array<NameNode | RestNode>] {
  let token = asNotUndefined(tokens[position])
  const functionArguments: Array<NameNode | RestNode> = []
  let restIsUsed = false
  const argNames: Record<string, true> = {}
  while (!(token.type === 'paren' && token.value === ')')) {
    const [newPosition, argumentNode] = parseToken(tokens, position)

    if (restIsUsed) {
      throw Error('&rest must be the last argument')
    } else {
      if (argumentNode.type !== 'Name' && argumentNode.type !== 'Rest') {
        throw Error('Expected a name node')
      }
    }

    if (argNames[argumentNode.value]) {
      throw Error(`Duplicate argument name '${argumentNode.value}'`)
    } else {
      argNames[argumentNode.value] = true
    }

    if (argumentNode.type === 'Rest') {
      restIsUsed = true
    }
    functionArguments.push(argumentNode)
    position = newPosition
    token = asNotUndefined(tokens[position])
  }
  position += 1
  return [position, functionArguments]
}
