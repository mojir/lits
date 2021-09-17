import { NameNode, ParseToken, ModifierNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined } from '../../utils'

export function parseFunctionArguments(
  tokens: Token[],
  position: number,
  parseToken: ParseToken,
): [number, Array<NameNode | ModifierNode>, number | undefined] {
  let token = asNotUndefined(tokens[position])
  const functionArguments: Array<NameNode | ModifierNode> = []
  let restIsUsed = false
  const argNames: Record<string, true> = {}
  let optionalParamsIndex: number | undefined = undefined
  let index = -1
  while (!(token.type === 'paren' && token.value === ')')) {
    index += 1
    const [newPosition, argumentNode] = parseToken(tokens, position)

    if (restIsUsed) {
      throw Error('&rest must be the last argument')
    } else {
      if (argumentNode.type !== 'Name' && argumentNode.type !== 'Modifier') {
        throw Error('Expected a name node or a modifier node')
      }
    }

    if (argNames[argumentNode.value]) {
      throw Error(`Duplicate argument name '${argumentNode.value}'`)
    } else {
      argNames[argumentNode.value] = true
    }

    if (argumentNode.type === 'Modifier') {
      if (argumentNode.value === '&rest') {
        restIsUsed = true
      }
      if (argumentNode.value === '&optional') {
        if (optionalParamsIndex !== undefined) {
          throw Error('Only one &optional modifier allowed')
        }
        optionalParamsIndex = index
      }
    }
    functionArguments.push(argumentNode)
    position = newPosition
    token = asNotUndefined(tokens[position])
  }
  position += 1
  return [position, functionArguments, optionalParamsIndex]
}
