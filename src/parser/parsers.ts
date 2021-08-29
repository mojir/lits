import { Token } from '../tokenizer/interface'
import { notUndefined } from '../utils'
import { AstNode, NormalExpressionNode, SpecialExpressionNode, NameNode, NumberNode, StringNode } from './interface'
import { specialExpression, builtInFunction } from '../builtin'

type ParseNumber = (tokens: Token[], position: number) => [number, NumberNode]
export const parseNumber: ParseNumber = (tokens: Token[], position: number) => {
  const token = notUndefined(tokens[position])
  return [position + 1, { type: 'Number', value: Number(token.value) }]
}

type ParseString = (tokens: Token[], position: number) => [number, StringNode]
export const parseString: ParseString = (tokens: Token[], position: number) => {
  const token = notUndefined(tokens[position])
  return [position + 1, { type: 'String', value: token.value }]
}

type ParseName = (tokens: Token[], position: number) => [number, NameNode]
export const parseName: ParseName = (tokens: Token[], position: number) => {
  const token = notUndefined(tokens[position])
  return [position + 1, { type: 'Name', value: token.value }]
}

type ExpressionNode = NormalExpressionNode | SpecialExpressionNode
type ParseExpression = (tokens: Token[], position: number) => [number, ExpressionNode]
export const parseExpression: ParseExpression = (tokens, position) => {
  position += 1 // Skip parenthesis - end of let bindingshesis
  let token = notUndefined(tokens[position])
  let node: ExpressionNode
  position += 1

  const specialExpressionParser = specialExpression[token.value]?.parse
  if (specialExpressionParser) {
    const [newPosition, specialExpressionNode] = specialExpressionParser(tokens, position)
    node = specialExpressionNode
    position = newPosition
  } else {
    node = {
      type: 'NormalExpression',
      name: token.value,
      params: [],
    }
  }
  token = notUndefined(tokens[position])
  while (!(token.type === 'paren' && token.value === ')')) {
    const [newPosition, param] = parseToken(tokens, position)
    position = newPosition
    node.params.push(param)
    token = notUndefined(tokens[position])
  }
  position += 1
  if (node.type === 'NormalExpression') {
    try {
      builtInFunction[node.name]?.validate?.(node)
    } catch (e) {
      if (e instanceof Error) {
        throw Error(e.message + '\n' + JSON.stringify(node, null, 2))
      }
      throw Error(e + '\n' + JSON.stringify(node, null, 2))
    }
  }
  return [position, node]
}

type ParseToken = (tokens: Token[], position: number) => [number, AstNode]
export const parseToken: ParseToken = (tokens, position) => {
  const token = notUndefined(tokens[position])
  let nodeDescriptor: [number, AstNode] | undefined = undefined
  switch (token.type) {
    case 'number':
      nodeDescriptor = parseNumber(tokens, position)
      break
    case 'string':
      nodeDescriptor = parseString(tokens, position)
      break
    case 'name':
      nodeDescriptor = parseName(tokens, position)
      break
    case 'paren':
      if (token.value === '(') {
        nodeDescriptor = parseExpression(tokens, position)
      }
      break
  }
  if (!nodeDescriptor) {
    throw SyntaxError(`Unrecognized token: ${token.type} value=${token.value}`)
  }
  return nodeDescriptor
}
