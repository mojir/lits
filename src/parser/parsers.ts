import { Token } from '../tokenizer/Tokenizer.types'
import { assertToken } from '../utils'
import { AstNode, BasicExpressionNode, LetExpressionNode, NameNode, NumberNode, StringNode } from './Parser.types'
import { stdLibValidators } from '../stdLib/stdLib'

type ParseNumber = (tokens: Token[], position: number) => [number, NumberNode]
export const parseNumber: ParseNumber = (tokens: Token[], position: number) => {
  const token = assertToken(tokens[position], position)
  return [position + 1, { type: 'Number', value: Number(token.value) }]
}

type ParseString = (tokens: Token[], position: number) => [number, StringNode]
export const parseString: ParseString = (tokens: Token[], position: number) => {
  const token = assertToken(tokens[position], position)
  return [position + 1, { type: 'String', value: token.value }]
}

type ParseName = (tokens: Token[], position: number) => [number, NameNode]
export const parseName: ParseName = (tokens: Token[], position: number) => {
  const token = assertToken(tokens[position], position)
  return [position + 1, { type: 'Name', value: token.value }]
}

type ExpressionNode = BasicExpressionNode | LetExpressionNode
type ParseExpression = (tokens: Token[], position: number) => [number, ExpressionNode]
export const parseExpression: ParseExpression = (tokens, position) => {
  position += 1 // Skip parenthesis - end of let bindingshesis
  let token = assertToken(tokens[position], position)
  let node: ExpressionNode
  position += 1

  if (token.value === 'let') {
    node = {
      type: 'LetExpression',
      params: [],
      bindings: [],
    }
    token = assertToken(tokens[position], position)
    if (!(token.type === 'paren' && token.value === '(')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected list of bindings`)
    }
    position += 1
    while (!(token.type === 'paren' && token.value === ')')) {
      if (!(token.type === 'paren' && token.value === '(')) {
        throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected an expression`)
      }
      const [newPosition, param] = parseExpression(tokens, position)
      if (param.type === 'LetExpression') {
        throw Error('Expected a binding expression')
      }
      position = newPosition
      node.bindings.push(param)
      token = assertToken(tokens[position], position)
    }
    position += 1 // skip right parenthesis - end of let bindings
  } else {
    node = {
      type: 'BasicExpression',
      name: token.value,
      params: [],
    }
  }
  token = assertToken(tokens[position], position)
  while (!(token.type === 'paren' && token.value === ')')) {
    const [newPosition, param] = parseToken(tokens, position)
    position = newPosition
    node.params.push(param)
    token = assertToken(tokens[position], position)
  }
  position += 1
  if (node.type === 'BasicExpression') {
    try {
      stdLibValidators[node.name]?.(node)
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
  const token = assertToken(tokens[position], position)
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
