import { Token } from '../tokenizer/interface'
import { asNotUndefined, assertExpressionNode, assertLengthEven } from '../utils'
import {
  AstNode,
  NormalExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  ParseExpression,
  ParseTokens,
  ParseToken,
  ParseSpecialExpression,
  ParseNormalExpression,
  ParseExpressionExpression,
  ParseArgument,
  BindingNode,
  ModifierName,
  ExpressionExpressionNode,
  ParseBindings,
} from './interface'
import { builtin } from '../builtin'
import { ReservedName } from '../reservedNames'
import { UnexpectedTokenError } from '../errors'

type ParseNumber = (tokens: Token[], position: number) => [number, NumberNode]
export const parseNumber: ParseNumber = (tokens: Token[], position: number) => {
  const token = asNotUndefined(tokens[position])
  return [position + 1, { type: `Number`, value: Number(token.value) }]
}

type ParseString = (tokens: Token[], position: number) => [number, StringNode]
export const parseString: ParseString = (tokens: Token[], position: number) => {
  const token = asNotUndefined(tokens[position])
  return [position + 1, { type: `String`, value: token.value }]
}

type ParseName = (tokens: Token[], position: number) => [number, NameNode]
export const parseName: ParseName = (tokens: Token[], position: number) => {
  const token = asNotUndefined(tokens[position])
  return [position + 1, { type: `Name`, value: token.value }]
}

type ParseReservedName = (tokens: Token[], position: number) => [number, ReservedNameNode]
export const parseReservedName: ParseReservedName = (tokens: Token[], position: number) => {
  const token = asNotUndefined(tokens[position])
  return [position + 1, { type: `ReservedName`, value: token.value as ReservedName }]
}

const parseTokens: ParseTokens = (tokens, position) => {
  let token = asNotUndefined(tokens[position])
  const astNodes: AstNode[] = []
  let astNode: AstNode
  while (!(token.type === `paren` && (token.value === `)` || token.value === `]`))) {
    ;[position, astNode] = parseToken(tokens, position)
    astNodes.push(astNode)
    token = asNotUndefined(tokens[position])
  }
  return [position, astNodes]
}

const parseExpression: ParseExpression = (tokens, position) => {
  position += 1 // Skip parenthesis

  const token = asNotUndefined(tokens[position])
  if (token.type === `name`) {
    const expressionName = token.value
    if (builtin.specialExpressions[expressionName]) {
      return parseSpecialExpression(tokens, position)
    }
    return parseNormalExpression(tokens, position)
  }
  return parseExpressionExpression(tokens, position)
}

type ParseArrayLitteral = (tokens: Token[], position: number) => [number, AstNode]
const parseArrayLitteral: ParseArrayLitteral = (tokens, position) => {
  position = position + 1

  let token = asNotUndefined(tokens[position])
  const params: AstNode[] = []
  let param: AstNode
  while (!(token.type === `paren` && token.value === `]`)) {
    ;[position, param] = parseToken(tokens, position)
    params.push(param)
    token = asNotUndefined(tokens[position])
  }

  position = position + 1

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `array`,
    params,
  }

  return [position, node]
}

type ParseObjectLitteral = (tokens: Token[], position: number) => [number, AstNode]
const parseObjectLitteral: ParseObjectLitteral = (tokens, position) => {
  position = position + 1

  let token = asNotUndefined(tokens[position])
  const params: AstNode[] = []
  let param: AstNode
  while (!(token.type === `paren` && token.value === `}`)) {
    ;[position, param] = parseToken(tokens, position)
    params.push(param)
    token = asNotUndefined(tokens[position])
  }

  position = position + 1

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `object`,
    params,
  }

  assertLengthEven(node)

  return [position, node]
}

const parseArgument: ParseArgument = (tokens, position) => {
  let token = asNotUndefined(tokens[position])
  if (token.type === `name`) {
    return [position + 1, { type: `Argument`, name: token.value }]
  } else if (token.type === `paren` && token.value === `(`) {
    position += 1
    token = asNotUndefined(tokens[position])
    if (token.type !== `name`) {
      throw new UnexpectedTokenError(`name`, token)
    }
    const name = token.value
    position += 1
    const [newPosition, defaultValue] = parseToken(tokens, position)
    token = asNotUndefined(tokens[newPosition])
    if (!(token.type === `paren` && token.value === `)`)) {
      throw new UnexpectedTokenError(`)`, token)
    }
    return [newPosition + 1, { type: `Argument`, name, defaultValue }]
  } else if (token.type === `modifier`) {
    const value = token.value as ModifierName
    return [position + 1, { type: `Modifier`, value }]
  } else {
    throw new UnexpectedTokenError(`"(", name or modifier`, token)
  }
}

const parseBindings: ParseBindings = (tokens, position) => {
  let token = asNotUndefined(tokens[position])
  if (!(token.type === `paren` && token.value === `[`)) {
    throw new UnexpectedTokenError(`[`, token)
  }
  position += 1
  token = asNotUndefined(tokens[position])
  const bindings: BindingNode[] = []
  let binding: BindingNode
  while (!(token.type === `paren` && token.value === `]`)) {
    ;[position, binding] = parseBinding(tokens, position)
    bindings.push(binding)
    token = asNotUndefined(tokens[position])
  }
  position += 1

  return [position, bindings]
}

function parseBinding(tokens: Token[], position: number): [number, BindingNode] {
  let token = asNotUndefined(tokens[position])
  if (token.type !== `name`) {
    throw Error(`Expected name node in binding, got ${token.type} value=${token.value}`)
  }
  const name = token.value

  position += 1
  token = asNotUndefined(tokens[position])
  let value: AstNode
  ;[position, value] = parseToken(tokens, position)

  const node: BindingNode = {
    type: `Binding`,
    name,
    value,
  }
  return [position, node]
}

const parseExpressionExpression: ParseExpressionExpression = (tokens, position) => {
  const [newPosition1, expression] = parseToken(tokens, position)

  assertExpressionNode(expression)

  const [newPosition2, params] = parseTokens(tokens, newPosition1)

  const node: ExpressionExpressionNode = {
    type: `ExpressionExpression`,
    expression,
    params,
  }

  return [newPosition2 + 1, node]
}

const parseNormalExpression: ParseNormalExpression = (tokens, position) => {
  const expressionName = asNotUndefined(tokens[position]).value

  const [newPosition, params] = parseTokens(tokens, position + 1)
  position = newPosition + 1

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: expressionName,
    params,
  }

  const builtinExpression = builtin.normalExpressions[node.name]

  if (builtinExpression) {
    builtinExpression.validate?.(node)
  }

  return [position, node]
}

const parseSpecialExpression: ParseSpecialExpression = (tokens, position) => {
  const expressionName = asNotUndefined(tokens[position]).value
  position += 1

  const { parse, validate } = asNotUndefined(
    builtin.specialExpressions[expressionName],
    `${expressionName} is not a built in special expression`,
  )

  const [positionAfterParse, node] = parse(tokens, position, {
    parseExpression,
    parseTokens,
    parseToken,
    parseBindings,
    parseArgument,
  })

  validate?.(node)

  return [positionAfterParse, node]
}

export const parseToken: ParseToken = (tokens, position) => {
  const token = asNotUndefined(tokens[position])
  let nodeDescriptor: [number, AstNode] | undefined = undefined
  switch (token.type) {
    case `number`:
      nodeDescriptor = parseNumber(tokens, position)
      break
    case `string`:
      nodeDescriptor = parseString(tokens, position)
      break
    case `name`:
      nodeDescriptor = parseName(tokens, position)
      break
    case `reservedName`:
      nodeDescriptor = parseReservedName(tokens, position)
      break
    case `paren`:
      if (token.value === `(`) {
        nodeDescriptor = parseExpression(tokens, position)
      } else if (token.value === `[`) {
        nodeDescriptor = parseArrayLitteral(tokens, position)
      } else if (token.value === `{`) {
        nodeDescriptor = parseObjectLitteral(tokens, position)
      }
      break
  }
  if (!nodeDescriptor) {
    throw SyntaxError(`Unrecognized token: ${token.type} value=${token.value}`)
  }
  return nodeDescriptor
}
