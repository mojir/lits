import { Token } from '../tokenizer/interface'
import { asNotUndefined } from '../utils'
import {
  AstNode,
  NormalExpressionNode,
  NameNode,
  NumberNode,
  StringNode,
  ReservedNameNode,
  ParseExpression,
  ParseParams,
  ParseToken,
  ParseSpecialExpression,
  ParseNormalExpression,
  ParseExpressionExpression,
  ExpressionExpressionNode,
  ParseBinding,
  ParseArgument,
  BindingNode,
  ModifierName,
} from './interface'
import { builtin } from '../builtin'
import { ReservedName } from '../reservedNames'
import { FunctionSpecialExpressionNode } from '../builtin/specialExpressions/function'
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

const parseParams: ParseParams = (tokens, position) => {
  let token = asNotUndefined(tokens[position])
  const params: AstNode[] = []
  while (!(token.type === `paren` && token.value === `)`)) {
    const [newPosition, param] = parseToken(tokens, position)
    position = newPosition
    params.push(param)
    token = asNotUndefined(tokens[position])
  }
  return [position, params]
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
  } else if (token.type === `paren` && token.value === `(`) {
    return parseExpressionExpression(tokens, position)
  } else {
    throw Error(`Could not parse expression, expected name or "(", got ${token.type}:${token.value}`)
  }
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
    name: `list`,
    params,
  }

  return [position, node]
}

type ParseFunctionShorthand = (tokens: Token[], position: number) => [number, FunctionSpecialExpressionNode]
const parseFunctionShorthand: ParseFunctionShorthand = (tokens, position) => {
  const [newPosition, innerNode] = parseToken(tokens, position + 1)

  const node: FunctionSpecialExpressionNode = {
    type: `SpecialExpression`,
    name: `function`,
    params: [innerNode],
  }
  return [newPosition, node]
}

type ParseListShorthand = (tokens: Token[], position: number) => [number, AstNode]
const parseListShorthand: ParseListShorthand = (tokens, position) => {
  position = position + 2
  let params: AstNode[]
  ;[position, params] = parseParams(tokens, position)

  position = position + 1

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `list`,
    params,
  }

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

const parseBinding: ParseBinding = (tokens, position) => {
  position += 1 // Skip parenthesis

  let token = asNotUndefined(tokens[position])
  if (token.type !== `name`) {
    throw Error(`Expected name node in binding, got ${token.type} value=${token.value}`)
  }
  const name = token.value

  position += 1
  token = asNotUndefined(tokens[position])
  const [newPosition, value] = parseToken(tokens, position)
  position = newPosition

  token = asNotUndefined(tokens[position])
  if (!(token.type === `paren` && token.value === `)`)) {
    throw new UnexpectedTokenError(`)`, token)
  }

  const node: BindingNode = {
    type: `Binding`,
    name,
    value,
  }
  return [position + 1, node]
}

const parseExpressionExpression: ParseExpressionExpression = (tokens, position) => {
  const [newPosition1, expression] = parseExpression(tokens, position)

  const [newPosition2, params] = parseParams(tokens, newPosition1)

  const node: ExpressionExpressionNode = {
    type: `ExpressionExpression`,
    expression,
    params,
  }

  return [newPosition2 + 1, node]
}

const parseNormalExpression: ParseNormalExpression = (tokens, position) => {
  const expressionName = asNotUndefined(tokens[position]).value

  const [newPosition, params] = parseParams(tokens, position + 1)
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
    parseParams,
    parseToken,
    parseBinding,
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
      }
      break
    case `shorthand`:
      if (token.value === `#'`) {
        nodeDescriptor = parseFunctionShorthand(tokens, position)
      } else {
        nodeDescriptor = parseListShorthand(tokens, position)
      }
      break
  }
  if (!nodeDescriptor) {
    throw SyntaxError(`Unrecognized token: ${token.type} value=${token.value}`)
  }
  return nodeDescriptor
}
