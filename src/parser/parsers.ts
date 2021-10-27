import { Token } from '../tokenizer/interface'
import { asNotUndefined, assertLengthEven, assertNameNode, assertNotUndefined, isExpressionNode } from '../utils'
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
  ParseArgument,
  BindingNode,
  ModifierName,
  ParseBindings,
  NormalExpressionNodeName,
  ParseBinding,
} from './interface'
import { builtin } from '../builtin'
import { ReservedName } from '../reservedNames'
import { UnexpectedTokenError } from '../errors'
import { FnSpecialExpressionNode } from '../builtin/specialExpressions/functions'
import { FunctionArguments } from '../builtin/utils'

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
  if (token.type === `name` && builtin.specialExpressions[token.value]) {
    return parseSpecialExpression(tokens, position)
  }
  return parseNormalExpression(tokens, position)
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

type ParseObjectLitteral = (tokens: Token[], position: number) => [number, NormalExpressionNodeName]
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

type ParseRegexpShorthand = (tokens: Token[], position: number) => [number, NormalExpressionNodeName]
const parseRegexpShorthand: ParseRegexpShorthand = (tokens, position) => {
  const token = asNotUndefined(tokens[position])
  const stringNode: StringNode = {
    type: `String`,
    value: token.value,
  }

  assertNotUndefined(token.options)

  const optionsNode: StringNode = {
    type: `String`,
    value: `${token.options.g ? `g` : ``}${token.options.i ? `i` : ``}`,
  }

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `regexp`,
    params: [stringNode, optionsNode],
  }

  return [position + 1, node]
}

const placeholderRegexp = /^%([1-9][0-9]?$)/
type ParseFnShorthand = (tokens: Token[], position: number) => [number, FnSpecialExpressionNode]
const parseFnShorthand: ParseFnShorthand = (tokens, position) => {
  position += 2
  const [newPosition, normalExpressionNode] = parseNormalExpression(tokens, position)

  let arity = 0
  for (let pos = position + 1; pos < newPosition - 1; pos += 1) {
    const token = asNotUndefined(tokens[pos])
    if (token.type === `name`) {
      const match = placeholderRegexp.exec(token.value)
      if (match) {
        arity = Math.max(arity, Number(match[1]))
        if (arity > 20) {
          throw Error(`Can't specify more than 20 arguments`)
        }
      }
    }
    if (token.type === `fnShorthand`) {
      throw Error(`Nested shortcut functions are not allowed`)
    }
  }

  const mandatoryArguments: string[] = []

  for (let i = 1; i <= arity; i += 1) {
    mandatoryArguments.push(`%${i}`)
  }

  const args: FunctionArguments = {
    bindings: [],
    mandatoryArguments,
    optionalArguments: [],
  }

  const node: FnSpecialExpressionNode = {
    type: `SpecialExpression`,
    name: `fn`,
    params: [],
    arguments: args,
    body: [normalExpressionNode],
  }

  return [newPosition, node]
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

const parseBinding: ParseBinding = (tokens, position) => {
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

const parseNormalExpression: ParseNormalExpression = (tokens, position) => {
  //  let fnNode: AstNode
  const [newPosition, fnNode] = parseToken(tokens, position)

  let params: AstNode[]
  ;[position, params] = parseTokens(tokens, newPosition)
  position += 1

  if (isExpressionNode(fnNode)) {
    const node: NormalExpressionNode = {
      type: `NormalExpression`,
      expression: fnNode,
      params,
    }

    return [position, node]
  }

  assertNameNode(fnNode)
  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: fnNode.value,
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

  const { parse, validate } = asNotUndefined(builtin.specialExpressions[expressionName])

  const [positionAfterParse, node] = parse(tokens, position, {
    parseExpression,
    parseTokens,
    parseToken,
    parseBinding,
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
    case `regexpShorthand`:
      nodeDescriptor = parseRegexpShorthand(tokens, position)
      break
    case `fnShorthand`:
      nodeDescriptor = parseFnShorthand(tokens, position)
      break
  }
  if (!nodeDescriptor) {
    throw SyntaxError(`Unrecognized token: ${token.type} value=${token.value}`)
  }
  return nodeDescriptor
}
