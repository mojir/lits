import { Token } from '../tokenizer/interface'
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
  NormalExpressionNodeWithName,
  ParseBinding,
} from './interface'
import { builtin } from '../builtin'
import { ReservedName } from '../reservedNames'
import { LitsError } from '../errors'
import { FnSpecialExpressionNode } from '../builtin/specialExpressions/functions'
import { FunctionArguments } from '../builtin/utils'
import { assertEventNumberOfParams, assertValue, asValue, expressionNode, nameNode, token } from '../utils/assertion'
import { valueToString } from '../utils/helpers'

type ParseNumber = (tokens: Token[], position: number) => [number, NumberNode]
export const parseNumber: ParseNumber = (tokens: Token[], position: number) => {
  const tkn = token.as(tokens[position], `EOF`)
  return [position + 1, { type: `Number`, value: Number(tkn.value), token: tkn }]
}

type ParseString = (tokens: Token[], position: number) => [number, StringNode]
export const parseString: ParseString = (tokens: Token[], position: number) => {
  const tkn = token.as(tokens[position], `EOF`)
  return [position + 1, { type: `String`, value: tkn.value, token: tkn }]
}

type ParseName = (tokens: Token[], position: number) => [number, NameNode]
export const parseName: ParseName = (tokens: Token[], position: number) => {
  const tkn = token.as(tokens[position], `EOF`)
  return [position + 1, { type: `Name`, value: tkn.value, token: tkn }]
}

type ParseReservedName = (tokens: Token[], position: number) => [number, ReservedNameNode]
export const parseReservedName: ParseReservedName = (tokens: Token[], position: number) => {
  const tkn = token.as(tokens[position], `EOF`)
  return [position + 1, { type: `ReservedName`, value: tkn.value as ReservedName, token: tkn }]
}

const parseTokens: ParseTokens = (tokens, position) => {
  let tkn = token.as(tokens[position], `EOF`)
  const astNodes: AstNode[] = []
  let astNode: AstNode
  while (!(tkn.value === `)` || tkn.value === `]`)) {
    ;[position, astNode] = parseToken(tokens, position)
    astNodes.push(astNode)
    tkn = token.as(tokens[position], `EOF`)
  }
  return [position, astNodes]
}

const parseExpression: ParseExpression = (tokens, position) => {
  position += 1 // Skip parenthesis

  const tkn = token.as(tokens[position], `EOF`)
  if (tkn.type === `name` && builtin.specialExpressions[tkn.value]) {
    return parseSpecialExpression(tokens, position)
  }
  return parseNormalExpression(tokens, position)
}

type ParseArrayLitteral = (tokens: Token[], position: number) => [number, AstNode]
const parseArrayLitteral: ParseArrayLitteral = (tokens, position) => {
  const firstToken = token.as(tokens[position], `EOF`)
  position = position + 1

  let tkn = token.as(tokens[position], `EOF`)
  const params: AstNode[] = []
  let param: AstNode
  while (!(tkn.type === `paren` && tkn.value === `]`)) {
    ;[position, param] = parseToken(tokens, position)
    params.push(param)
    tkn = token.as(tokens[position], `EOF`)
  }

  position = position + 1

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `array`,
    params,
    token: firstToken,
  }

  return [position, node]
}

type ParseObjectLitteral = (tokens: Token[], position: number) => [number, NormalExpressionNodeWithName]
const parseObjectLitteral: ParseObjectLitteral = (tokens, position) => {
  const firstToken = token.as(tokens[position], `EOF`)
  position = position + 1

  let tkn = token.as(tokens[position], `EOF`)
  const params: AstNode[] = []
  let param: AstNode
  while (!(tkn.type === `paren` && tkn.value === `}`)) {
    ;[position, param] = parseToken(tokens, position)
    params.push(param)
    tkn = token.as(tokens[position], `EOF`)
  }

  position = position + 1

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `object`,
    params,
    token: firstToken,
  }

  assertEventNumberOfParams(node)

  return [position, node]
}

type ParseRegexpShorthand = (tokens: Token[], position: number) => [number, NormalExpressionNodeWithName]
const parseRegexpShorthand: ParseRegexpShorthand = (tokens, position) => {
  const tkn = token.as(tokens[position], `EOF`)
  const stringNode: StringNode = {
    type: `String`,
    value: tkn.value,
    token: tkn,
  }

  assertValue(tkn.options, tkn.sourceCodeInfo)

  const optionsNode: StringNode = {
    type: `String`,
    value: `${tkn.options.g ? `g` : ``}${tkn.options.i ? `i` : ``}`,
    token: tkn,
  }

  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: `regexp`,
    params: [stringNode, optionsNode],
    token: tkn,
  }

  return [position + 1, node]
}

const placeholderRegexp = /^%([1-9][0-9]?$)/
type ParseFnShorthand = (tokens: Token[], position: number) => [number, FnSpecialExpressionNode]
const parseFnShorthand: ParseFnShorthand = (tokens, position) => {
  const firstToken = token.as(tokens[position], `EOF`)

  position += 2
  const [newPosition, normalExpressionNode] = parseNormalExpression(tokens, position)

  let arity = 0
  for (let pos = position + 1; pos < newPosition - 1; pos += 1) {
    const tkn = token.as(tokens[pos], `EOF`)
    if (tkn.type === `name`) {
      const match = placeholderRegexp.exec(tkn.value)
      if (match) {
        arity = Math.max(arity, Number(match[1]))
        if (arity > 20) {
          throw new LitsError(`Can't specify more than 20 arguments`, firstToken.sourceCodeInfo)
        }
      }
    }
    if (tkn.type === `fnShorthand`) {
      throw new LitsError(`Nested shortcut functions are not allowed`, firstToken.sourceCodeInfo)
    }
  }

  const mandatoryArguments: string[] = []

  for (let i = 1; i <= arity; i += 1) {
    mandatoryArguments.push(`%${i}`)
  }

  const args: FunctionArguments = {
    bindings: [],
    mandatoryArguments,
  }

  const node: FnSpecialExpressionNode = {
    type: `SpecialExpression`,
    name: `fn`,
    params: [],
    overloads: [
      {
        arguments: args,
        body: [normalExpressionNode],
        arity: args.mandatoryArguments.length,
      },
    ],
    token: firstToken,
  }

  return [newPosition, node]
}

const parseArgument: ParseArgument = (tokens, position) => {
  const tkn = token.as(tokens[position], `EOF`)
  if (tkn.type === `name`) {
    return [position + 1, { type: `Argument`, name: tkn.value, token: tkn }]
  } else if (tkn.type === `modifier`) {
    const value = tkn.value as ModifierName
    return [position + 1, { type: `Modifier`, value, token: tkn }]
  } else {
    throw new LitsError(`Expected name or modifier token, got ${valueToString(tkn)}.`, tkn.sourceCodeInfo)
  }
}

const parseBindings: ParseBindings = (tokens, position) => {
  let tkn = token.as(tokens[position], `EOF`, { type: `paren`, value: `[` })
  position += 1
  tkn = token.as(tokens[position], `EOF`)
  const bindings: BindingNode[] = []
  let binding: BindingNode
  while (!(tkn.type === `paren` && tkn.value === `]`)) {
    ;[position, binding] = parseBinding(tokens, position)
    bindings.push(binding)
    tkn = token.as(tokens[position], `EOF`)
  }
  position += 1

  return [position, bindings]
}

const parseBinding: ParseBinding = (tokens, position) => {
  const firstToken = token.as(tokens[position], `EOF`, { type: `name` })
  const name = firstToken.value

  position += 1
  let value: AstNode
  ;[position, value] = parseToken(tokens, position)

  const node: BindingNode = {
    type: `Binding`,
    name,
    value,
    token: firstToken,
  }
  return [position, node]
}

const parseNormalExpression: ParseNormalExpression = (tokens, position) => {
  const [newPosition, fnNode] = parseToken(tokens, position)

  let params: AstNode[]
  ;[position, params] = parseTokens(tokens, newPosition)
  position += 1

  if (expressionNode.is(fnNode)) {
    const node: NormalExpressionNode = {
      type: `NormalExpression`,
      expression: fnNode,
      params,
      token: fnNode.token,
    }

    return [position, node]
  }

  nameNode.assert(fnNode, fnNode.token.sourceCodeInfo)
  const node: NormalExpressionNode = {
    type: `NormalExpression`,
    name: fnNode.value,
    params,
    token: fnNode.token,
  }

  const builtinExpression = builtin.normalExpressions[node.name]

  if (builtinExpression) {
    builtinExpression.validate?.(node)
  }

  return [position, node]
}

const parseSpecialExpression: ParseSpecialExpression = (tokens, position) => {
  const { value: expressionName, sourceCodeInfo } = token.as(tokens[position], `EOF`)
  position += 1

  const { parse, validate } = asValue(builtin.specialExpressions[expressionName], sourceCodeInfo)

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
  const tkn = token.as(tokens[position], `EOF`)
  let nodeDescriptor: [number, AstNode] | undefined = undefined
  switch (tkn.type) {
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
      if (tkn.value === `(`) {
        nodeDescriptor = parseExpression(tokens, position)
      } else if (tkn.value === `[`) {
        nodeDescriptor = parseArrayLitteral(tokens, position)
      } else if (tkn.value === `{`) {
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
    throw new LitsError(`Unrecognized token: ${tkn.type} value=${tkn.value}`, tkn.sourceCodeInfo)
  }
  return nodeDescriptor
}
