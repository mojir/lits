import { builtin } from '../builtin'
import type { FnNode } from '../builtin/specialExpressions/functions'
import type { FunctionArguments } from '../builtin/utils'
import { AstNodeType, TokenType } from '../constants/constants'
import { LitsError } from '../errors'
import type { ReservedName } from '../reservedNames'
import type { TokenStream } from '../tokenizer/interface'
import { asNonUndefined, assertEventNumberOfParams, assertNonUndefined, assertUnreachable } from '../typeGuards'
import { assertNameNode, isExpressionNode } from '../typeGuards/astNode'
import { asToken } from '../typeGuards/token'
import { valueToString } from '../utils/debug/debugTools'
import type {
  AstNode,
  BindingNode,
  ModifierName,
  NameNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  ParseArgument,
  ParseExpression,
  ReservedNameNode,
  SpecialExpressionNode,
  StringNode,
} from './interface'

export function parseNumber(tokenStream: TokenStream, position: number): [number, NumberNode] {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  return [position + 1, { t: AstNodeType.Number, v: Number(tkn.v), tkn: tkn.sourceCodeInfo ? tkn : undefined }]
}

function parseString(tokenStream: TokenStream, position: number): [number, StringNode] {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  return [position + 1, { t: AstNodeType.String, v: tkn.v, tkn: tkn.sourceCodeInfo ? tkn : undefined }]
}

function parseName(tokenStream: TokenStream, position: number): [number, NameNode] {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  return [position + 1, { t: AstNodeType.Name, v: tkn.v, tkn: tkn.sourceCodeInfo ? tkn : undefined }]
}

function parseReservedName(tokenStream: TokenStream, position: number): [number, ReservedNameNode] {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  return [
    position + 1,
    { t: AstNodeType.ReservedName, v: tkn.v as ReservedName, tkn: tkn.sourceCodeInfo ? tkn : undefined },
  ]
}

function parseTokens(tokenStream: TokenStream, position: number): [number, AstNode[]] {
  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  const astNodes: AstNode[] = []
  let astNode: AstNode
  while (!(tkn.t === TokenType.Bracket && (tkn.v === ')' || tkn.v === ']'))) {
    ;[position, astNode] = parseToken(tokenStream, position)
    astNodes.push(astNode)
    tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  }
  return [position, astNodes]
}

const parseExpression: ParseExpression = (tokenStream, position) => {
  position += 1 // Skip parenthesis

  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  if (tkn.t === TokenType.Name && builtin.specialExpressions[tkn.v])
    return parseSpecialExpression(tokenStream, position)

  return parseNormalExpression(tokenStream, position)
}

function parseArrayLitteral(tokenStream: TokenStream, position: number): [number, AstNode] {
  const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
  position = position + 1

  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  const params: AstNode[] = []
  let param: AstNode
  while (!(tkn.t === TokenType.Bracket && tkn.v === ']')) {
    ;[position, param] = parseToken(tokenStream, position)
    params.push(param)
    tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  }

  position = position + 1

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'array',
    p: params,
    tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
  }

  return [position, node]
}

function parseObjectLitteral(tokenStream: TokenStream, position: number): [number, NormalExpressionNodeWithName] {
  const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
  position = position + 1

  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  const params: AstNode[] = []
  let param: AstNode
  while (!(tkn.t === TokenType.Bracket && tkn.v === '}')) {
    ;[position, param] = parseToken(tokenStream, position)
    params.push(param)
    tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  }

  position = position + 1

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'object',
    p: params,
    tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
  }

  assertEventNumberOfParams(node)

  return [position, node]
}

function parseRegexpShorthand(tokenStream: TokenStream, position: number): [number, NormalExpressionNodeWithName] {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: tkn.v,
    tkn: tkn.sourceCodeInfo ? tkn : undefined,
  }

  assertNonUndefined(tkn.o, tkn.sourceCodeInfo)

  const optionsNode: StringNode = {
    t: AstNodeType.String,
    v: `${tkn.o.g ? 'g' : ''}${tkn.o.i ? 'i' : ''}`,
    tkn: tkn.sourceCodeInfo ? tkn : undefined,
  }

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'regexp',
    p: [stringNode, optionsNode],
    tkn: tkn.sourceCodeInfo ? tkn : undefined,
  }

  return [position + 1, node]
}

const placeholderRegexp = /^%([1-9][0-9]?)?$/
type ParseFnShorthand = (tokenStream: TokenStream, position: number) => [number, FnNode]
const parseFnShorthand: ParseFnShorthand = (tokenStream, position) => {
  const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)

  position += 1
  const [newPosition, exprNode] = parseExpression(tokenStream, position)

  let arity = 0
  let percent1: 'NOT_SET' | 'WITH_1' | 'NAKED' = 'NOT_SET'
  for (let pos = position + 1; pos < newPosition - 1; pos += 1) {
    const tkn = asToken(tokenStream.tokens[pos], tokenStream.filePath)
    if (tkn.t === TokenType.Name) {
      const match = placeholderRegexp.exec(tkn.v)
      if (match) {
        const number = match[1] ?? '1'
        if (number === '1') {
          const mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED')
          if (mixedPercent1)
            throw new LitsError('Please make up your mind, either use % or %1', firstToken.sourceCodeInfo)

          percent1 = match[1] ? 'WITH_1' : 'NAKED'
        }

        arity = Math.max(arity, Number(number))
        if (arity > 20)
          throw new LitsError('Can\'t specify more than 20 arguments', firstToken.sourceCodeInfo)
      }
    }
    if (tkn.t === TokenType.FnShorthand)
      throw new LitsError('Nested shortcut functions are not allowed', firstToken.sourceCodeInfo)
  }

  const mandatoryArguments: string[] = []

  for (let i = 1; i <= arity; i += 1) {
    if (i === 1 && percent1 === 'NAKED')
      mandatoryArguments.push('%')
    else
      mandatoryArguments.push(`%${i}`)
  }

  const args: FunctionArguments = {
    b: [],
    m: mandatoryArguments,
  }

  const node: FnNode = {
    t: AstNodeType.SpecialExpression,
    n: 'fn',
    p: [],
    o: [
      {
        as: args,
        b: [exprNode],
        a: args.m.length,
      },
    ],
    tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
  }

  return [newPosition, node]
}

const parseArgument: ParseArgument = (tokenStream, position) => {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  if (tkn.t === TokenType.Name) {
    return [position + 1, { t: AstNodeType.Argument, n: tkn.v, tkn }]
  }
  else if (tkn.t === TokenType.Modifier) {
    const value = tkn.v as ModifierName
    return [position + 1, { t: AstNodeType.Modifier, v: value, tkn: tkn.sourceCodeInfo ? tkn : undefined }]
  }
  else {
    throw new LitsError(`Expected name or modifier token, got ${valueToString(tkn)}.`, tkn.sourceCodeInfo)
  }
}

function parseBindings(tokenStream: TokenStream, position: number): [number, BindingNode[]] {
  let tkn = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '[' })
  position += 1
  tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  const bindings: BindingNode[] = []
  let binding: BindingNode
  while (!(tkn.t === TokenType.Bracket && tkn.v === ']')) {
    ;[position, binding] = parseBinding(tokenStream, position)
    bindings.push(binding)
    tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  }
  position += 1

  return [position, bindings]
}

function parseBinding(tokenStream: TokenStream, position: number): [number, BindingNode] {
  const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Name })
  const name = firstToken.v

  position += 1
  let value: AstNode
  ;[position, value] = parseToken(tokenStream, position)

  const node: BindingNode = {
    t: AstNodeType.Binding,
    n: name,
    v: value,
    tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
  }
  return [position, node]
}

function parseNormalExpression(tokenStream: TokenStream, position: number): [number, NormalExpressionNode] {
  const [newPosition, fnNode] = parseToken(tokenStream, position)

  let params: AstNode[]
  ;[position, params] = parseTokens(tokenStream, newPosition)
  position += 1

  if (isExpressionNode(fnNode)) {
    const node: NormalExpressionNode = {
      t: AstNodeType.NormalExpression,
      e: fnNode,
      p: params,
      tkn: fnNode.tkn,
    }

    return [position, node]
  }

  assertNameNode(fnNode, fnNode.tkn?.sourceCodeInfo)
  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: fnNode.v,
    p: params,
    tkn: fnNode.tkn,
  }

  const builtinExpression = builtin.normalExpressions[node.n]

  if (builtinExpression)
    builtinExpression.validate?.(node)

  return [position, node]
}

function parseSpecialExpression(tokenStream: TokenStream, position: number): [number, SpecialExpressionNode] {
  const { v: expressionName, sourceCodeInfo } = asToken(
    tokenStream.tokens[position],
    tokenStream.filePath,
  )
  position += 1

  const { parse, validate } = asNonUndefined(builtin.specialExpressions[expressionName], sourceCodeInfo)

  const [positionAfterParse, node] = parse(tokenStream, position, {
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

export function parseToken(tokenStream: TokenStream, position: number): [number, AstNode] {
  const tkn = asToken(tokenStream.tokens[position], tokenStream.filePath)
  switch (tkn.t) {
    case TokenType.Number:
      return parseNumber(tokenStream, position)
    case TokenType.String:
      return parseString(tokenStream, position)
    case TokenType.Name:
      return parseName(tokenStream, position)
    case TokenType.ReservedName:
      return parseReservedName(tokenStream, position)
    case TokenType.Bracket:
      if (tkn.v === '(')
        return parseExpression(tokenStream, position)
      else if (tkn.v === '[')
        return parseArrayLitteral(tokenStream, position)
      else if (tkn.v === '{')
        return parseObjectLitteral(tokenStream, position)

      break
    case TokenType.RegexpShorthand:
      return parseRegexpShorthand(tokenStream, position)
    case TokenType.FnShorthand:
      return parseFnShorthand(tokenStream, position)
    case TokenType.CollectionAccessor:
    case TokenType.Modifier:
      break
    /* v8 ignore next 2 */
    default:
      assertUnreachable(tkn.t)
  }
  throw new LitsError(`Unrecognized token: ${tkn.t} value=${tkn.v}`, tkn.sourceCodeInfo)
}
