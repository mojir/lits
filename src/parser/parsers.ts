import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import { builtin } from '../builtin'
import type { FnNode } from '../builtin/specialExpressions/functions'
import type { FunctionArguments } from '../builtin/utils'
import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { withoutCommentNodes } from '../removeCommentNodes'
import type { TokenStream } from '../tokenizer/interface'
import { asNonUndefined, assertEvenNumberOfParams, assertNonUndefined } from '../typeGuards'
import { assertNameNode, isExpressionNode } from '../typeGuards/astNode'
import { asToken } from '../typeGuards/token'
import { valueToString } from '../utils/debug/debugTools'
import type {
  AstNode,
  BindingNode,
  CommentNode,
  ModifierName,
  NameNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  ParseArgument,
  ParseExpression,
  ParseState,
  ReservedNameNode,
  StringNode,
} from './interface'

export function parseNumber(tokenStream: TokenStream, parseState: ParseState): NumberNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)
  return {
    t: AstNodeType.Number,
    v: Number(tkn.v),
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}

function parseString(tokenStream: TokenStream, parseState: ParseState): StringNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)

  return {
    t: AstNodeType.String,
    v: tkn.v,
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}

function parseName(tokenStream: TokenStream, parseState: ParseState): NameNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)
  return {
    t: AstNodeType.Name,
    v: tkn.v,
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}

function parseReservedName(tokenStream: TokenStream, parseState: ParseState): ReservedNameNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)
  return {
    t: AstNodeType.ReservedName,
    v: tkn.v,
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}

function parseComment(tokenStream: TokenStream, parseState: ParseState): CommentNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)
  return {
    t: AstNodeType.Comment,
    v: tkn.v,
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}

function parseTokensUntilClosingBracket(tokenStream: TokenStream, parseState: ParseState): AstNode[] {
  let tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  const astNodes: AstNode[] = []
  while (tkn.t !== 'RParen' && tkn.t !== 'RBracket') {
    astNodes.push(parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  }
  return astNodes
}

const parseExpression: ParseExpression = (tokenStream, parseState) => {
  const tkn = asToken(tokenStream.tokens[parseState.position + 1], tokenStream.filePath)
  if (tkn.t === 'Name' && builtin.specialExpressions[tkn.v as SpecialExpressionName])
    return parseSpecialExpression(tokenStream, parseState)

  return parseNormalExpression(tokenStream, parseState)
}

function parseArrayLitteral(tokenStream: TokenStream, parseState: ParseState): AstNode {
  const firstToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)

  let tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  const params: AstNode[] = []
  while (tkn.t !== 'RBracket') {
    params.push(parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  }

  parseState.position += 1

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'array',
    p: params,
    debugData: firstToken.debugData?.sourceCodeInfo
      ? {
          token: firstToken,
          lastToken: tkn,
        }
      : undefined,
  }

  return node
}

function parseObjectLitteral(tokenStream: TokenStream, parseState: ParseState): NormalExpressionNodeWithName {
  const firstToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)

  let tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  const params: AstNode[] = []
  while (tkn.t !== 'RBrace') {
    params.push(parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  }

  parseState.position += 1

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'object',
    p: params,
    debugData: firstToken.debugData?.sourceCodeInfo
      ? {
          token: firstToken,
          lastToken: tkn,
        }
      : undefined,
  }

  assertEvenNumberOfParams(node)

  return node
}

function parseRegexpShorthand(tokenStream: TokenStream, parseState: ParseState): NormalExpressionNodeWithName {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)

  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: tkn.v,
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? {
          token: tkn,
          lastToken: tkn,
        }
      : undefined,
  }

  assertNonUndefined(tkn.o, tkn.debugData?.sourceCodeInfo)

  const optionsNode: StringNode = {
    t: AstNodeType.String,
    v: `${tkn.o.g ? 'g' : ''}${tkn.o.i ? 'i' : ''}`,
    p: [],
    n: undefined,
    debugData: tkn.debugData?.sourceCodeInfo
      ? {
          token: tkn,
          lastToken: tkn,
        }
      : undefined,
  }

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'regexp',
    p: [stringNode, optionsNode],
    debugData: tkn.debugData?.sourceCodeInfo
      ? {
          token: tkn,
          lastToken: tkn,
        }
      : undefined,
  }

  return node
}

const placeholderRegexp = /^%([1-9]\d?)?$/
function parseFnShorthand(tokenStream: TokenStream, parseState: ParseState): FnNode {
  const firstToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)
  const startPos = parseState.position + 1
  const exprNode = parseExpression(tokenStream, parseState)

  let arity = 0
  let percent1: 'NOT_SET' | 'WITH_1' | 'NAKED' = 'NOT_SET' // referring to argument bindings. % = NAKED, %1, %2, %3, etc = WITH_1
  for (let pos = startPos; pos < parseState.position - 1; pos += 1) {
    const tkn = asToken(tokenStream.tokens[pos], tokenStream.filePath)
    if (tkn.t === 'Name') {
      const match = placeholderRegexp.exec(tkn.v)
      if (match) {
        const number = match[1] ?? '1'
        if (number === '1') {
          const mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED')
          if (mixedPercent1)
            throw new LitsError('Please make up your mind, either use % or %1', firstToken.debugData?.sourceCodeInfo)

          percent1 = match[1] ? 'WITH_1' : 'NAKED'
        }

        arity = Math.max(arity, Number(number))
        if (arity > 20)
          throw new LitsError('Can\'t specify more than 20 arguments', firstToken.debugData?.sourceCodeInfo)
      }
    }
    if (tkn.t === 'FnShorthand')
      throw new LitsError('Nested shortcut functions are not allowed', firstToken.debugData?.sourceCodeInfo)
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
    debugData: firstToken.debugData?.sourceCodeInfo
      ? {
          token: firstToken,
          lastToken: exprNode.debugData!.lastToken,
        }
      : undefined,
  }

  return node
}

const parseArgument: ParseArgument = (tokenStream, parseState) => {
  const tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath)

  if (tkn.t === 'Name') {
    return {
      t: AstNodeType.Argument,
      n: tkn.v,
      p: [],
      debugData: tkn.debugData?.sourceCodeInfo
        ? {
            token: tkn,
            lastToken: tkn,
          }
        : undefined,
    }
  }
  else if (tkn.t === 'Modifier') {
    const value = tkn.v as ModifierName
    return {
      t: AstNodeType.Modifier,
      v: value,
      p: [],
      n: undefined,
      debugData: tkn.debugData?.sourceCodeInfo
        ? {
            token: tkn,
            lastToken: tkn,
          }
        : undefined,
    }
  }
  else {
    throw new LitsError(`Expected name or modifier token, got ${valueToString(tkn)}.`, tkn.debugData?.sourceCodeInfo)
  }
}

function parseBindings(tokenStream: TokenStream, parseState: ParseState): BindingNode[] {
  let tkn = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath, { type: 'LBracket' })
  tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  const bindings: BindingNode[] = []
  while (tkn.t !== 'RBracket') {
    bindings.push(parseBinding(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  }
  parseState.position += 1

  return bindings
}

function parseBinding(tokenStream: TokenStream, parseState: ParseState): BindingNode {
  const firstToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath, { type: 'Name' })
  const name = firstToken.v

  const value = parseToken(tokenStream, parseState)

  const node: BindingNode = {
    t: AstNodeType.Binding,
    n: name,
    v: value,
    p: [],
    debugData: firstToken.debugData?.sourceCodeInfo
      ? {
          token: firstToken,
          lastToken: value.debugData!.lastToken,
        }
      : undefined,
  }
  return node
}

function parseNormalExpression(tokenStream: TokenStream, parseState: ParseState): NormalExpressionNode {
  const startBracketToken = tokenStream.hasDebugData
    ? asToken(tokenStream.tokens[parseState.position], tokenStream.filePath, { type: 'LParen' })
    : undefined
  parseState.position += 1
  const fnNode = parseToken(tokenStream, parseState)

  const params = parseTokensUntilClosingBracket(tokenStream, parseState)

  const lastToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath, { type: 'RParen' })

  if (isExpressionNode(fnNode)) {
    const node: NormalExpressionNode = {
      t: AstNodeType.NormalExpression,
      p: [fnNode, ...params],
      n: undefined,
      debugData: startBracketToken
        ? {
            token: startBracketToken,
            lastToken,
          }
        : undefined,
    }

    return node
  }

  assertNameNode(fnNode, fnNode.debugData?.token.debugData?.sourceCodeInfo)
  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: fnNode.v,
    p: params,
    debugData: startBracketToken
      ? {
          token: startBracketToken,
          nameToken: fnNode.debugData?.token,
          lastToken,
        }
      : undefined,
  }

  const builtinExpression = builtin.normalExpressions[node.n]

  if (builtinExpression) {
    builtinExpression.validate?.({
      ...node,
      p: withoutCommentNodes(node.p),
    })
  }

  return node
}

function parseSpecialExpression(tokenStream: TokenStream, parseState: ParseState): SpecialExpressionNode {
  const firstToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath, { type: 'LParen' })

  const nameToken = asToken(tokenStream.tokens[parseState.position++], tokenStream.filePath, { type: 'Name' })
  const { v: expressionName, debugData } = nameToken

  const { parse } = asNonUndefined(builtin.specialExpressions[expressionName as SpecialExpressionName], debugData?.sourceCodeInfo)

  const node = parse(tokenStream, parseState, firstToken, {
    parseExpression,
    parseTokensUntilClosingBracket,
    parseToken,
    parseBinding,
    parseBindings,
    parseArgument,
  })

  if (node.debugData) {
    node.debugData.nameToken = nameToken
  }

  return node
}
export function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  // if (parseState.infix) {
  //   return
  // }
  const tkn = asToken(tokenStream.tokens[parseState.position], tokenStream.filePath)
  switch (tkn.t) {
    case 'Number':
      return parseNumber(tokenStream, parseState)
    case 'String':
      return parseString(tokenStream, parseState)
    case 'Name':
      return parseName(tokenStream, parseState)
    case 'ReservedName':
      return parseReservedName(tokenStream, parseState)
    case 'LParen':
      return parseExpression(tokenStream, parseState)
    case 'LBracket':
      return parseArrayLitteral(tokenStream, parseState)
    case 'LBrace':
      return parseObjectLitteral(tokenStream, parseState)
    case 'RegexpShorthand':
      return parseRegexpShorthand(tokenStream, parseState)
    case 'FnShorthand':
      return parseFnShorthand(tokenStream, parseState)
    case 'Comment':
      return parseComment(tokenStream, parseState)
    case 'CollectionAccessor':
    case 'Modifier':
    case 'NewLine':
    case 'Infix':
    case 'Postfix':
    case 'InfixOperator':
    case 'RParen':
    case 'RBracket':
    case 'RBrace':
      break
    /* v8 ignore next 2 */
    default:
      throw new LitsError(`Unrecognized token: ${tkn.t satisfies never} ${tkn.v}`, tkn.debugData?.sourceCodeInfo)
  }
  throw new LitsError(`Unrecognized token: ${tkn.t} ${tkn.v}`, tkn.debugData?.sourceCodeInfo)
}
