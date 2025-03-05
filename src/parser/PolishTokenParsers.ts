import type { SpecialExpressionName, SpecialExpressionNode } from '../builtin'
import { builtin } from '../builtin'
import type { FnNode } from '../builtin/specialExpressions/functions'
import type { FunctionArguments } from '../builtin/utils'
import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { asLParenToken, assertLBracketToken, assertRParenToken, isRBraceToken, isRBracketToken, isRParenToken } from '../tokenizer/common/commonTokens'
import type { TokenStream } from '../tokenizer/interface'
import type { PolishTokenType } from '../tokenizer/polish/polishTokens'
import { asP_CommentToken, asP_StringShorthandToken, asP_SymbolToken, isP_FnShorthandToken, isP_ModifierToken, isP_SymbolToken } from '../tokenizer/polish/polishTokens'
import { asToken } from '../tokenizer/tokens'
import { getTokenDebugData } from '../tokenizer/utils'
import { asNonUndefined, assertNumberOfParams } from '../typeGuards'
import { assertSymbolNode, isExpressionNode } from '../typeGuards/astNode'
import { valueToString } from '../utils/debug/debugTools'
import { parseNumber, parseRegexpShorthand, parseReservedSymbol, parseString, parseSymbol } from './commonTokenParsers'
import type {
  AstNode,
  BindingNode,
  CommentNode,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  ParseArgument,
  ParseExpression,
  ParseState,
  StringNode,
} from './interface'

function parseStringShorthand(tokenStream: TokenStream, parseState: ParseState): StringNode {
  const tkn = asP_StringShorthandToken(tokenStream.tokens[parseState.position++])
  const value = tkn[1].substring(1)

  return {
    t: AstNodeType.String,
    v: value,
    p: [],
    n: undefined,
    token: getTokenDebugData(tkn) && tkn,
  }
}

function parseComment(tokenStream: TokenStream, parseState: ParseState): CommentNode {
  const tkn = asP_CommentToken(tokenStream.tokens[parseState.position++])
  return {
    t: AstNodeType.Comment,
    v: tkn[1],
    p: [],
    n: undefined,
    token: getTokenDebugData(tkn) && tkn,
  }
}

function parseTokensUntilClosingBracket(tokenStream: TokenStream, parseState: ParseState): AstNode[] {
  let tkn = asToken(tokenStream.tokens[parseState.position])
  const astNodes: AstNode[] = []
  while (!isRParenToken(tkn) && !isRBracketToken(tkn)) {
    astNodes.push(parseState.parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position])
  }
  return astNodes
}

const parseExpression: ParseExpression = (tokenStream, parseState) => {
  const tkn = asToken(tokenStream.tokens[parseState.position + 1])
  if (isP_SymbolToken(tkn) && builtin.specialExpressions[tkn[1] as SpecialExpressionName])
    return parseSpecialExpression(tokenStream, parseState)

  return parseNormalExpression(tokenStream, parseState)
}

function parseArrayLitteral(tokenStream: TokenStream, parseState: ParseState): AstNode {
  const firstToken = asToken(tokenStream.tokens[parseState.position++])

  let tkn = asToken(tokenStream.tokens[parseState.position])
  const params: AstNode[] = []
  while (!isRBracketToken(tkn)) {
    params.push(parseState.parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position])
  }

  parseState.position += 1

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'array',
    p: params,
    token: getTokenDebugData(firstToken) && firstToken,
  }

  return node
}

function parseObjectLitteral(tokenStream: TokenStream, parseState: ParseState): NormalExpressionNodeWithName {
  const firstToken = asToken(tokenStream.tokens[parseState.position++])

  let tkn = asToken(tokenStream.tokens[parseState.position])
  const params: AstNode[] = []
  while (!isRBraceToken(tkn)) {
    params.push(parseState.parseToken(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position])
  }

  parseState.position += 1

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'object',
    p: params,
    token: getTokenDebugData(firstToken) && firstToken,
  }

  assertNumberOfParams({ even: true }, node)

  return node
}

const placeholderRegexp = /^%([1-9]\d?)?$/
function parseFnShorthand(tokenStream: TokenStream, parseState: ParseState): FnNode {
  const firstToken = asToken(tokenStream.tokens[parseState.position++])
  const startPos = parseState.position + 1
  const exprNode = parseExpression(tokenStream, parseState)

  let arity = 0
  let percent1: 'NOT_SET' | 'WITH_1' | 'NAKED' = 'NOT_SET' // referring to argument bindings. % = NAKED, %1, %2, %3, etc = WITH_1
  for (let pos = startPos; pos < parseState.position - 1; pos += 1) {
    const tkn = asToken(tokenStream.tokens[pos])
    if (isP_SymbolToken(tkn)) {
      const match = placeholderRegexp.exec(tkn[1])
      if (match) {
        const number = match[1] ?? '1'
        if (number === '1') {
          const mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED')
          if (mixedPercent1)
            throw new LitsError('Please make up your mind, either use % or %1', getTokenDebugData(firstToken)?.sourceCodeInfo)

          percent1 = match[1] ? 'WITH_1' : 'NAKED'
        }

        arity = Math.max(arity, Number(number))
        if (arity > 20)
          throw new LitsError('Can\'t specify more than 20 arguments', getTokenDebugData(firstToken)?.sourceCodeInfo)
      }
    }
    if (isP_FnShorthandToken(tkn))
      throw new LitsError('Nested shortcut functions are not allowed', getTokenDebugData(firstToken)?.sourceCodeInfo)
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
    token: getTokenDebugData(firstToken) && firstToken,
  }

  return node
}

const parseArgument: ParseArgument = (tokenStream, parseState) => {
  const tkn = asToken(tokenStream.tokens[parseState.position++])

  if (isP_SymbolToken(tkn)) {
    return {
      t: AstNodeType.Argument,
      n: tkn[1],
      p: [],
      token: getTokenDebugData(tkn) && tkn,
    }
  }
  else if (isP_ModifierToken(tkn)) {
    return {
      t: AstNodeType.Modifier,
      v: tkn[1],
      p: [],
      n: undefined,
      token: getTokenDebugData(tkn) && tkn,
    }
  }
  else {
    throw new LitsError(`Expected name or modifier token, got ${valueToString(tkn)}.`, getTokenDebugData(tkn)?.sourceCodeInfo)
  }
}

function parseBindings(tokenStream: TokenStream, parseState: ParseState): BindingNode[] {
  assertLBracketToken(tokenStream.tokens[parseState.position++])
  let tkn = asToken(tokenStream.tokens[parseState.position])
  const bindings: BindingNode[] = []
  while (!isRBracketToken(tkn)) {
    bindings.push(parseBinding(tokenStream, parseState))
    tkn = asToken(tokenStream.tokens[parseState.position])
  }
  parseState.position += 1

  return bindings
}

function parseBinding(tokenStream: TokenStream, parseState: ParseState): BindingNode {
  const firstToken = asP_SymbolToken(tokenStream.tokens[parseState.position])
  const name = parseSymbol(tokenStream, parseState)

  const value = parseState.parseToken(tokenStream, parseState)

  const node: BindingNode = {
    t: AstNodeType.Binding,
    n: name.v,
    v: value,
    p: [],
    token: getTokenDebugData(firstToken) && firstToken,
  }
  return node
}

function parseNormalExpression(tokenStream: TokenStream, parseState: ParseState): NormalExpressionNode {
  const startBracketToken = tokenStream.hasDebugData ? asLParenToken(tokenStream.tokens[parseState.position]) : undefined
  parseState.position += 1
  const fnNode = parseState.parseToken(tokenStream, parseState)

  const params = parseTokensUntilClosingBracket(tokenStream, parseState)

  assertRParenToken(tokenStream.tokens[parseState.position++])

  if (isExpressionNode(fnNode)) {
    const node: NormalExpressionNode = {
      t: AstNodeType.NormalExpression,
      p: [fnNode, ...params],
      n: undefined,
      token: startBracketToken,
    }

    return node
  }

  assertSymbolNode(fnNode, getTokenDebugData(fnNode.token)?.sourceCodeInfo)
  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: fnNode.v,
    p: params,
    token: startBracketToken,
  }

  const builtinExpression = builtin.normalExpressions[node.n]

  if (builtinExpression) {
    assertNumberOfParams(builtinExpression.paramCount, node)
  }

  return node
}

function parseSpecialExpression(tokenStream: TokenStream, parseState: ParseState): SpecialExpressionNode {
  const firstToken = asLParenToken(tokenStream.tokens[parseState.position++])

  const nameToken = asP_SymbolToken(tokenStream.tokens[parseState.position++])
  const expressionName = nameToken[1] as SpecialExpressionName

  const { polishParse: parse, paramCount } = asNonUndefined(builtin.specialExpressions[expressionName], getTokenDebugData(nameToken)?.sourceCodeInfo)

  const node = parse(tokenStream, parseState, firstToken, {
    parseExpression,
    parseTokensUntilClosingBracket,
    parseToken: parseState.parseToken,
    parseBinding,
    parseBindings,
    parseArgument,
  })

  assertNumberOfParams(paramCount, node)

  return node
}

export function parsePolishToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  const tkn = asToken(tokenStream.tokens[parseState.position])

  const tokenType = tkn[0] as PolishTokenType
  switch (tokenType) {
    case 'String':
      return parseString(tokenStream, parseState)
    case 'P_Number':
      return parseNumber(tokenStream, parseState)
    case 'P_StringShorthand':
      return parseStringShorthand(tokenStream, parseState)
    case 'P_Symbol':
      return parseSymbol(tokenStream, parseState)
    case 'P_ReservedSymbol':
      return parseReservedSymbol(tokenStream, parseState)
    case 'LParen':
      return parseExpression(tokenStream, parseState)
    case 'LBracket':
      return parseArrayLitteral(tokenStream, parseState)
    case 'LBrace':
      return parseObjectLitteral(tokenStream, parseState)
    case 'RegexpShorthand':
      return parseRegexpShorthand(tokenStream, parseState)
    case 'P_FnShorthand':
      return parseFnShorthand(tokenStream, parseState)
    case 'P_Comment':
      return parseComment(tokenStream, parseState)
    case 'P_CollectionAccessor':
    case 'P_Modifier':
    case 'RParen':
    case 'RBracket':
    case 'RBrace':
    case 'P_Whitespace':
      break
    /* v8 ignore next 2 */
    default:
      throw new LitsError(`Unrecognized token: ${tokenType satisfies never} ${tkn[1]}`, getTokenDebugData(tkn)?.sourceCodeInfo)
  }
  throw new LitsError(`Unrecognized token: ${tokenType}${tkn[1] ? ` ${tkn[1]}` : ''}`, getTokenDebugData(tkn)?.sourceCodeInfo)
}
