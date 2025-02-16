import { LitsError } from '../../errors'
import { asNonUndefined } from '../../typeGuards'
import { assertNumber, isNumber } from '../../typeGuards/number'
import type { SourceCodeInfo, TokenStream } from '../interface'
import type { CollectionAccessorToken, StringToken, TokenType } from '../Token'
import { addTokenDebugData, asCollectionAccessorToken, asToken, assertNumberToken, assertSymbolToken, getTokenDebugData, isCollectionAccessorToken, isFnShorthandToken, isRBraceToken, isRBracketToken, isRParenToken, isSymbolToken } from '../Token'
import type { SugarFunction } from '.'

export const applyCollectionAccessors: SugarFunction = (tokenStream) => {
  let dotTokenIndex = tokenStream.tokens.findIndex(isCollectionAccessorToken)
  while (dotTokenIndex >= 0) {
    applyCollectionAccessor(tokenStream, dotTokenIndex)
    dotTokenIndex = tokenStream.tokens.findIndex(isCollectionAccessorToken)
  }
  return tokenStream
}

function applyCollectionAccessor(tokenStream: TokenStream, position: number) {
  const dotTkn = asCollectionAccessorToken(tokenStream.tokens[position])
  const debugData = getTokenDebugData(dotTkn)
  const backPosition = getPositionBackwards(tokenStream, position, debugData?.sourceCodeInfo)
  checkForward(tokenStream, position, dotTkn, debugData?.sourceCodeInfo)

  tokenStream.tokens.splice(position, 1)
  tokenStream.tokens.splice(backPosition, 0, ['LParen'])
  const nextTkn = asToken(tokenStream.tokens[position + 1])
  if (dotTkn[1] === '.') {
    assertSymbolToken(nextTkn)
    const token: StringToken = ['String', nextTkn[1]]
    tokenStream.tokens[position + 1] = token

    const nextTkndebugData = getTokenDebugData(nextTkn)
    if (nextTkndebugData) {
      addTokenDebugData(token, nextTkndebugData)
    }
  }
  else {
    assertNumberToken(nextTkn)
    assertNumber(Number(nextTkn[1]), debugData?.sourceCodeInfo, { integer: true, nonNegative: true })

    tokenStream.tokens[position + 1] = ['Number', nextTkn[1]]
  }
  tokenStream.tokens.splice(position + 2, 0, ['RParen'])
}

function getPositionBackwards(tokenStream: TokenStream, position: number, sourceCodeInfo: SourceCodeInfo | undefined) {
  let bracketCount: number | null = null
  if (position <= 0)
    throw new LitsError('Array accessor # must come after a sequence', sourceCodeInfo)

  const prevToken = asNonUndefined(tokenStream.tokens[position - 1])
  let openBracket: 'LParen' | 'LBracket' | 'LBrace' | null = null satisfies TokenType | null
  let closeBracket: 'RParen' | 'RBracket' | 'RBrace' | null = null satisfies TokenType | null

  if (isRParenToken(prevToken)) {
    openBracket = 'LParen'
    closeBracket = 'RParen'
  }
  else if (isRBracketToken(prevToken)) {
    openBracket = 'LBracket'
    closeBracket = 'RBracket'
  }
  else if (isRBraceToken(prevToken)) {
    openBracket = 'LBrace'
    closeBracket = 'RBrace'
  }

  while (bracketCount !== 0) {
    bracketCount = bracketCount === null ? 0 : bracketCount
    position -= 1
    const tkn = asNonUndefined(tokenStream.tokens[position], sourceCodeInfo)
    if (tkn[0] === openBracket) {
      bracketCount += 1
    }
    else if (tkn[0] === closeBracket) {
      bracketCount -= 1
    }
  }
  if (openBracket === 'LParen' && position > 0) {
    const tokenBeforeBracket = asNonUndefined(tokenStream.tokens[position - 1])
    if (isFnShorthandToken(tokenBeforeBracket))
      throw new LitsError('# or . must NOT be preceeded by shorthand lambda function', sourceCodeInfo)
  }
  return position
}

function checkForward(
  tokenStream: TokenStream,
  position: number,
  dotTkn: CollectionAccessorToken,
  sourceCodeInfo: SourceCodeInfo | undefined,
) {
  const tkn = tokenStream.tokens[position + 1]

  if (dotTkn[1] === '.' && !isSymbolToken(tkn)) {
    throw new LitsError('# as a collection accessor must be followed by an name', sourceCodeInfo)
  }

  if (dotTkn[1] === '#' && isNumber(tkn)) {
    throw new LitsError('# as a collection accessor must be followed by an integer', sourceCodeInfo)
  }
}
