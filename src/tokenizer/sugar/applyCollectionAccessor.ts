import { LitsError } from '../../errors'
import { asNonUndefined } from '../../typeGuards'
import { assertNumber } from '../../typeGuards/number'
import type { SourceCodeInfo, Token, TokenStream } from '../interface'
import type { TokenType } from '../../constants/constants'
import type { SugarFunction } from '.'

export const applyCollectionAccessors: SugarFunction = (tokenStream) => {
  let dotTokenIndex = tokenStream.tokens.findIndex(tkn => tkn.t === 'CollectionAccessor')
  while (dotTokenIndex >= 0) {
    applyCollectionAccessor(tokenStream, dotTokenIndex)
    dotTokenIndex = tokenStream.tokens.findIndex(tkn => tkn.t === 'CollectionAccessor')
  }
  return tokenStream
}

function applyCollectionAccessor(tokenStream: TokenStream, position: number) {
  const dotTkn = asNonUndefined(tokenStream.tokens[position])
  const debugData = dotTkn.debugData
  const backPosition = getPositionBackwards(tokenStream, position, debugData?.sourceCodeInfo)
  checkForward(tokenStream, position, dotTkn, debugData?.sourceCodeInfo)

  tokenStream.tokens.splice(position, 1)
  tokenStream.tokens.splice(backPosition, 0, {
    t: 'LParen',
    v: '(',
    debugData,
  })
  const nextTkn = asNonUndefined(tokenStream.tokens[position + 1])
  if (dotTkn.v === '.') {
    tokenStream.tokens[position + 1] = {
      t: 'String',
      v: nextTkn.v,
      debugData: nextTkn.debugData,
    }
  }
  else {
    assertNumber(Number(nextTkn.v), debugData?.sourceCodeInfo, { integer: true, nonNegative: true })
    tokenStream.tokens[position + 1] = {
      t: 'Number',
      v: nextTkn.v,
      debugData: nextTkn.debugData,
    }
  }
  tokenStream.tokens.splice(position + 2, 0, {
    t: 'RParen',
    v: ')',
    debugData,
  })
}

function getPositionBackwards(tokenStream: TokenStream, position: number, sourceCodeInfo: SourceCodeInfo | undefined) {
  let bracketCount: number | null = null
  if (position <= 0)
    throw new LitsError('Array accessor # must come after a sequence', sourceCodeInfo)

  const prevToken = asNonUndefined(tokenStream.tokens[position - 1])
  let openBracket: 'LParen' | 'LBracket' | 'LBrace' | null = null satisfies TokenType | null
  let closeBracket: 'RParen' | 'RBracket' | 'RBrace' | null = null satisfies TokenType | null

  if (prevToken.t === 'RParen') {
    openBracket = 'LParen'
    closeBracket = 'RParen'
  }
  else if (prevToken.t === 'RBracket') {
    openBracket = 'LBracket'
    closeBracket = 'RBracket'
  }
  else if (prevToken.t === 'RBrace') {
    openBracket = 'LBrace'
    closeBracket = 'RBrace'
  }

  while (bracketCount !== 0) {
    bracketCount = bracketCount === null ? 0 : bracketCount
    position -= 1
    const tkn = asNonUndefined(tokenStream.tokens[position], sourceCodeInfo)
    if (tkn.t === openBracket) {
      bracketCount += 1
    }
    else if (tkn.t === closeBracket) {
      bracketCount -= 1
    }
  }
  if (openBracket === 'LParen' && position > 0) {
    const tokenBeforeBracket = asNonUndefined(tokenStream.tokens[position - 1])
    if (tokenBeforeBracket.t === 'FnShorthand')
      throw new LitsError('# or . must NOT be preceeded by shorthand lambda function', sourceCodeInfo)
  }
  return position
}

function checkForward(
  tokenStream: TokenStream,
  position: number,
  dotTkn: Token,
  sourceCodeInfo: SourceCodeInfo | undefined,
) {
  const tkn = asNonUndefined(tokenStream.tokens[position + 1], sourceCodeInfo)

  if (dotTkn.v === '.' && tkn.t !== 'Name')
    throw new LitsError('# as a collection accessor must be followed by an name', sourceCodeInfo)

  if (dotTkn.v === '#' && tkn.t !== 'Number')
    throw new LitsError('# as a collection accessor must be followed by an integer', sourceCodeInfo)
}
