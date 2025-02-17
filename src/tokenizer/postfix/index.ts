import { LitsError } from '../../errors'
import type { SourceCodeInfo, TokenDescriptor, TokenizeParams } from '../interface'
import type { CommentToken, MetaToken, NewLineToken, Token, TokenDebugData } from '../Token'
import { addTokenDebugData, assertCommentToken, assertNewLineToken, getTokenDebugData, isCommentToken, isNewLineToken } from '../Token'
import { commentTokenizers, newLineTokenizers, postfixTokenizers } from './tokenizers'

function getSourceCodeLine(input: string, lineNbr: number): string {
  return input.split(/\r\n|\r|\n/)[lineNbr] as string
}

function createSourceCodeInfo(input: string, position: number, filePath?: string): SourceCodeInfo {
  const lines = input.substring(0, position + 1).split(/\r\n|\r|\n/)
  const lastLine = lines[lines.length - 1] as string

  const code = getSourceCodeLine(input, lines.length - 1)
  const line = lines.length
  const column = lastLine.length
  return {
    code,
    position: {
      line,
      column,
    },
    filePath,
  }
}

export function getNextPostfixToken(input: string, position: number, params: TokenizeParams): TokenDescriptor<Token> {
  const debug = !!params.debug
  const initialPosition = position
  const [leadingNewLineTokensLength, leadingNewLineTokens] = readLeadingNewLineTokens(input, position, params)
  position += leadingNewLineTokensLength
  if (position >= input.length)
    return [position - initialPosition, undefined]

  const [leadingCommentTokensLength, leadingCommentTokens] = readLeadingCommentTokens(input, position, params)
  position += leadingCommentTokensLength
  if (position >= input.length)
    return [position - initialPosition, undefined]

  const leadingMetaTokens: MetaToken[] = [...leadingNewLineTokens, ...leadingCommentTokens]

  // Loop through all tokenizer until one matches
  const debugData: TokenDebugData | undefined = debug
    ? {
        sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
        metaTokens: { inlineCommentToken: null, leadingMetaTokens },
      }
    : undefined

  let tryNext = true
  while (tryNext) {
    if (position >= input.length) {
      return [position - initialPosition, undefined]
    }
    tryNext = false
    for (const tokenizer of postfixTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position)
      if (token && debugData) {
        addTokenDebugData(token, debugData)
      }
      position += nbrOfCharacters
      if (nbrOfCharacters === 0) {
        continue
      }

      if (!token) {
        tryNext = true
        break
      }

      let inlineCommentToken: CommentToken | null = null
      if (!isCommentToken(token)) {
        [position, inlineCommentToken] = readInlineCommentToken(input, position, params)
      }

      const tokenDebugData = getTokenDebugData(token)
      if (tokenDebugData) {
        tokenDebugData.metaTokens.inlineCommentToken = inlineCommentToken
      }

      if (!isCommentToken(token) || debug) {
        return [position - initialPosition, token]
      }
      tryNext = true
      break
    }
  }
  throw new LitsError(`Unrecognized character '${input[position]}'.`, debugData?.sourceCodeInfo)
}

function readLeadingNewLineTokens(input: string, position: number, params: TokenizeParams): [number, MetaToken[]] {
  const newLineTokens: NewLineToken[] = []

  const initialPosition = position

  let tokenized = false
  while (position < input.length) {
    tokenized = false

    const debugData: TokenDebugData | undefined = params.debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
        }
      : undefined

    // Loop through all tokenizer until one matches
    for (const tokenizer of newLineTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position)
      if (token && debugData) {
        addTokenDebugData(token, debugData)
      }
      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          assertNewLineToken(token)

          if (newLineTokens.length < 2)
            newLineTokens.push(token)
        }
        break
      }
    }
    if (!tokenized)
      // All newline tokens read!
      return [position - initialPosition, newLineTokens]
  }
  // Ending up here means that no non newline token was found. I.e. this cannot be leading newline tokens
  return [position - initialPosition, []]
}

function readLeadingCommentTokens(input: string, position: number, params: TokenizeParams): [number, MetaToken[]] {
  const initialPosition = position
  const commentTokens: CommentToken[] = []

  let tokenized = false
  while (position < input.length) {
    tokenized = false

    const debugData: TokenDebugData | undefined = params.debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
        }
      : undefined

    // Loop through all tokenizer until one matches
    for (const tokenizer of commentTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position)
      if (token && debugData) {
        addTokenDebugData(token, debugData)
      }
      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          assertMetaToken(token)

          // If a newline token is found, then this is not a leading comment
          if (isNewLineToken(token))
            return [0, []]

          commentTokens.push(token)
        }
        break
      }
    }
    if (!tokenized)
      // All metatokens read!
      return [position - initialPosition, commentTokens]
  }
  // Ending up here means that no non meta token was found. I.e. this cannot be leading meta tokens
  return [0, []]
}

function readInlineCommentToken(input: string, position: number, params: TokenizeParams): [number, CommentToken | null] {
  const rollbackPosition = position
  let tokenized = false
  while (position < input.length) {
    tokenized = false
    const debugData: TokenDebugData | undefined = params.debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
        }
      : undefined

    // Loop through all tokenizer until one matches
    for (const tokenizer of commentTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position)
      if (token && debugData) {
        addTokenDebugData(token, debugData)
      }

      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          if (isNewLineToken(token))
            return [rollbackPosition, null]
          assertCommentToken(token)
          return [position, token]
        }
        break
      }
    }
    if (!tokenized)
      // All metatokens read! Return undefined if not debug mode
      return [rollbackPosition, null]
  }
  // Ending up here means that no comment token was found and end of tokens reached
  return [position, null]
}

function isMetaToken(token?: Token): token is MetaToken {
  return !!token && (isNewLineToken(token) || isCommentToken(token))
}

export function assertMetaToken(token?: Token): asserts token is MetaToken {
  if (!isMetaToken(token))
    throw new LitsError(`Expected meta token, got ${token}.`)
}
