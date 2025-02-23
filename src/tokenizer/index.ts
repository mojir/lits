import { LitsError } from '../errors'
import { infixTokenizers } from './infix/infixTokenizers'
import { isIF_PostfixToken } from './infix/infixTokens'
import type { SourceCodeInfo, TokenDescriptor, TokenStream, TokenizeParams, Tokenizer } from './interface'
import { postfixTokenizers } from './postfix/postfixTokenizers'
import { isPF_InfixToken } from './postfix/postfixTokens'
import { getSugar } from './sugar'
import type { Token } from './tokens'
import type { TokenDebugData } from './utils'
import { addTokenDebugData } from './utils'

export function tokenize(input: string, params: TokenizeParams): TokenStream {
  const debug = !!params.debug
  let infix = !!params.infix
  let position = 0
  const tokenStream: TokenStream = {
    tokens: [],
    filePath: params.filePath,
    hasDebugData: debug,
    infix,
  }

  while (position < input.length) {
    const tokenizers = infix ? infixTokenizers : postfixTokenizers
    const tokenDescriptor = getCurrentToken(input, position, tokenizers)

    const debugData: TokenDebugData | undefined = debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
        }
      : undefined

    if (!tokenDescriptor) {
      throw new LitsError(`Unrecognized character '${input[position]}'.`, debugData?.sourceCodeInfo)
    }

    const [count, token] = tokenDescriptor

    position += count
    if (token) {
      if (debugData) {
        addTokenDebugData(token, debugData)
      }

      tokenStream.tokens.push(token)
      if (isPF_InfixToken(token)) {
        infix = true
      }
      if (isIF_PostfixToken(token)) {
        infix = false
      }
    }
  }

  applySugar(tokenStream)

  return tokenStream
}

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

function getCurrentToken(input: string, position: number, tokenizers: Tokenizer<Token>[]): TokenDescriptor<Token> | null {
  const initialPosition = position

  let tryNext = true
  while (tryNext) {
    if (position >= input.length) {
      return [position - initialPosition, undefined]
    }
    tryNext = false
    for (const tokenizer of tokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position)
      position += nbrOfCharacters
      if (nbrOfCharacters === 0) {
        continue
      }

      if (!token) {
        tryNext = true
        break
      }

      return [position - initialPosition, token]
    }
  }
  return null
}

function applySugar(tokenStream: TokenStream) {
  const sugar = getSugar()
  sugar.forEach(sugarFn => sugarFn(tokenStream))
}
