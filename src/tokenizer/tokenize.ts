import type { FilePathParams } from '../Lits/Lits'
import { tokenizers } from './tokenizers'
import type { SourceCodeInfo, Token, TokenDescriptor } from './token'

export interface TokenStream {
  tokens: Token[]
  hasDebugData: boolean
  filePath?: string
}

export function tokenize(input: string, debug: boolean, filePath: FilePathParams['filePath']): TokenStream {
  let position = 0
  const tokenStream: TokenStream = {
    tokens: [],
    filePath,
    hasDebugData: debug,
  }

  while (position < input.length) {
    const sourceCodeInfo: SourceCodeInfo | undefined = debug
      ? createSourceCodeInfo(input, position, filePath)
      : undefined

    const tokenDescriptor = getCurrentToken(input, position)

    const [count, token] = tokenDescriptor

    position += count
    if (token) {
      if (sourceCodeInfo) {
        token[2] = sourceCodeInfo
      }

      tokenStream.tokens.push(token)
    }
  }

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

function getCurrentToken(input: string, position: number): TokenDescriptor<Token> {
  const initialPosition = position

  for (const tokenizer of tokenizers) {
    const [nbrOfCharacters, token] = tokenizer(input, position)
    position += nbrOfCharacters
    if (nbrOfCharacters === 0) {
      continue
    }

    return [position - initialPosition, token]
  }
  return [1, ['Error', input[initialPosition], undefined, 'Unrecognized character']] as TokenDescriptor<Token>
}
