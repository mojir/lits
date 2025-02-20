import { LitsError } from '../../errors'
import type { SourceCodeInfo, TokenDescriptor, TokenizeParams } from '../interface'
import { type Token, type TokenDebugData, addTokenDebugData } from '../Token'
import { tokenizers } from './infixTokenizers'

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

export function getNextInfixToken(input: string, position: number, params: TokenizeParams): TokenDescriptor<Token> {
  const debug = !!params.debug
  const initialPosition = position

  // Loop through all tokenizer until one matches
  const debugData: TokenDebugData | undefined = debug
    ? {
        sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
      }
    : undefined

  let tryNext = true
  while (tryNext) {
    if (position >= input.length) {
      return [position - initialPosition, undefined]
    }
    tryNext = false
    for (const tokenizer of tokenizers) {
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

      return [position - initialPosition, token]
    }
  }
  throw new LitsError(`Unrecognized character '${input[position]}'.`, debugData?.sourceCodeInfo)
}
