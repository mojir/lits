import type { TokenType } from '../constants/constants'

export interface SourceCodeInfo {
  position?: {
    line: number
    column: number
  }
  code?: string
  filePath?: string
}

export interface Token {
  t: TokenType // type
  v: string // value
  o?: Record<string, boolean> // options
  sourceCodeInfo?: SourceCodeInfo // sourceCodeInfo
}
export type TokenDescriptor = [length: number, token: Token | undefined]
export type Tokenizer = (input: string, position: number, sourceCodeInfo?: SourceCodeInfo) => TokenDescriptor
export interface TokenStream {
  tokens: Token[]
  filePath?: string
}

export interface TokenizeParams {
  debug: boolean
  filePath?: string
}
