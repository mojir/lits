import type { Token } from './tokens'

export interface SourceCodeInfo {
  position?: {
    line: number
    column: number
  }
  code?: string
  filePath?: string
}

export type TokenDescriptor<T extends Token> = [length: number, token?: T]
export type Tokenizer<T extends Token> = (input: string, position: number) => TokenDescriptor<T>
export interface TokenStream {
  tokens: Token[]
  hasDebugData: boolean
  filePath?: string
}

export interface TokenizeParams {
  debug: boolean
  filePath?: string
}
