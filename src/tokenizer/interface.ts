import type { TokenType } from '../constants/constants'

export interface SourceCodeInfo {
  position?: {
    line: number
    column: number
  }
  code?: string
  filePath?: string
}

export type MetaToken = Token<TokenType.NewLine> | Token<TokenType.Comment>
export interface MetaTokens {
  leadingMetaTokens: MetaToken[] // Comments on the lines before the token
  inlineCommentToken: Token<TokenType.Comment> | null // Comment on the same line as the token
}

export interface TokenDebugData {
  sourceCodeInfo: SourceCodeInfo
  metaTokens: MetaTokens
}
export interface Token<T extends TokenType = TokenType> {
  t: T // type
  v: string // value
  o?: Record<string, boolean> // options
  debugData: TokenDebugData | undefined
}
export type TokenDescriptor = [length: number, token: Token | undefined]
export type Tokenizer = (input: string, position: number, debugData?: TokenDebugData) => TokenDescriptor
export interface TokenStream {
  tokens: Token[]
  hasDebugData: boolean
  filePath?: string
}

export interface TokenizeParams {
  debug?: boolean
  filePath?: string
}
