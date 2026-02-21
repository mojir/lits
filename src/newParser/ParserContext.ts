import { LitsError } from '../errors'
import type { SourceCodeInfo, Token } from '../tokenizer/token'
import type { TokenStream } from '../tokenizer/tokenize'

export class ParserContext {
  private readonly tokens: Token[]
  private position: number
  private storedPosition: number = 0

  constructor(tokenStream: TokenStream) {
    this.tokens = tokenStream.tokens
    this.position = 0
  }

  public advance(): void {
    this.position += 1
  }

  public tryPeek(): Token | undefined {
    return this.tokens[this.position]
  }

  public peek(): Token {
    const token = this.tokens[this.position]
    if (!token) {
      const lastToken = this.tokens.at(-1)
      const sourceCodeInfo = lastToken ? lastToken[2] : undefined
      throw new LitsError('Unexpected end of input', sourceCodeInfo)
    }
    return token
  }

  public isAtEnd(): boolean {
    return this.position >= this.tokens.length
  }

  // TODO rename to getSourceCodeInfo
  public peekSourceCodeInfo(): SourceCodeInfo | undefined {
    const currentToken = this.tryPeek()
    return currentToken ? currentToken[2] : this.tokens.at(-1)?.[2]
  }

  public storePosition(): number {
    return this.storedPosition = this.position
  }

  public restorePosition(): void {
    this.position = this.storedPosition
  }

  public peekAhead(count: number): Token | undefined {
    return this.tokens[this.position + count]
  }

  public getPosition(): number {
    return this.position
  }

  public getTokenAt(pos: number): Token | undefined {
    return this.tokens[pos]
  }

  // Add methods for token management, error handling, etc.
}
