export type TokenizerType =
  | `paren`
  | `number`
  | `name`
  | `string`
  | `reservedName`
  | `modifier`
  | `regexpShorthand`
  | `fnShorthand`

export type SourceCodeInfo =
  | {
      line: number
      column: number
      sourceCodeLine: string | null
      toString(): string
    }
  | `EOF`
  | null

export type Token = {
  type: TokenizerType
  value: string
  options?: Record<string, boolean>
  sourceCodeInfo: SourceCodeInfo
}
export type TokenDescriptor = [length: number, token: Token | undefined]
export type Tokenizer = (input: string, position: number, sourceCodeInfo: SourceCodeInfo) => TokenDescriptor
