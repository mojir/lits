export type TokenizerType =
  | `paren`
  | `number`
  | `name`
  | `string`
  | `reservedName`
  | `modifier`
  | `regexpShorthand`
  | `fnShorthand`
export type Token = { type: TokenizerType; value: string; options?: Record<string, boolean> }
export type TokenDescriptor = [length: number, token: Token | undefined]
export type Tokenizer = (input: string, position: number) => TokenDescriptor
