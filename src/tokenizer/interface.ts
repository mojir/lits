export type TokenizerType = 'paren' | 'number' | 'name' | 'string' | 'reservedName' | 'shorthand'
export type Token = { type: TokenizerType; value: string; inputPosition: number }
export type TokenDescriptor = [length: number, token: Token | undefined]
export type Tokenizer = (input: string, position: number) => TokenDescriptor
