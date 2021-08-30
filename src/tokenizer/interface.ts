export type Token = { type: TokenizerType; value: string }
export type TokenizerType = 'paren' | 'number' | 'name' | 'string' | 'reservedName'
export type TokenDescriptor = [length: number, token: Token | null]
export type Tokenizer = (input: string, position: number) => TokenDescriptor
