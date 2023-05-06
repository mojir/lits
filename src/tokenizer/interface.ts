import { LocationGetter } from '../Lits/interface'

export type TokenizerType =
  | `paren`
  | `number`
  | `name`
  | `string`
  | `reservedName`
  | `modifier`
  | `regexpShorthand`
  | `fnShorthand`
  | `typeName`

export type SourceCodeInfo = {
  line: number
  column: number
  code: string
  getLocation?: LocationGetter
}

export type DebugInfo = SourceCodeInfo | `EOF`

export type Token = {
  type: TokenizerType
  value: string
  options?: Record<string, boolean>
  debugInfo?: DebugInfo
}
export type TokenDescriptor = [length: number, token: Token | undefined]
export type Tokenizer = (input: string, position: number, debugInfo?: DebugInfo) => TokenDescriptor

export type TokenizeParams = {
  debug: boolean
  getLocation?: LocationGetter
}
