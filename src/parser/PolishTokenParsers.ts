import type { TokenStream } from '../tokenizer/interface'
import type {
  AstNode,
  ParseState,
} from './interface'

export function parsePolishToken(_tokenStream: TokenStream, _parseState: ParseState): AstNode {
  throw new Error('Should not be called')
}
