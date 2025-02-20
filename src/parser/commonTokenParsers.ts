import { AstNodeType } from '../constants/constants'
import type { TokenStream } from '../tokenizer/interface'
import { asStringToken, asToken, getTokenDebugData } from '../tokenizer/Token'
import type {
  NumberNode,
  ParseState,
  StringNode,
} from './interface'

export function parseNumber(tokenStream: TokenStream, parseState: ParseState): NumberNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++])
  return {
    t: AstNodeType.Number,
    v: Number(tkn[1]),
    p: [],
    n: undefined,
    debugData: getTokenDebugData(tkn)?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}

export function parseString(tokenStream: TokenStream, parseState: ParseState): StringNode {
  const tkn = asStringToken(tokenStream.tokens[parseState.position++])
  const value = tkn[1].substring(1, tkn[1].length - 1)
    .replace(
      /(\\{2})|(\\")|(\\n)|(\\t)|(\\r)|(\\b)|(\\f)|\\(.)/g,
      (
        _,
        backslash: string,
        doubleQuote: string,
        newline: string,
        tab: string,
        carriageReturn: string,
        backspace: string,
        formFeed: string,
        normalChar: string,
      ) => {
        // If it's a double escape (\\x), return \x
        if (backslash) {
          return '\\'
        }
        // If it's a special character (\n, \t, \r, \b, \f), return the special character
        else if (newline) {
          return '\n'
        }
        else if (tab) {
          return '\t'
        }
        else if (carriageReturn) {
          return '\r'
        }
        else if (backspace) {
          return '\b'
        }
        else if (formFeed) {
          return '\f'
        }
        else if (doubleQuote) {
          return '"'
        }
        return normalChar
      },
    )

  return {
    t: AstNodeType.String,
    v: value,
    p: [],
    n: undefined,
    debugData: getTokenDebugData(tkn)?.sourceCodeInfo
      ? { token: tkn, lastToken: tkn }
      : undefined,
  }
}
