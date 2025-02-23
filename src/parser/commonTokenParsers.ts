import { LitsError } from '../errors'
import { AstNodeType } from '../constants/constants'
import { asStringToken } from '../tokenizer/common/commonTokens'
import { isIF_ReservedSymbolToken, isIF_SymbolToken } from '../tokenizer/infix/infixTokens'
import type { TokenStream } from '../tokenizer/interface'
import { isPF_ReservedSymbolToken, isPF_SymbolToken } from '../tokenizer/postfix/postfixTokens'
import { asToken } from '../tokenizer/tokens'
import { getTokenDebugData } from '../tokenizer/utils'
import type {
  NumberNode,
  ParseState,
  ReservedSymbolNode,
  StringNode,
  SymbolNode,
} from './interface'

export function parseSymbol(tokenStream: TokenStream, parseState: ParseState): SymbolNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++])
  if (!isIF_SymbolToken(tkn) && !isPF_SymbolToken(tkn)) {
    throw new LitsError(`Expected symbol token, got ${tkn[0]}`)
  }
  return {
    t: AstNodeType.Name,
    v: tkn[1],
    p: [],
    n: undefined,
    debugData: getTokenDebugData(tkn)?.sourceCodeInfo
      ? { token: tkn }
      : undefined,
  }
}

export function parseReservedSymbol(tokenStream: TokenStream, parseState: ParseState): ReservedSymbolNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++])

  if (!isIF_ReservedSymbolToken(tkn) && !isPF_ReservedSymbolToken(tkn)) {
    throw new LitsError(`Expected symbol token, got ${tkn[0]}`)
  }
  return {
    t: AstNodeType.ReservedName,
    v: tkn[1],
    p: [],
    n: undefined,
    debugData: getTokenDebugData(tkn)?.sourceCodeInfo
      ? { token: tkn }
      : undefined,
  }
}

export function parseNumber(tokenStream: TokenStream, parseState: ParseState): NumberNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++])
  return {
    t: AstNodeType.Number,
    v: Number(tkn[1]),
    p: [],
    n: undefined,
    debugData: getTokenDebugData(tkn)?.sourceCodeInfo
      ? { token: tkn }
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
      ? { token: tkn }
      : undefined,
  }
}
