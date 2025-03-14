import { AstNodeType } from '../constants/constants'
import { LitsError } from '../errors'
import { isNumberReservedSymbol, numberReservedSymbolRecord } from '../tokenizer/reservedNames'
import { asRegexpShorthandToken, asStringToken, asToken, isA_BasePrefixedNumberToken, isA_NumberToken, isA_ReservedSymbolToken, isA_SymbolToken } from '../tokenizer/tokens'
import type { TokenStream } from '../tokenizer/interface'
import { getTokenDebugData } from '../tokenizer/utils'
import type {
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  NumberNode,
  ParseState,
  ReservedSymbolNode,
  StringNode,
  SymbolNode,
} from './interface'

export function parseSymbol(tokenStream: TokenStream, parseState: ParseState): SymbolNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++])
  if (!isA_SymbolToken(tkn)) {
    throw new LitsError(`Expected symbol token, got ${tkn[0]}`, getTokenDebugData(tkn)?.sourceCodeInfo)
  }
  if (tkn[1][0] !== '\'') {
    return {
      t: AstNodeType.Symbol,
      v: tkn[1],
      p: [],
      n: undefined,
      token: getTokenDebugData(tkn) && tkn,
    }
  }
  else {
    const value = tkn[1].substring(1, tkn[1].length - 1)
      .replace(
        /(\\{2})|(\\')|\\(.)/g,
        (
          _,
          backslash: string,
          singleQuote: string,
          normalChar: string,
        ) => {
          if (backslash) {
            return '\\'
          }
          if (singleQuote) {
            return '\''
          }
          return `\\${normalChar}`
        },
      )
    return {
      t: AstNodeType.Symbol,
      v: value,
      p: [],
      n: undefined,
      token: getTokenDebugData(tkn) && tkn,
    }
  }
}

export function parseReservedSymbol(tokenStream: TokenStream, parseState: ParseState): ReservedSymbolNode | NumberNode {
  const tkn = asToken(tokenStream.tokens[parseState.position++])

  if (!isA_ReservedSymbolToken(tkn)) {
    throw new LitsError(`Expected symbol token, got ${tkn[0]}`, getTokenDebugData(tkn)?.sourceCodeInfo)
  }

  if (isA_ReservedSymbolToken(tkn)) {
    const symbol = tkn[1]
    if (isNumberReservedSymbol(symbol)) {
      return {
        t: AstNodeType.Number,
        v: numberReservedSymbolRecord[symbol],
        p: [],
        n: undefined,
        token: getTokenDebugData(tkn) && tkn,
      }
    }
  }
  return {
    t: AstNodeType.ReservedSymbol,
    v: tkn[1],
    p: [],
    n: undefined,
    token: getTokenDebugData(tkn) && tkn,
  } satisfies ReservedSymbolNode
}

export function parseNumber(tokenStream: TokenStream, parseState: ParseState): NumberNode {
  const tkn = tokenStream.tokens[parseState.position++]
  if (!isA_BasePrefixedNumberToken(tkn) && !isA_NumberToken(tkn)) {
    throw new LitsError(`Expected number token, got ${tkn}`, getTokenDebugData(tkn)?.sourceCodeInfo)
  }

  const value = tkn[1]
  const negative = value[0] === '-'
  const numberString = (negative ? value.substring(1) : value).replace(/_/g, '')
  return {
    t: AstNodeType.Number,
    v: negative ? -Number(numberString) : Number(numberString),
    p: [],
    n: undefined,
    token: getTokenDebugData(tkn) && tkn,
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
    token: getTokenDebugData(tkn) && tkn,
  }
}

export function parseRegexpShorthand(tokenStream: TokenStream, parseState: ParseState): NormalExpressionNodeWithName {
  const tkn = asRegexpShorthandToken(tokenStream.tokens[parseState.position++])

  const endStringPosition = tkn[1].lastIndexOf('"')
  const regexpString = tkn[1].substring(2, endStringPosition)
  const optionsString = tkn[1].substring(endStringPosition + 1)
  const stringNode: StringNode = {
    t: AstNodeType.String,
    v: regexpString,
    p: [],
    n: undefined,
    token: getTokenDebugData(tkn) && tkn,
  }

  const optionsNode: StringNode = {
    t: AstNodeType.String,
    v: optionsString,
    p: [],
    n: undefined,
    token: getTokenDebugData(tkn) && tkn,
  }

  const node: NormalExpressionNode = {
    t: AstNodeType.NormalExpression,
    n: 'regexp',
    p: [stringNode, optionsNode],
    token: getTokenDebugData(tkn) && tkn,
  }

  return node
}
