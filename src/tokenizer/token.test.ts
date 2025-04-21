import { describe, expect, test } from 'vitest'
import { LitsError } from '../errors'
import { asA_BinaryOperatorToken, asBasePrefixedNumberToken, asLBraceToken, asLBracketToken, asLParenToken, asMultiLineCommentToken, asNumberToken, asOperatorToken, asRBraceToken, asRBracketToken, asRParenToken, asRegexpShorthandToken, asReservedSymbolToken, asSingleLineCommentToken, asStringToken, asSymbolToken, asWhitespaceToken } from './token'

describe('token', () => {
  describe('guards', () => {
    test('asSymbolToken', () => {
      expect(() => asSymbolToken(['Symbol', 'a'])).not.toThrow()
      expect(() => asSymbolToken(['Foo' as unknown as 'Symbol', 'a'])).toThrow(LitsError)
    })
    test('asReservedSymbolToken', () => {
      expect(() => asReservedSymbolToken(['ReservedSymbol', 'null'])).not.toThrow()
      expect(() => asReservedSymbolToken(['Number', '1'])).toThrow(LitsError)
    })
    test('asSingleLineCommentToken', () => {
      expect(() => asSingleLineCommentToken(['SingleLineComment', '// foo'])).not.toThrow()
      expect(() => asSingleLineCommentToken(['Number', '1'])).toThrow(LitsError)
    })
    test('asMultiLineCommentToken', () => {
      expect(() => asMultiLineCommentToken(['MultiLineComment', '/* foo */'])).not.toThrow()
      expect(() => asMultiLineCommentToken(['Number', '1'])).toThrow(LitsError)
    })
    test('asOperatorToken', () => {
      expect(() => asOperatorToken(['Operator', '*'])).not.toThrow()
      expect(() => asOperatorToken(['Number', '1'])).toThrow(LitsError)
    })
    test('asWhitespaceToken', () => {
      expect(() => asWhitespaceToken(['Whitespace', ' '])).not.toThrow()
      expect(() => asWhitespaceToken(['Number', '1'])).toThrow(LitsError)
    })
    test('asNumberToken', () => {
      expect(() => asNumberToken(['Number', '0xff'])).not.toThrow()
      expect(() => asNumberToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asBasePrefixedNumberToken', () => {
      expect(() => asBasePrefixedNumberToken(['BasePrefixedNumber', '0xff'])).not.toThrow()
      expect(() => asBasePrefixedNumberToken(['Whitespace', ' '])).toThrow(LitsError)
    })

    test('asStringToken', () => {
      expect(() => asStringToken(['String', '"asd"'])).not.toThrow()
      expect(() => asStringToken(['Whitespace', ' '])).toThrow(LitsError)
    })

    test('asRegexpShorthandToken', () => {
      expect(() => asRegexpShorthandToken(['RegexpShorthand', '#"asd"'])).not.toThrow()
      expect(() => asRegexpShorthandToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asA_BinaryOperatorToken', () => {
      expect(() => asA_BinaryOperatorToken(['Operator', '+'])).not.toThrow()
      expect(() => asA_BinaryOperatorToken(['Operator', '...'])).toThrow(LitsError)
      expect(() => asA_BinaryOperatorToken(['Whitespace', ' '])).toThrow(LitsError)
    })

    test('asLParenToken', () => {
      expect(() => asLParenToken(['LParen', '('])).not.toThrow()
      expect(() => asLParenToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asRParenToken', () => {
      expect(() => asRParenToken(['RParen', ')'])).not.toThrow()
      expect(() => asRParenToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asLBracketToken', () => {
      expect(() => asLBracketToken(['LBracket', '['])).not.toThrow()
      expect(() => asLBracketToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asRBracketToken', () => {
      expect(() => asRBracketToken(['RBracket', ']'])).not.toThrow()
      expect(() => asRBracketToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asLBraceToken', () => {
      expect(() => asLBraceToken(['LBrace', '{'])).not.toThrow()
      expect(() => asLBraceToken(['Whitespace', ' '])).toThrow(LitsError)
    })
    test('asRBraceToken', () => {
      expect(() => asRBraceToken(['RBrace', '}'])).not.toThrow()
      expect(() => asRBraceToken(['Whitespace', ' '])).toThrow(LitsError)
    })
  })
})
