import { describe, expect, it } from 'vitest'
import { NO_MATCH, tokenizeBasePrefixedNumber, tokenizeMultiLineComment, tokenizeNumber, tokenizeOperator, tokenizeSingleLineComment, tokenizeSymbol } from './tokenizers'

describe('tokenizers', () => {
  describe('tokenizeSingleLineComment', () => {
    it('should tokenize inline comment', () => {
      expect(tokenizeSingleLineComment('// comment', 0)).toEqual([10, ['SingleLineComment', '// comment']])
      expect(tokenizeSingleLineComment('... // comment', 4)).toEqual([10, ['SingleLineComment', '// comment']])
      expect(tokenizeSingleLineComment('... // comment\n...', 4)).toEqual([10, ['SingleLineComment', '// comment']])
    })
  })
  describe('tokenizeMultiLineComment', () => {
    it('should tokenize block comment', () => {
      expect(tokenizeMultiLineComment('/* comment */', 0)).toEqual([13, ['MultiLineComment', '/* comment */']])
      expect(tokenizeMultiLineComment('... /* comment */', 4)).toEqual([13, ['MultiLineComment', '/* comment */']])
      expect(tokenizeMultiLineComment('... /* comment \n comment */ ...', 4))
        .toEqual([23, ['MultiLineComment', '/* comment \n comment */']])
    })
  })
  describe('tokenizeSymbol', () => {
    it('should tokenize symbol', () => {
      expect(tokenizeSymbol('symbol', 0)).toEqual([6, ['Symbol', 'symbol']])
      expect(tokenizeSymbol('A:B', 0)).toEqual([3, ['Symbol', 'A:B']])
      expect(tokenizeSymbol('Grid1!A:B', 0)).toEqual([9, ['Symbol', 'Grid1!A:B']])
      expect(tokenizeSymbol('number?', 0)).toEqual([7, ['Symbol', 'number?']])
      expect(tokenizeSymbol('... A-B', 4)).toEqual([3, ['Symbol', 'A-B']])
      expect(tokenizeSymbol('... \'A B\'', 4)).toEqual([5, ['Symbol', '\'A B\'']])
      expect(tokenizeSymbol('... \'A\\\'B\'', 4)).toEqual([6, ['Symbol', '\'A\\\'B\'']])
    })
  })
  describe('tokenizeOperator', () => {
    it('should tokenize operator', () => {
      expect(tokenizeOperator('>>>', 0)).toEqual([3, ['Operator', '>>>']])
      expect(tokenizeOperator('<=', 0)).toEqual([2, ['Operator', '<=']])
      expect(tokenizeOperator('...', 4)).toEqual(NO_MATCH)
    })
  })
  describe('tokenizeNumber', () => {
    it('should tokenize operator', () => {
      expect(tokenizeNumber('1', 0)).toEqual([1, ['Number', '1']])
      expect(tokenizeBasePrefixedNumber('0xF', 0)).toEqual([3, ['BasePrefixedNumber', '0xF']])
      expect(tokenizeBasePrefixedNumber('0o7', 0)).toEqual([3, ['BasePrefixedNumber', '0o7']])
      expect(tokenizeBasePrefixedNumber('0b1100', 0)).toEqual([6, ['BasePrefixedNumber', '0b1100']])
      expect(tokenizeNumber('-1', 0)).toEqual([2, ['Number', '-1']])
      expect(tokenizeNumber('1.12', 0)).toEqual([4, ['Number', '1.12']])
      expect(tokenizeNumber('-1.12', 0)).toEqual([5, ['Number', '-1.12']])
      expect(tokenizeNumber('1_000', 0)).toEqual([5, ['Number', '1_000']])
      expect(tokenizeNumber('.12', 0)).toEqual([0])
    })
  })
})
