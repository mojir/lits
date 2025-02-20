import { describe, expect, it } from 'vitest'
import { NO_MATCH } from '../common/tokenizers'
import { tokenizeIF_MultiLineComment, tokenizeIF_Operator, tokenizeIF_SingleLineComment, tokenizeIF_Symbol } from './infixTokenizers'

describe('infixTokenizers', () => {
  describe('tokenizeIF_SingleLineComment', () => {
    it('should tokenize inline comment', () => {
      expect(tokenizeIF_SingleLineComment('// comment', 0)).toEqual([10, ['IF_SingleLineComment', '// comment']])
      expect(tokenizeIF_SingleLineComment('... // comment', 4)).toEqual([10, ['IF_SingleLineComment', '// comment']])
      expect(tokenizeIF_SingleLineComment('... // comment\n...', 4)).toEqual([10, ['IF_SingleLineComment', '// comment']])
    })
  })
  describe('tokenizeIF_MultiLineComment', () => {
    it('should tokenize block comment', () => {
      expect(tokenizeIF_MultiLineComment('/* comment */', 0)).toEqual([13, ['IF_MultiLineComment', '/* comment */']])
      expect(tokenizeIF_MultiLineComment('... /* comment */', 4)).toEqual([13, ['IF_MultiLineComment', '/* comment */']])
      expect(tokenizeIF_MultiLineComment('... /* comment \n comment */ ...', 4))
        .toEqual([23, ['IF_MultiLineComment', '/* comment \n comment */']])
    })
  })
  describe('tokenizeIF_Symbol', () => {
    it('should tokenize symbol', () => {
      expect(tokenizeIF_Symbol('symbol', 0)).toEqual([6, ['IF_Symbol', 'symbol']])
      expect(tokenizeIF_Symbol('A:B', 0)).toEqual([3, ['IF_Symbol', 'A:B']])
      expect(tokenizeIF_Symbol('... `A-B`', 4)).toEqual([5, ['IF_Symbol', 'A-B']])
      expect(tokenizeIF_Symbol('... `A B`', 4)).toEqual(NO_MATCH)
    })
  })
  describe('tokenizeIF_Operator', () => {
    it('should tokenize operator', () => {
      expect(tokenizeIF_Operator('>>>', 0)).toEqual([3, ['IF_Operator', '>>>']])
      expect(tokenizeIF_Operator('<=', 0)).toEqual([2, ['IF_Operator', '<=']])
      expect(tokenizeIF_Operator('+', 0)).toEqual([1, ['IF_Operator', '+']])
      expect(tokenizeIF_Operator('...', 4)).toEqual(NO_MATCH)
    })
  })
})
