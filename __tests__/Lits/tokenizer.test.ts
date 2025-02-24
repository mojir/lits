import { describe, expect, it } from 'vitest'
import { tokenize } from '../../src/tokenizer'
import type { TokenStream } from '../../src/tokenizer/interface'

describe('tokenizer', () => {
  it('simple expressions', () => {
    const tokenStream = tokenize(
      `
      (let ((day (* 24 60 60 1000)))
        (* days day)
      )`,
      { debug: false },
    )
    expect(tokenStream.tokens.length).toBeGreaterThan(0)
  })
  it('another simple expressions', () => {
    const tokenStream = tokenize('(do-me)', { debug: false })
    expect(tokenStream.tokens.length).toBeGreaterThan(0)
  })

  it('forbidden reserved names', () => {
    expect(() => tokenize('nil', { debug: false })).not.toThrow()
    expect(() => tokenize('false', { debug: false })).not.toThrow()
    expect(() => tokenize('true', { debug: false })).not.toThrow()
    expect(() => tokenize('null', { debug: false })).toThrow()
    expect(() => tokenize('undefined', { debug: false })).toThrow()
    expect(() => tokenize('===', { debug: false })).toThrow()
    expect(() => tokenize('!==', { debug: false })).toThrow()
    expect(() => tokenize('&&', { debug: false })).toThrow()
    expect(() => tokenize('||', { debug: false })).toThrow()
  })

  it('comments', () => {
    expect(tokenize('"Hi" ;This is a string', { debug: false })).toEqual<TokenStream>({
      hasDebugData: false,
      algebraic: false,
      tokens: [['String', '"Hi"'], ['P_Whitespace', ' '], ['P_Comment', ';This is a string']],
    })
    expect(tokenize('"Hi" ;This is a string\n"there"', { debug: false })).toEqual<TokenStream>({
      hasDebugData: false,
      algebraic: false,
      tokens: [
        ['String', '"Hi"'],
        ['P_Whitespace', ' '],
        ['P_Comment', ';This is a string'],
        ['P_Whitespace', '\n'],
        ['String', '"there"'],
      ],
    })
  })

  describe('strings', () => {
    it('unclosed string', () => {
      expect(() => tokenize('"Hi', { debug: false })).toThrow()
    })
    it('escaped string', () => {
      expect(tokenize('"He\\"j"', { debug: false }).tokens[0]).toEqual(['String', '"He\\"j"'])
      expect(tokenize('"He\\\\j"', { debug: false }).tokens[0]).toEqual(['String', '"He\\\\j"'])
      expect(tokenize('"H\\ej"', { debug: false }).tokens[0]).toEqual(['String', '"H\\ej"'])
    })
  })

  describe('regexpShorthand', () => {
    it('samples', () => {
      expect(tokenize('#"Hi"', { debug: true, filePath: 'foo.lits' })).toEqual<TokenStream>({
        hasDebugData: true,
        algebraic: false,
        tokens: [
          ['P_RegexpShorthand', '#"Hi"', {
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"', filePath: 'foo.lits' },
          }],
        ],
        filePath: 'foo.lits',
      })
      expect(tokenize('#"Hi"g', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        algebraic: false,
        tokens: [
          ['P_RegexpShorthand', '#"Hi"g', {
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"g' },
          }],
        ],
      })
      expect(tokenize('#"Hi"i', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        algebraic: false,
        tokens: [
          ['P_RegexpShorthand', '#"Hi"i', {
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"i' },
          }],
        ],
      })
      expect(tokenize('#"Hi"gi', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        algebraic: false,
        tokens: [
          ['P_RegexpShorthand', '#"Hi"gi', {
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"gi' },
          }],
        ],
      })
      expect(tokenize('#"Hi"ig', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        algebraic: false,
        tokens: [
          ['P_RegexpShorthand', '#"Hi"ig', {
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"ig' },
          }],
        ],
      })
      expect(() => tokenize('#"Hi"gg', { debug: true })).toThrow()
      expect(() => tokenize('#"Hi"ii', { debug: true })).toThrow()
      expect(() => tokenize('#1', { debug: true })).toThrow()
    })
  })

  describe('fnShorthand', () => {
    it('samples', () => {
      expect(tokenize('#(', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        algebraic: false,
        tokens: [
          ['P_FnShorthand', {
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#(' },
          }],
          ['LParen', {
            sourceCodeInfo: { position: { line: 1, column: 2 }, code: '#(' },
          }],
        ],
      })
      expect(() => tokenize('#', { debug: true })).toThrow()
    })
  })

  describe('dotExpression', () => {
    it('samples', () => {
      const samples = [
        '(#(indentity %1) [1 2 3])#1',
        'x#1.bar',
        'x.y.z#1#2',
        'foo.bar',
        'foo#10',
        '[1 2 3]#1',
        '{:a 1}.a',
      ]
      for (const sample of samples)
        tokenize(sample, { debug: false })
    })
    it('illegal samples', () => {
      const illegalSamples = ['#(indentity %1)#1', '(.bar', 'foo##1', 'foo..bar', '.bar', ').1', 'foo # 10', 'foo #10 #20 . bar']
      for (const sample of illegalSamples) {
        try {
          tokenize(sample, { debug: false })
          throw new Error('Expected to throw an error')
        }
        catch {
          // Expected
        }
      }
    })
  })
})
