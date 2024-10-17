import { describe, expect, it } from 'vitest'
import { tokenize } from '../../src/tokenizer'
import { TokenType } from '../../src/constants/constants'

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
    expect(tokenize('"Hi" ;This is a string', { debug: false })).toEqual({ tokens: [{ t: TokenType.String, v: 'Hi' }] })
    expect(tokenize('"Hi" ;This is a string\n"there"', { debug: false })).toEqual({
      tokens: [
        { t: TokenType.String, v: 'Hi' },
        { t: TokenType.String, v: 'there' },
      ],
    })
  })

  describe('strings', () => {
    it('unclosed string', () => {
      expect(() => tokenize('"Hej', { debug: false })).toThrow()
    })
    it('escaped string', () => {
      expect(tokenize('"He\\"j"', { debug: false }).tokens[0]).toEqual({
        t: TokenType.String,
        v: 'He"j',
      })
      expect(tokenize('"He\\\\j"', { debug: false }).tokens[0]).toEqual({
        t: TokenType.String,
        v: 'He\\j',
      })
      expect(tokenize('"H\\ej"', { debug: false }).tokens[0]).toEqual({
        t: TokenType.String,
        v: 'H\\ej',
      })
    })
  })

  describe('regexpShorthand', () => {
    it('samples', () => {
      expect(tokenize('#"Hej"', { debug: true, filePath: 'foo.lits' })).toEqual({
        tokens: [
          {
            t: TokenType.RegexpShorthand,
            v: 'Hej',
            o: {},
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hej"', filePath: 'foo.lits' },
          },
        ],
        filePath: 'foo.lits',
      })
      expect(tokenize('#"Hej"g', { debug: true })).toEqual({
        tokens: [
          {
            t: TokenType.RegexpShorthand,
            v: 'Hej',
            o: { g: true },
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hej"g', getLocation: undefined },
          },
        ],
      })
      expect(tokenize('#"Hej"i', { debug: true })).toEqual({
        tokens: [
          {
            t: TokenType.RegexpShorthand,
            v: 'Hej',
            o: { i: true },
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hej"i', getLocation: undefined },
          },
        ],
      })
      expect(tokenize('#"Hej"gi', { debug: true })).toEqual({
        tokens: [
          {
            t: TokenType.RegexpShorthand,
            v: 'Hej',
            o: { i: true, g: true },
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hej"gi', getLocation: undefined },
          },
        ],
      })
      expect(tokenize('#"Hej"ig', { debug: true })).toEqual({
        tokens: [
          {
            t: TokenType.RegexpShorthand,
            v: 'Hej',
            o: { i: true, g: true },
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hej"ig', getLocation: undefined },
          },
        ],
      })
      expect(() => tokenize('#"Hej"gg', { debug: true })).toThrow()
      expect(() => tokenize('#"Hej"ii', { debug: true })).toThrow()
      expect(() => tokenize('#1', { debug: true })).toThrow()
    })
  })

  describe('fnShorthand', () => {
    it('samples', () => {
      expect(tokenize('#(', { debug: true })).toEqual({
        tokens: [
          {
            t: TokenType.FnShorthand,
            v: '#',
            sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#(' },
          },
          {
            t: TokenType.Bracket,
            v: '(',
            sourceCodeInfo: { position: { line: 1, column: 2 }, code: '#(' },
          },
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
        'foo # 10',
        'foo #10 #20 . bar',
        '[1 2 3]#1',
        '{:a 1}.a',
      ]
      for (const sample of samples)
        tokenize(sample, { debug: false })
    })
    it('illegal samples', () => {
      const illegalSamples = ['#(indentity %1)#1', '(.bar', 'foo##1', 'foo..bar', '.bar', ').1']
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
