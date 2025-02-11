import { describe, expect, it } from 'vitest'
import { tokenize } from '../../src/tokenizer'
import type { TokenStream } from '../../src/tokenizer/interface'
import type { TokenType } from '../../src/constants/constants'

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
      infix: false,
      tokens: [{ t: 'String', v: 'Hi', debugData: undefined }],
    })
    expect(tokenize('"Hi" ;This is a string\n"there"', { debug: false })).toEqual<TokenStream>({
      hasDebugData: false,
      infix: false,
      tokens: [
        { t: 'String', v: 'Hi', debugData: undefined },
        { t: 'String', v: 'there', debugData: undefined },
      ],
    })
  })

  describe('strings', () => {
    it('unclosed string', () => {
      expect(() => tokenize('"Hi', { debug: false })).toThrow()
    })
    it('escaped string', () => {
      expect(tokenize('"He\\"j"', { debug: false }).tokens[0]).toEqual({
        t: 'String',
        v: 'He"j',
      })
      expect(tokenize('"He\\\\j"', { debug: false }).tokens[0]).toEqual({
        t: 'String',
        v: 'He\\j',
      })
      expect(tokenize('"H\\ej"', { debug: false }).tokens[0]).toEqual({
        t: 'String',
        v: 'H\\ej',
      })
    })
  })

  describe('regexpShorthand', () => {
    it('samples', () => {
      expect(tokenize('#"Hi"', { debug: true, filePath: 'foo.lits' })).toEqual<TokenStream>({
        hasDebugData: true,
        infix: false,
        tokens: [
          {
            t: 'RegexpShorthand',
            v: 'Hi',
            o: {},
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"', filePath: 'foo.lits' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
          },
        ],
        filePath: 'foo.lits',
      })
      expect(tokenize('#"Hi"g', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        infix: false,
        tokens: [
          {
            t: 'RegexpShorthand',
            v: 'Hi',
            o: { g: true },
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"g' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
          },
        ],
      })
      expect(tokenize('#"Hi"i', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        infix: false,
        tokens: [
          {
            t: 'RegexpShorthand',
            v: 'Hi',
            o: { i: true },
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"i' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
          },
        ],
      })
      expect(tokenize('#"Hi"gi', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        infix: false,
        tokens: [
          {
            t: 'RegexpShorthand',
            v: 'Hi',
            o: { i: true, g: true },
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"gi' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
          },
        ],
      })
      expect(tokenize('#"Hi"ig', { debug: true })).toEqual<TokenStream>({
        hasDebugData: true,
        infix: false,
        tokens: [
          {
            t: 'RegexpShorthand',
            v: 'Hi',
            o: { i: true, g: true },
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#"Hi"ig' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
          },
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
        infix: false,
        tokens: [
          {
            t: 'FnShorthand',
            v: '#',
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 1 }, code: '#(' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
          },
          {
            t: 'Bracket',
            v: '(',
            debugData: {
              sourceCodeInfo: { position: { line: 1, column: 2 }, code: '#(' },
              metaTokens: {
                inlineCommentToken: null,
                leadingMetaTokens: [],
              },
            },
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

type TokenPrefix =
  | '' // No leading tokens
  | ';' // One leading comment
  | ';+' // Two or more leading comments
  | '_;' // Leading newline and one leading comment
  | '_;+' // Leading newline and two or more leading comments
  | '__;' // Two leading newlines and one leading comment
  | '__;+' // Two leading newlines and two or more leading comments
  | '_' // One leading newline
  | '__' // Two leading newlines
type TokenSuffix =
  | '' // No inline comment
  | ';' // Inline comment

type TokenDescription = `${TokenPrefix}${TokenType}${TokenSuffix}`
describe('tokenize comments and new lines with debug', () => {
  const samples: [string, TokenDescription[]][] = [
    ['1 ;; One', ['Number;']],
    ['\n1', ['_Number']],
    ['\n1 ;; One', ['_Number;']],
    ['\n;Her it comes  \n 1 ;; One', ['_;Number;']],
    ['\n\n1 ;; One', ['__Number;']],
    ['\n\n;Here it comes \n 1 ;; One', ['__;Number;']],
    ['\n\n;Here it comes \n ;Here it comes \n 1 ;; One', ['__;+Number;']],
    ['\n\n;Here it comes \n ;Here it comes \n ;Here it comes \n 1 ;; One', ['__;+Number;']],
    ['\n\n', []],
    [`;; One
;; Two

;;Leading
(+
  ;One
  1
  ;Two, as comment node

  2) ; Adding one and two
;; Soon done...

;; very soon done..


;; very soon done..



;; The end`, [
      'Comment',
      'Comment',
      '_;Bracket',
      'Name',
      '_;Number',
      '_Comment',
      '_Number',
      'Bracket;',
      'Comment',
      '_Comment',
      '__Comment',
      '__Comment',
    ]],
    [`
;; A
(round ;; B
  ;; C
  (+ ;; D
    ;; E
    1 ;; F
    ;; G 
    2 ;; H
    ;; I
    (/ 3 4) 5)) ;; J`, [
      '_;Bracket',
      'Name;',
      ';Bracket',
      'Name;',
      ';Number;',
      ';Number;',
      ';Bracket',
      'Name',
      'Number',
      'Number',
      'Bracket',
      'Number',
      'Bracket',
      'Bracket;',
    ]],
  ]
  testSamples(samples, true)
})

describe('tokenize comments and new lines without debug', () => {
  const samples: [string, TokenDescription[]][] = [
    ['1 ;; One', ['Number']],
    ['\n1', ['Number']],
    ['\n1 ;; One', ['Number']],
    ['\n;Her it comes  \n 1 ;; One', ['Number']],
    ['\n\n1 ;; One', ['Number']],
    ['\n\n;Here it comes \n 1 ;; One', ['Number']],
    ['\n\n;Here it comes \n ;Here it comes \n 1 ;; One', ['Number']],
    ['\n\n;Here it comes \n ;Here it comes \n ;Here it comes \n 1 ;; One', ['Number']],
    ['\n\n', []],
    [`;; One
;; Two

;;Leading
(+
  ;One
  1
  ;Two, as comment node

  2) ; Adding one and two
;; Soon done...

;; very soon done..


;; very soon done..



;; The end`, ['Bracket', 'Name', 'Number', 'Number', 'Bracket']],

  ]
  testSamples(samples, false)
})

function testSamples(samples: [string, TokenDescription[]][], debug: boolean) {
  for (const sampel of samples) {
    const [input, expected] = sampel
    it(input, () => {
      const tokenStream = tokenize(input, { debug })
      const actual = tokenStream.tokens.map<TokenDescription>((token) => {
        const prefix: TokenPrefix = token.debugData?.metaTokens.leadingMetaTokens?.reduce<TokenPrefix>((acc, metaToken) => {
          if (metaToken.t === 'NewLine')
            return `${acc}_` as TokenPrefix
          if (metaToken.t === 'Comment') {
            return acc.endsWith(';')
              ? `${acc}+` as TokenPrefix
              : acc.endsWith('+')
                ? acc
                : `${acc};` as TokenPrefix
          }
          return acc
        }, '') ?? ''
        const suffix: TokenSuffix = token.debugData?.metaTokens.inlineCommentToken ? ';' : ''

        return `${prefix}${token.t}${suffix}`
      })
      expect(actual).toEqual(expected)
    })
  }
}
