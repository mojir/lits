import { describe, expect, it } from 'vitest'
import { parse } from '../../src/parser'
import { AstNodeType } from '../../src/constants/constants'
import type { Ast } from '../../src/parser/interface'
import { tokenize } from '../../src/tokenizer'
import type { TokenStream } from '../../src/tokenizer/interface'
import type { P_CollectionAccessorToken } from '../../src/tokenizer/polish/polishTokens'
import { parsePolishToken } from '../../src/parser/PolishTokenParsers'

const program = `
(let [day (* 24 60 60 1000)])
(* days day)
`

const optimizableProgram = `
(let [day (* 24 60 60 1000)])
  (* 11 day)
`

describe('parser', () => {
  it('simple program', () => {
    const tokens = tokenize(program, { debug: true, polish: true })
    const ast = parse(tokens)
    expect(ast.b.length).toBe(2)
  })
  it('empty program', () => {
    const tokens = tokenize('', { debug: true, polish: true })
    const ast = parse(tokens)
    expect(ast.b.length).toBe(0)
  })

  it('optimization', () => {
    const tokens = tokenize(optimizableProgram, { debug: true, polish: true })
    const ast = parse(tokens)
    expect(ast.b.length).toBe(2)
  })

  it('unparsable expression', () => {
    const tokens = tokenize('(', { debug: true, polish: true, filePath: 'test.lits' })
    expect(() => parse(tokens)).toThrow()
  })

  it('parse for', () => {
    expect(() => parse(tokenize('(for [x [1 2 3]] x)', { debug: true, polish: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &let [y (* x x)]] y)', { debug: true, polish: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &let [z x] &let [y (* x x)]] y)', { debug: true, polish: true }))).toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &when (odd? x)] x)', { debug: true, polish: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &when (odd? x) &when (odd? x)] x)', { debug: true, polish: true }))).toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &while (odd? x)] x)', { debug: true, polish: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &while (odd? x) &while (odd? x)] x)', { debug: true, polish: true }))).toThrow()
    expect(() =>
      parse(
        tokenize(
          '(for [x [1 2 3] &when (odd? x) &while (!= x 3) &let [y (* x x)] y [5 10 15] z [100 200 300]] (+ x y z))',
          { debug: true, polish: true },
        ),
      ),
    ).not.toThrow()
  })

  it('parse dotNotation, check ast 1', () => {
    const tokens = tokenize('foo#1.a', { debug: false, polish: true })
    const ast = parse(tokens)
    expect(ast).toEqual<Ast>({
      hasDebugData: false,
      b: [
        {
          t: AstNodeType.NormalExpression,
          p: [
            {
              t: AstNodeType.NormalExpression,
              n: 'foo',
              p: [
                {
                  t: AstNodeType.Number,
                  v: 1,
                  token: undefined,
                  p: [],
                  n: undefined,
                },
              ],
              token: undefined,
            },
            {
              t: AstNodeType.String,
              v: 'a',
              token: undefined,
              p: [],
              n: undefined,
            },
          ],
          token: undefined,
          n: undefined,
        },
      ],
    })
  })

  it('parse dotNotation, check ast 2', () => {
    const tokens = tokenize('(#(identity %1) [1 2 3])#1', { debug: false, polish: true })
    const ast = parse(tokens)
    expect(ast).toEqual<Ast>({
      hasDebugData: false,
      b: [
        {
          t: 203,
          p: [
            {
              t: 203,
              p: [
                {
                  t: 204,
                  n: 'fn',
                  p: [],
                  o: [
                    {
                      as: {
                        b: [],
                        m: ['%1'],
                      },
                      b: [
                        {
                          t: 203,
                          n: 'identity',
                          p: [
                            {
                              t: 205,
                              v: '%1',
                              token: undefined,
                              p: [],
                              n: undefined,
                            },
                          ],
                          token: undefined,
                        },
                      ],
                      a: 1,
                    },
                  ],
                  token: undefined,
                },
                {
                  t: 203,
                  n: 'array',
                  p: [
                    {
                      t: 201,
                      v: 1,
                      token: undefined,
                      p: [],
                      n: undefined,
                    },
                    {
                      t: 201,
                      v: 2,
                      token: undefined,
                      p: [],
                      n: undefined,
                    },
                    {
                      t: 201,
                      v: 3,
                      token: undefined,
                      p: [],
                      n: undefined,
                    },
                  ],
                  token: undefined,
                },
              ],
              n: undefined,
              token: undefined,
            },
            {
              t: 201,
              v: 1,
              token: undefined,
              p: [],
              n: undefined,
            },
          ],
          token: undefined,
          n: undefined,
        },
      ],
    })
  })

  it('parseToken unknown token', () => {
    const tokenStream: TokenStream = {
      hasDebugData: false,
      polish: true,
      tokens: [
        ['CollectionAccessor', ''] as unknown as P_CollectionAccessorToken,
        ['Modifier', ''] as unknown as P_CollectionAccessorToken,
      ],
    }
    expect(() => parsePolishToken(tokenStream, { position: 0, parseToken: parsePolishToken })).toThrow()
    expect(() => parsePolishToken(tokenStream, { position: 0, parseToken: parsePolishToken })).toThrow()
  })
})
