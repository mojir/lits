import { describe, expect, it } from 'vitest'
import { parse } from '../../src/parser'
import { AstNodeType } from '../../src/constants/constants'
import type { Ast } from '../../src/parser/interface'
import { parseToken } from '../../src/parser/parsers'
import { tokenize } from '../../src/tokenizer'
import type { TokenStream } from '../../src/tokenizer/interface'
import type { CollectionAccessorToken } from '../../src/tokenizer/Token'

const program = `
(let [day (* 24 60 60 1000)]
  (* days day)
)`

const optimizableProgram = `
(let [day (* 24 60 60 1000)]
  (* 11 day)
)`

describe('parser', () => {
  it('simple program', () => {
    const tokens = tokenize(program, { debug: true })
    const ast = parse(tokens)
    expect(ast.b.length).toBe(1)
  })
  it('empty program', () => {
    const tokens = tokenize('', { debug: true })
    const ast = parse(tokens)
    expect(ast.b.length).toBe(0)
  })

  it('optimization', () => {
    const tokens = tokenize(optimizableProgram, { debug: true })
    const ast = parse(tokens)
    expect(ast.b.length).toBe(1)
  })

  it('unparsable expression', () => {
    const tokens = tokenize('(', { debug: true, filePath: 'test.lits' })
    expect(() => parse(tokens)).toThrow()
  })

  it('parse for', () => {
    expect(() => parse(tokenize('(for [x [1 2 3]] x)', { debug: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &let [y (* x x)]] y)', { debug: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &let [z x] &let [y (* x x)]] y)', { debug: true }))).toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &when (odd? x)] x)', { debug: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &when (odd? x) &when (odd? x)] x)', { debug: true }))).toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &while (odd? x)] x)', { debug: true }))).not.toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &while (odd? x) &while (odd? x)] x)', { debug: true }))).toThrow()
    expect(() => parse(tokenize('(for [x [1 2 3] &while (odd? x) &whil (odd? x)] x)', { debug: true }))).toThrow()
    expect(() =>
      parse(
        tokenize(
          '(for [x [1 2 3] &when (odd? x) &while (not= x 3) &let [y (* x x)] y [5 10 15] z [100 200 300]] (+ x y z))',
          { debug: true },
        ),
      ),
    ).not.toThrow()
  })

  it('parse dotNotation, check ast 1', () => {
    const tokens = tokenize('foo#1.a', { debug: false })
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
                  debugData: undefined,
                  p: [],
                  n: undefined,
                },
              ],
              debugData: undefined,
            },
            {
              t: AstNodeType.String,
              v: 'a',
              debugData: undefined,
              p: [],
              n: undefined,
            },
          ],
          debugData: undefined,
          n: undefined,
        },
      ],
    })
  })

  it('parse dotNotation, check ast 2', () => {
    const tokens = tokenize('(#(identity %1) [1 2 3])#1', { debug: false })
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
                              debugData: undefined,
                              p: [],
                              n: undefined,
                            },
                          ],
                          debugData: undefined,
                        },
                      ],
                      a: 1,
                    },
                  ],
                  debugData: undefined,
                },
                {
                  t: 203,
                  n: 'array',
                  p: [
                    {
                      t: 201,
                      v: 1,
                      debugData: undefined,
                      p: [],
                      n: undefined,
                    },
                    {
                      t: 201,
                      v: 2,
                      debugData: undefined,
                      p: [],
                      n: undefined,
                    },
                    {
                      t: 201,
                      v: 3,
                      debugData: undefined,
                      p: [],
                      n: undefined,
                    },
                  ],
                  debugData: undefined,
                },
              ],
              n: undefined,
              debugData: undefined,
            },
            {
              t: 201,
              v: 1,
              debugData: undefined,
              p: [],
              n: undefined,
            },
          ],
          debugData: undefined,
          n: undefined,
        },
      ],
    })
  })

  it('parseToken unknown token', () => {
    const tokenStream: TokenStream = {
      hasDebugData: false,
      infix: false,
      tokens: [
        ['CollectionAccessor', ''] as unknown as CollectionAccessorToken,
        ['Modifier', ''] as unknown as CollectionAccessorToken,
      ],
    }
    expect(() => parseToken(tokenStream, { position: 0, infix: false })).toThrow()
    expect(() => parseToken(tokenStream, { position: 0, infix: false })).toThrow()
  })
})
