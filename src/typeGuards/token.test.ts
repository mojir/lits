import { describe, expect, it } from 'vitest'
import { TokenType } from '../constants/constants'
import type { Token } from '../tokenizer/interface'
import { assertToken, isToken } from './token'

describe('token type guard', () => {
  it('token', () => {
    const tkn: Token = {
      t: TokenType.Name,
      v: 'Albert',
      debugData: undefined,
    }
    const nonTkn1 = {
      ...tkn,
      t: 999,
    }
    const nonTkn2: any = {
      ...tkn,
    }
    // eslint-disable-next-line ts/no-unsafe-member-access
    delete nonTkn2.v
    expect(isToken(tkn)).toBe(true)
    expect(isToken(nonTkn1)).toBe(false)
    expect(isToken(10)).toBe(false)
    expect(() => assertToken(tkn, '')).not.toThrow()
    expect(() => assertToken(nonTkn2, '')).toThrow()
    expect(() => assertToken(nonTkn2, '')).toThrow()
    expect(() => assertToken(tkn, '', { type: TokenType.Name })).not.toThrow()
    expect(() => assertToken(tkn, '', { type: TokenType.Number })).toThrow()
    expect(() => assertToken(tkn, '', { type: TokenType.Name, value: 'Albert' })).not.toThrow()
    expect(() => assertToken(tkn, '', { type: TokenType.Name, value: 'Mojir' })).toThrow()
  })
})
