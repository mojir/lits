import { describe, expect, test } from 'vitest'
import { tokenize } from './tokenize'

describe('tokenenizers', () => {
  test('tokenize', () => {
    expect(() => tokenize('1.e0', false, undefined)).toThrow()
    expect(() => tokenize('0o8', false, undefined)).toThrow()
    expect(() => tokenize('0xfg', false, undefined)).toThrow()
    expect(() => tokenize('0a', false, undefined)).toThrow()
    expect(() => tokenize('"0a', false, undefined)).toThrow()
    expect(() => tokenize('10_.0', false, undefined)).toThrow()
    expect(() => tokenize('\' ', false, undefined)).toThrow()
    expect(() => tokenize('/* ', false, undefined)).toThrow()
  })
})
