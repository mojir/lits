import { describe, expect, test } from 'vitest'
import { tokenize } from './tokenize'

describe('tokenenizers', () => {
  test('tokenize errors', () => {
    expect(tokenize('1.e0', false, undefined).tokens[0]).toEqual(['Error', '1.e', undefined, 'Invalid number format at position 3'])
    expect(tokenize('0o8', false, undefined).tokens[0]).toEqual(['Error', '0o', undefined, 'Invalid number format at position 2'])
    expect(tokenize('0xfg', false, undefined).tokens[0]).toEqual(['Error', '0x', undefined, 'Invalid number format at position 2'])
    expect(tokenize('0a', false, undefined).tokens[0]).toEqual(['Error', '0a', undefined, 'Invalid number format at position 2'])
    expect(tokenize('"0a', false, undefined).tokens[0]).toEqual(['Error', '"0a', undefined, 'Unclosed string at position 0'])
    expect(tokenize('10_.0', false, undefined).tokens[0]).toEqual(['Error', '10_', undefined, 'Invalid number format at position 3'])
    expect(tokenize('#"ads', false, undefined).tokens[0]).toEqual(['Error', '#"ads', undefined, 'Unclosed regexp at position 0'])
    expect(tokenize('/* ', false, undefined).tokens[0]).toEqual(['Error', '/*', undefined, 'Unclosed multi-line comment at position 0'])
    expect(tokenize('\' ', false, undefined).tokens[0]).toEqual(['Error', '\' ', undefined, 'Unclosed quoted symbol at position 0'])
  })
  test('tokenize shebang', () => {
    expect(tokenize('#!...\n10', false, undefined).tokens.length).toBe(3)
    expect(tokenize('#!...', false, undefined).tokens.length).toBe(1)
  })
})
