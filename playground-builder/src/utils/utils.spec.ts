import { describe, expect, it } from 'vitest'
import { findAllOccurrences } from '../../../common/utils'

describe('findAllOccurrences', () => {
  it('should find all occurrences of a pattern in the input string', () => {
    const input = 'Hello, hello, hello!'
    const pattern = /hello/gi
    const result = findAllOccurrences(input, pattern)
    expect(result.size).toBe(2)
    expect(result.has('hello')).toBe(true)
    expect(result.has('Hello')).toBe(true)
  })

  it('should return an empty set if no occurrences are found', () => {
    const input = 'Hello, world!'
    const pattern = /foo/gi
    const result = findAllOccurrences(input, pattern)
    expect(result.size).toBe(0)
  })

  it('should handle special characters in the pattern', () => {
    const input = 'Hello, world!'
    const pattern = /[!@#$%^&*()]/g
    const result = findAllOccurrences(input, pattern)
    expect(result.size).toBe(1)
    expect(result.has('!')).toBe(true)
  })
})
