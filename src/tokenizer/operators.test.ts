import { describe, expect, test } from 'vitest'
import { asBinaryOperator, asSymbolicOperator } from './operators'

describe('operators', () => {
  describe('guards', () => {
    test('asBinaryOperator', () => {
      expect(() => asBinaryOperator('??')).not.toThrow()
      expect(() => asBinaryOperator('...')).toThrow()
      expect(() => asBinaryOperator('==')).toThrow()
    })
    test('asSymbolOperator', () => {
      expect(() => asSymbolicOperator('??')).not.toThrow()
      expect(() => asSymbolicOperator('...')).not.toThrow()
      expect(() => asSymbolicOperator('==')).toThrow()
    })
  })
})
