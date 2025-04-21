import { describe, expect, test } from 'vitest'
import { LitsError } from '../errors'
import { asBinaryOperator, asSymbolicOperator } from './operators'

describe('operators', () => {
  describe('guards', () => {
    test('asBinaryOperator', () => {
      expect(() => asBinaryOperator('??')).not.toThrow()
      expect(() => asBinaryOperator('...')).toThrow(LitsError)
      expect(() => asBinaryOperator('==')).toThrow(LitsError)
    })
    test('asSymbolOperator', () => {
      expect(() => asSymbolicOperator('??')).not.toThrow()
      expect(() => asSymbolicOperator('...')).not.toThrow()
      expect(() => asSymbolicOperator('==')).toThrow(LitsError)
    })
  })
})
