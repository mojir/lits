import { describe, expect, it } from 'vitest'
import { LitsError, RecurSignal, UserDefinedError, isLitsError } from '../src/errors'

describe('errors', () => {
  it('recurSignal', () => {
    const err = new RecurSignal([100])
    expect(err).toBeInstanceOf(RecurSignal)
    expect(err.name).toBe('RecurSignal')
    expect(err.params).toEqual([100])
  })
  it('userDefinedError', () => {
    const err = new UserDefinedError('A message', {
      position: {
        line: 1,
        column: 1,
      },
      code: '(+ 1 2)',
    })
    expect(err).toBeInstanceOf(UserDefinedError)
    expect(err.name).toBe('UserDefinedError')
    expect(err.message).toBe('A message\n(+ 1 2)\n^      ')
  })
  describe('isLitsError', () => {
    it('isLitsError', () => {
      const error = new Error('An error')
      const litsError = new LitsError('An error', undefined)
      const recurSignal = new RecurSignal([100])
      const userDefinedError = new UserDefinedError('An error')

      expect(isLitsError(litsError)).toBe(true)
      expect(isLitsError(userDefinedError)).toBe(true)

      expect(isLitsError(error)).toBe(false)
      expect(isLitsError(recurSignal)).toBe(false)
      expect(isLitsError({})).toBe(false)
      expect(isLitsError(null)).toBe(false)
      expect(isLitsError(undefined)).toBe(false)
    })
  })
})
