import { describe, expect, it } from 'vitest'
import { getStandardEffectHandler, standardEffectNames } from './standardEffects'
import type { ContinuationStack } from './frames'

// A minimal continuation stack for testing — standard effects don't inspect frames
const emptyK: ContinuationStack = []

describe('standardEffects', () => {
  describe('standardEffectNames', () => {
    it('should contain the 4 standard effects', () => {
      expect(standardEffectNames).toEqual(new Set([
        'lits.log',
        'lits.now',
        'lits.random',
        'lits.sleep',
      ]))
    })
  })

  describe('getStandardEffectHandler', () => {
    it('should return a handler for known effects', () => {
      expect(getStandardEffectHandler('lits.log')).toBeTypeOf('function')
      expect(getStandardEffectHandler('lits.now')).toBeTypeOf('function')
      expect(getStandardEffectHandler('lits.random')).toBeTypeOf('function')
      expect(getStandardEffectHandler('lits.sleep')).toBeTypeOf('function')
    })

    it('should return undefined for unknown effects', () => {
      expect(getStandardEffectHandler('llm.complete')).toBeUndefined()
      expect(getStandardEffectHandler('lits.unknown')).toBeUndefined()
      expect(getStandardEffectHandler('')).toBeUndefined()
    })
  })

  describe('lits.log handler', () => {
    it('should return a ValueStep with null', () => {
      const handler = getStandardEffectHandler('lits.log')!
      const result = handler(['hello', 42], emptyK)
      expect(result).toEqual({ type: 'Value', value: null, k: emptyK })
    })
  })

  describe('lits.now handler', () => {
    it('should return a ValueStep with a number', () => {
      const handler = getStandardEffectHandler('lits.now')!
      const before = Date.now()
      const result = handler([], emptyK) as { type: string, value: number, k: unknown }
      const after = Date.now()
      expect(result.type).toBe('Value')
      expect(result.value).toBeGreaterThanOrEqual(before)
      expect(result.value).toBeLessThanOrEqual(after)
    })
  })

  describe('lits.random handler', () => {
    it('should return a ValueStep with a number in [0, 1)', () => {
      const handler = getStandardEffectHandler('lits.random')!
      const result = handler([], emptyK) as { type: string, value: number, k: unknown }
      expect(result.type).toBe('Value')
      expect(result.value).toBeGreaterThanOrEqual(0)
      expect(result.value).toBeLessThan(1)
    })
  })

  describe('lits.sleep handler', () => {
    it('should return a Promise that resolves with a ValueStep', async () => {
      const handler = getStandardEffectHandler('lits.sleep')!
      const result = handler([10], emptyK)
      expect(result).toBeInstanceOf(Promise)
      const step = await result
      expect(step).toEqual({ type: 'Value', value: null, k: emptyK })
    })

    it('should throw on negative ms', () => {
      const handler = getStandardEffectHandler('lits.sleep')!
      expect(() => handler([-1], emptyK)).toThrow('non-negative number')
    })

    it('should throw on non-number argument', () => {
      const handler = getStandardEffectHandler('lits.sleep')!
      expect(() => handler(['fast'], emptyK)).toThrow('non-negative number')
    })
  })
})
