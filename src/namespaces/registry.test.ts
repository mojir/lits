import { describe, expect, it } from 'vitest'
import { getNamespaceNames, hasNamespace, registerNamespace } from './index'
import type { LitsNamespace } from './interface'

describe('namespace registry', () => {
  describe('registerNamespace', () => {
    it('should throw when registering a namespace that already exists', () => {
      // 'Grid' is already registered by the main registration
      const duplicateNamespace: LitsNamespace = {
        name: 'Grid',
        functions: {},
      }
      expect(() => registerNamespace(duplicateNamespace)).toThrow('Namespace \'Grid\' is already registered')
    })
  })

  describe('hasNamespace', () => {
    it('should return true for registered namespaces', () => {
      expect(hasNamespace('Grid')).toBe(true)
      expect(hasNamespace('Assert')).toBe(true)
    })

    it('should return false for unregistered namespaces', () => {
      expect(hasNamespace('NonExistentNamespace')).toBe(false)
    })
  })

  describe('getNamespaceNames', () => {
    it('should return an array of registered namespace names', () => {
      const names = getNamespaceNames()
      expect(Array.isArray(names)).toBe(true)
      expect(names).toContain('Grid')
      expect(names).toContain('Assert')
    })
  })
})
