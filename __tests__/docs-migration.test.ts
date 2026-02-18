import { describe, expect, it } from 'vitest'
import { bitwiseNormalExpression } from '../src/builtin/core/bitwise'
import { bitwiseReference as legacyBitwiseReference } from '../reference/categories/bitwise'
import { allReference } from '../reference'

// --- Phase 1: Validate co-located docs for migrated categories ---

describe('co-located docs: bitwise', () => {
  it('every bitwise function has a docs field', () => {
    for (const [key, expr] of Object.entries(bitwiseNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every bitwise docs has category "Bitwise"', () => {
    for (const [key, expr] of Object.entries(bitwiseNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Bitwise')
    }
  })

  it('bitwise docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(bitwiseNormalExpression).sort()
    const legacyKeys = Object.keys(legacyBitwiseReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('bitwise docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyBitwiseReference)) {
      const expr = bitwiseNormalExpression[key]!
      const docs = expr.docs!

      // Compare each field that exists in the legacy reference
      expect(docs.category).toBe(legacyRef.category)
      expect(docs.description).toBe(legacyRef.description)
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

// --- Snapshot: reference output stability ---

describe('reference output stability', () => {
  it('allReference snapshot is stable', () => {
    expect(JSON.stringify(allReference, null, 2)).toMatchSnapshot()
  })
})
