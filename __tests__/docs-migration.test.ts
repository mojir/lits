import { describe, expect, it } from 'vitest'
import { bitwiseNormalExpression } from '../src/builtin/core/bitwise'
import { arrayNormalExpression } from '../src/builtin/core/array'
import { collectionNormalExpression } from '../src/builtin/core/collection'
import { functionalNormalExpression } from '../src/builtin/core/functional'
import { mathNormalExpression } from '../src/builtin/core/math'
import { getMetaNormalExpression } from '../src/builtin/core/meta'
import { miscNormalExpression } from '../src/builtin/core/misc'
import { objectNormalExpression } from '../src/builtin/core/object'
import { predicatesNormalExpression } from '../src/builtin/core/predicates'
import { regexpNormalExpression } from '../src/builtin/core/regexp'
import { sequenceNormalExpression } from '../src/builtin/core/sequence'
import { stringNormalExpression } from '../src/builtin/core/string'
import { bitwiseReference as legacyBitwiseReference } from '../reference/categories/bitwise'
import { arrayReference as legacyArrayReference } from '../reference/categories/array'
import { collectionReference as legacyCollectionReference } from '../reference/categories/collection'
import { functionalReference as legacyFunctionalReference } from '../reference/categories/functional'
import { mathReference as legacyMathReference } from '../reference/categories/math'
import { metaReference as legacyMetaReference } from '../reference/categories/meta'
import { miscReference as legacyMiscReference } from '../reference/categories/misc'
import { objectReference as legacyObjectReference } from '../reference/categories/object'
import { predicateReference as legacyPredicateReference } from '../reference/categories/predicate'
import { regularExpressionReference as legacyRegexpReference } from '../reference/categories/regularExpression'
import { sequenceReference as legacySequenceReference } from '../reference/categories/sequence'
import { stringReference as legacyStringReference } from '../reference/categories/string'
import type { FunctionReference } from '../reference'
import { allReference } from '../reference'

// Normalize trailing whitespace on each line (legacy reference files may have trailing spaces)
function normalizeDesc(s: string): string {
  return s.replace(/[ \t]+$/gm, '')
}

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
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: regexp', () => {
  it('every regexp function has a docs field', () => {
    for (const [key, expr] of Object.entries(regexpNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every regexp docs has category "Regular expression"', () => {
    for (const [key, expr] of Object.entries(regexpNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Regular expression')
    }
  })

  it('regexp docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(regexpNormalExpression).sort()
    const legacyKeys = Object.keys(legacyRegexpReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('regexp docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyRegexpReference)) {
      const expr = regexpNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: array', () => {
  it('every array function has a docs field', () => {
    for (const [key, expr] of Object.entries(arrayNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every array docs has category "Array"', () => {
    for (const [key, expr] of Object.entries(arrayNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Array')
    }
  })

  it('array docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(arrayNormalExpression).sort()
    const legacyKeys = Object.keys(legacyArrayReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('array docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyArrayReference)) {
      const expr = arrayNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: object', () => {
  it('every object function has a docs field', () => {
    for (const [key, expr] of Object.entries(objectNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every object docs has category "Object"', () => {
    for (const [key, expr] of Object.entries(objectNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Object')
    }
  })

  it('object docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(objectNormalExpression).sort()
    const legacyKeys = Object.keys(legacyObjectReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('object docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyObjectReference)) {
      const expr = objectNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: functional', () => {
  it('every functional function has a docs field', () => {
    for (const [key, expr] of Object.entries(functionalNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every functional docs has category "Functional"', () => {
    for (const [key, expr] of Object.entries(functionalNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Functional')
    }
  })

  it('functional docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(functionalNormalExpression).sort()
    const legacyKeys = Object.keys(legacyFunctionalReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('functional docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyFunctionalReference)) {
      const expr = functionalNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: meta', () => {
  const metaNormalExpression = getMetaNormalExpression({} as Record<string, FunctionReference>)

  it('every meta function has a docs field', () => {
    for (const [key, expr] of Object.entries(metaNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every meta docs has category "Meta"', () => {
    for (const [key, expr] of Object.entries(metaNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Meta')
    }
  })

  it('meta docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(metaNormalExpression).sort()
    const legacyKeys = Object.keys(legacyMetaReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('meta docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyMetaReference)) {
      const expr = metaNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: predicates', () => {
  it('every predicate function has a docs field', () => {
    for (const [key, expr] of Object.entries(predicatesNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every predicate docs has category "Predicate"', () => {
    for (const [key, expr] of Object.entries(predicatesNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Predicate')
    }
  })

  it('predicate docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(predicatesNormalExpression).sort()
    const legacyKeys = Object.keys(legacyPredicateReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('predicate docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyPredicateReference)) {
      const expr = predicatesNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: misc', () => {
  it('every misc function has a docs field', () => {
    for (const [key, expr] of Object.entries(miscNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every misc docs has category "Misc"', () => {
    for (const [key, expr] of Object.entries(miscNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Misc')
    }
  })

  it('misc docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(miscNormalExpression).sort()
    const legacyKeys = Object.keys(legacyMiscReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('misc docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyMiscReference)) {
      const expr = miscNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: string', () => {
  it('every string function has a docs field', () => {
    for (const [key, expr] of Object.entries(stringNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every string docs has category "String"', () => {
    for (const [key, expr] of Object.entries(stringNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('String')
    }
  })

  it('string docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(stringNormalExpression).sort()
    const legacyKeys = Object.keys(legacyStringReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('string docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyStringReference)) {
      const expr = stringNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: sequence', () => {
  it('every sequence function has a docs field', () => {
    for (const [key, expr] of Object.entries(sequenceNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every sequence docs has category "Sequence"', () => {
    for (const [key, expr] of Object.entries(sequenceNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Sequence')
    }
  })

  it('sequence docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(sequenceNormalExpression).sort()
    const legacyKeys = Object.keys(legacySequenceReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('sequence docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacySequenceReference)) {
      const expr = sequenceNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: collection', () => {
  it('every collection function has a docs field', () => {
    for (const [key, expr] of Object.entries(collectionNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every collection docs has category "Collection"', () => {
    for (const [key, expr] of Object.entries(collectionNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Collection')
    }
  })

  it('collection docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(collectionNormalExpression).sort()
    const legacyKeys = Object.keys(legacyCollectionReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('collection docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyCollectionReference)) {
      const expr = collectionNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
      expect(docs.returns).toEqual(legacyRef.returns)
      expect(docs.args).toEqual(legacyRef.args)
      expect(docs.variants).toEqual(legacyRef.variants)
      expect(docs.examples).toEqual(legacyRef.examples)
    }
  })
})

describe('co-located docs: math', () => {
  it('every math function has a docs field', () => {
    for (const [key, expr] of Object.entries(mathNormalExpression)) {
      expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
    }
  })

  it('every math docs has category "Math"', () => {
    for (const [key, expr] of Object.entries(mathNormalExpression)) {
      expect(expr.docs!.category, `"${key}" has wrong category`).toBe('Math')
    }
  })

  it('math docs keys match legacy reference keys', () => {
    const docsKeys = Object.keys(mathNormalExpression).sort()
    const legacyKeys = Object.keys(legacyMathReference).sort()
    expect(docsKeys).toEqual(legacyKeys)
  })

  it('math docs data matches legacy reference data', () => {
    for (const [key, legacyRef] of Object.entries(legacyMathReference)) {
      const expr = mathNormalExpression[key]!
      const docs = expr.docs!
      expect(docs.category).toBe(legacyRef.category)
      expect(normalizeDesc(docs.description)).toBe(normalizeDesc(legacyRef.description))
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
