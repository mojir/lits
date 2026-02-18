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
import { allReference, isCustomReference, isDatatypeReference, isFunctionReference, isShorthandReference, normalExpressionReference } from '../reference'
import { normalExpressionKeys, specialExpressions } from '../src/builtin'
import { specialExpressionTypes } from '../src/builtin/specialExpressionTypes'
import { isFunctionDocs } from '../src/builtin/interface'
import type { BuiltinNormalExpressions, Category, FunctionDocs } from '../src/builtin/interface'
import { assertNamespace } from '../src/builtin/namespaces/assert'
import { gridNamespace } from '../src/builtin/namespaces/grid'
import { randomNamespace } from '../src/builtin/namespaces/random'
import { vectorNamespace } from '../src/builtin/namespaces/vector'
import { linearAlgebraNamespace } from '../src/builtin/namespaces/linearAlgebra'
import { matrixNamespace } from '../src/builtin/namespaces/matrix'
import { numberTheoryNamespace } from '../src/builtin/namespaces/numberTheory'
import { isApiName } from '../reference/api'

// ============================================================================
// Core category expression maps with their expected category strings
// ============================================================================

const coreCategories: Array<{ name: string, expressions: BuiltinNormalExpressions, category: Category }> = [
  { name: 'bitwise', expressions: bitwiseNormalExpression, category: 'Bitwise' },
  { name: 'array', expressions: arrayNormalExpression, category: 'Array' },
  { name: 'collection', expressions: collectionNormalExpression, category: 'Collection' },
  { name: 'functional', expressions: functionalNormalExpression, category: 'Functional' },
  { name: 'math', expressions: mathNormalExpression, category: 'Math' },
  { name: 'meta', expressions: getMetaNormalExpression(normalExpressionReference), category: 'Meta' },
  { name: 'misc', expressions: miscNormalExpression, category: 'Misc' },
  { name: 'object', expressions: objectNormalExpression, category: 'Object' },
  { name: 'predicates', expressions: predicatesNormalExpression, category: 'Predicate' },
  { name: 'regexp', expressions: regexpNormalExpression, category: 'Regular expression' },
  { name: 'sequence', expressions: sequenceNormalExpression, category: 'Sequence' },
  { name: 'string', expressions: stringNormalExpression, category: 'String' },
]

const namespaceEntries = [
  { namespace: assertNamespace, category: 'Assert' as Category },
  { namespace: gridNamespace, category: 'Grid' as Category },
  { namespace: randomNamespace, category: 'Random' as Category },
  { namespace: vectorNamespace, category: 'Vector' as Category },
  { namespace: linearAlgebraNamespace, category: 'Linear Algebra' as Category },
  { namespace: matrixNamespace, category: 'Matrix' as Category },
  { namespace: numberTheoryNamespace, category: 'Number Theory' as Category },
]

const coreCategoryNames = new Set(coreCategories.map(c => c.category))
const namespaceCategoryNames = new Set(namespaceEntries.map(n => n.category))

// ============================================================================
// 1. Every function has docs with a valid category
// ============================================================================

describe('every function has docs with a valid category', () => {
  for (const { name, expressions, category } of coreCategories) {
    describe(`core: ${name}`, () => {
      it('every function has a docs field', () => {
        for (const [key, expr] of Object.entries(expressions)) {
          expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
        }
      })
      it(`every function has category "${category}"`, () => {
        for (const [key, expr] of Object.entries(expressions)) {
          expect(expr.docs!.category, `"${key}" has wrong category`).toBe(category)
        }
      })
      it('every docs has description, returns, args, variants, and examples', () => {
        for (const [key, expr] of Object.entries(expressions)) {
          const docs: FunctionDocs = expr.docs!
          expect(typeof docs.description, `"${key}" description`).toBe('string')
          expect(docs.description.length, `"${key}" description is empty`).toBeGreaterThan(0)
          expect(docs.returns, `"${key}" returns`).toBeDefined()
          expect(docs.args, `"${key}" args`).toBeDefined()
          expect(Array.isArray(docs.variants), `"${key}" variants`).toBe(true)
          expect(docs.variants.length, `"${key}" has no variants`).toBeGreaterThan(0)
          expect(Array.isArray(docs.examples), `"${key}" examples`).toBe(true)
          expect(docs.examples.length, `"${key}" has no examples`).toBeGreaterThan(0)
        }
      })
    })
  }

  describe('special expressions', () => {
    it('documented special expressions have docs with category "Special expression"', () => {
      for (const [name, index] of Object.entries(specialExpressionTypes)) {
        const expr = specialExpressions[index]
        if (!expr?.docs)
          continue
        expect(expr.docs.category, `"${name}" has wrong category`).toBe('Special expression')
      }
    })

    it('documented special expressions have description and examples', () => {
      for (const [name, index] of Object.entries(specialExpressionTypes)) {
        const expr = specialExpressions[index]
        if (!expr?.docs)
          continue
        const docs = expr.docs
        expect(typeof docs.description, `"${name}" description`).toBe('string')
        expect(docs.description.length, `"${name}" description is empty`).toBeGreaterThan(0)
        expect(Array.isArray(docs.examples), `"${name}" examples`).toBe(true)
        expect(docs.examples.length, `"${name}" has no examples`).toBeGreaterThan(0)
      }
    })

    it('functionDocs special expressions have returns, args, and variants', () => {
      for (const [name, index] of Object.entries(specialExpressionTypes)) {
        const expr = specialExpressions[index]
        if (!expr?.docs || !isFunctionDocs(expr.docs))
          continue
        const docs = expr.docs
        expect(docs.returns, `"${name}" returns`).toBeDefined()
        expect(docs.args, `"${name}" args`).toBeDefined()
        expect(Array.isArray(docs.variants), `"${name}" variants`).toBe(true)
      }
    })

    it('customDocs special expressions have customVariants', () => {
      for (const [name, index] of Object.entries(specialExpressionTypes)) {
        const expr = specialExpressions[index]
        if (!expr?.docs || isFunctionDocs(expr.docs))
          continue
        const docs = expr.docs
        expect(Array.isArray(docs.customVariants), `"${name}" customVariants`).toBe(true)
        expect(docs.customVariants.length, `"${name}" has no customVariants`).toBeGreaterThan(0)
      }
    })
  })

  for (const { namespace, category } of namespaceEntries) {
    describe(`namespace: ${namespace.name}`, () => {
      it('every function has a docs field', () => {
        for (const [key, expr] of Object.entries(namespace.functions)) {
          expect(expr.docs, `"${key}" is missing docs`).toBeDefined()
        }
      })
      it(`every function has category "${category}"`, () => {
        for (const [key, expr] of Object.entries(namespace.functions)) {
          expect(expr.docs!.category, `"${key}" has wrong category`).toBe(category)
        }
      })
      it('every docs has description, returns, args, variants, and examples', () => {
        for (const [key, expr] of Object.entries(namespace.functions)) {
          const docs: FunctionDocs = expr.docs!
          expect(typeof docs.description, `"${key}" description`).toBe('string')
          expect(docs.description.length, `"${key}" description is empty`).toBeGreaterThan(0)
          expect(docs.returns, `"${key}" returns`).toBeDefined()
          expect(docs.args, `"${key}" args`).toBeDefined()
          expect(Array.isArray(docs.variants), `"${key}" variants`).toBe(true)
          expect(docs.variants.length, `"${key}" has no variants`).toBeGreaterThan(0)
          expect(Array.isArray(docs.examples), `"${key}" examples`).toBe(true)
          expect(docs.examples.length, `"${key}" has no examples`).toBeGreaterThan(0)
        }
      })
    })
  }
})

// ============================================================================
// 2. Core category names and namespace names are disjoint
// ============================================================================

describe('core and namespace category names are disjoint', () => {
  it('no category name appears in both core and namespace', () => {
    const overlap = Array.from(coreCategoryNames).filter(c => namespaceCategoryNames.has(c))
    expect(overlap, `Overlapping categories: ${overlap.join(', ')}`).toEqual([])
  })
})

// ============================================================================
// 3. seeAlso references resolve to valid function identifiers
// ============================================================================

describe('seeAlso references resolve to valid identifiers', () => {
  it('all seeAlso entries in allReference are valid ApiNames', () => {
    const invalidRefs: string[] = []
    for (const [key, ref] of Object.entries(allReference)) {
      if ('seeAlso' in ref && ref.seeAlso) {
        for (const sa of ref.seeAlso) {
          if (!isApiName(sa)) {
            invalidRefs.push(`${key} -> ${sa}`)
          }
        }
      }
    }
    expect(invalidRefs, `Invalid seeAlso refs: ${invalidRefs.join(', ')}`).toEqual([])
  })

  it('all seeAlso entries point to entries that exist in allReference', () => {
    const missing: string[] = []
    for (const [key, ref] of Object.entries(allReference)) {
      if ('seeAlso' in ref && ref.seeAlso) {
        for (const sa of ref.seeAlso) {
          if (!(sa in allReference)) {
            missing.push(`${key} -> ${sa}`)
          }
        }
      }
    }
    expect(missing, `Missing seeAlso targets: ${missing.join(', ')}`).toEqual([])
  })
})

// ============================================================================
// 4. No orphaned reference data
// ============================================================================

describe('no orphaned reference data', () => {
  it('every normalExpressionReference key exists in normalExpressionKeys', () => {
    const refKeys = Object.keys(normalExpressionReference)
    const exprKeys = new Set(normalExpressionKeys)
    const orphaned = refKeys.filter(k => !exprKeys.has(k))
    expect(orphaned, `Orphaned normal expression refs: ${orphaned.join(', ')}`).toEqual([])
  })

  it('every normalExpressionKey has a reference entry', () => {
    const refKeys = new Set(Object.keys(normalExpressionReference))
    const missing = normalExpressionKeys.filter(k => !refKeys.has(k))
    expect(missing, `Normal expressions missing reference: ${missing.join(', ')}`).toEqual([])
  })

  it('every documented special expression has a reference entry', () => {
    const docNames: string[] = []
    for (const [name, index] of Object.entries(specialExpressionTypes)) {
      if (specialExpressions[index]?.docs)
        docNames.push(name)
    }
    const missing = docNames.filter(n => !(n in allReference))
    expect(missing, `Special expressions missing from allReference: ${missing.join(', ')}`).toEqual([])
  })

  it('every namespace function has a reference entry in allReference', () => {
    const missing: string[] = []
    for (const { namespace } of namespaceEntries) {
      for (const key of Object.keys(namespace.functions)) {
        const qualifiedKey = `${namespace.name}.${key}`
        if (!(qualifiedKey in allReference)) {
          missing.push(qualifiedKey)
        }
      }
    }
    expect(missing, `Namespace functions missing from allReference: ${missing.join(', ')}`).toEqual([])
  })
})

// ============================================================================
// 5. allReference type consistency
// ============================================================================

describe('allReference type consistency', () => {
  it('every FunctionReference has returns, args, and variants', () => {
    for (const [key, ref] of Object.entries(allReference)) {
      if (isFunctionReference(ref)) {
        expect(ref.returns, `"${key}" returns`).toBeDefined()
        expect(ref.args, `"${key}" args`).toBeDefined()
        expect(Array.isArray(ref.variants), `"${key}" variants`).toBe(true)
      }
    }
  })

  it('every CustomReference has customVariants', () => {
    for (const [key, ref] of Object.entries(allReference)) {
      if (isCustomReference(ref)) {
        expect(Array.isArray(ref.customVariants), `"${key}" customVariants`).toBe(true)
      }
    }
  })

  it('every ShorthandReference has shorthand: true', () => {
    for (const [key, ref] of Object.entries(allReference)) {
      if (isShorthandReference(ref)) {
        expect(ref.shorthand, `"${key}" shorthand`).toBe(true)
      }
    }
  })

  it('every DatatypeReference has datatype: true', () => {
    for (const [key, ref] of Object.entries(allReference)) {
      if (isDatatypeReference(ref)) {
        expect(ref.datatype, `"${key}" datatype`).toBe(true)
      }
    }
  })

  it('every reference has title, category, description, and examples', () => {
    for (const [key, ref] of Object.entries(allReference)) {
      expect(typeof ref.title, `"${key}" title`).toBe('string')
      expect(ref.title.length, `"${key}" title is empty`).toBeGreaterThan(0)
      expect(typeof ref.category, `"${key}" category`).toBe('string')
      expect(typeof ref.description, `"${key}" description`).toBe('string')
      expect(Array.isArray(ref.examples), `"${key}" examples`).toBe(true)
    }
  })
})

// ============================================================================
// 6. Reference output stability (snapshot)
// ============================================================================

describe('reference output stability', () => {
  it('allReference snapshot is stable', () => {
    expect(JSON.stringify(allReference, null, 2)).toMatchSnapshot()
  })
})
