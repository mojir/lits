import { describe, expect, it, test } from 'vitest'
import { allReference, apiReference, getLinkName, isCustomReference, isDatatypeReference, isFunctionReference, isShorthandReference, moduleReference, normalExpressionReference } from '../reference'
import { normalExpressionKeys, specialExpressionKeys, specialExpressions } from '../src/builtin'
import { isUnknownRecord } from '../src/typeGuards'
import { canBeOperator } from '../src/utils/arity'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { isReservedSymbol } from '../src/tokenizer/reservedNames'
import { Lits } from '../src/Lits/Lits'
import { allBuiltinModules } from '../src/allModules'
import { specialExpressionTypes } from '../src/builtin/specialExpressionTypes'
import { type ApiName, isApiName } from '../reference/api'
import '../src/initReferenceData'

const lits = new Lits({ modules: allBuiltinModules })
describe('apiReference', () => {
  Object.entries(apiReference).forEach(([key, obj]) => {
    if (!isFunctionReference(obj))
      return
    it(key, () => {
      expect(obj.title).toBe(key)
      expect(obj.description.length).toBeGreaterThanOrEqual(1)
      expect(obj.returns.type.length).toBeGreaterThanOrEqual(1)
      expect(obj.description[obj.description.length - 1]).toBe('.')

      expect(obj.examples.length).toBeGreaterThan(0)
      expect(isUnknownRecord(obj.args)).toBe(true)
      if (normalExpressionKeys.includes(key))
        expect(obj.category).not.toBe('Special expression')
      else if (specialExpressionKeys.includes(key))
        expect(obj.category).toBe('Special expression')
      else
        throw new Error(`${key} is not a builtin function`)
    })
  })

  it('unique linkNames', () => {
    const linkNames = Object.values(apiReference).map(obj => getLinkName(obj))
    const linkNameSet = new Set(linkNames)
    linkNameSet.forEach(linkName => linkNames.splice(linkNames.indexOf(linkName), 1))
    expect(linkNames).toEqual([])
  })

  it('everything documented', () => {
    const functionReferenceKeys = Object.entries(apiReference)
      .filter(([, obj]) => isFunctionReference(obj))
      .map(([key]) => key)

    const allReferenceKeys = functionReferenceKeys.filter(key => !specialExpressionKeys.includes(key))

    const builtinKeys = [...normalExpressionKeys]
    const missingReference = allReferenceKeys.find(key => !builtinKeys.includes(key))
    expect(missingReference, `Missing reference: ${missingReference}`).toBeUndefined()

    const missingImplementation = builtinKeys.find(key => !allReferenceKeys.includes(key))
    expect(missingImplementation, `Missing application: ${missingImplementation}`).toBeUndefined()
  })

  describe('argument names', () => {
    const allBuiltins = [...normalExpressionKeys, ...specialExpressionKeys]
    Object.entries(apiReference).forEach(([key, obj]) => {
      if (!isFunctionReference(obj))
        return
      test(key, () => {
        const variants = obj.variants
        variants.forEach((variant) => {
          const argumentNames = variant.argumentNames
          argumentNames.forEach((argName) => {
            expect(isReservedSymbol(argName) || allBuiltins.includes(argName), `${key} in ${obj.category} has invalid argument name ${argName}`).toBe(false)
          })
        })
      })
    })
  })

  describe('examples', () => {
    Object.entries(apiReference).forEach(([key, obj]) => {
      test(key, () => {
        obj.examples.forEach((example, index) => {
          expect(example, `${obj.category}:${key}. Example number ${index + 1} ended with ;`).not.toMatch(/;\s*$/)
          expect(() => lits.run(example), `${obj.category}:${key}. Example number ${index + 1}`).not.toThrow()
        })
      })
    })
  })

  describe('operator functions', () => {
    Object.entries(normalExpressionReference)
      .forEach(([key, obj]) => {
        test(key, () => {
          const arity = normalExpressions[key]!.arity
          if (canBeOperator(arity) && !obj.noOperatorDocumentation) {
            expect(obj.args.a, `${obj.category} - ${key} is missing "a" arg`).toBeDefined()
            expect(obj.args.b, `${obj.category} - ${key} is missing "b" arg`).toBeDefined()
          }
        })
      })
  })
})

describe('seeAlso', () => {
  it('if A references B in seeAlso, then B must reference A', () => {
    const asymmetric: string[] = []
    for (const [key, ref] of Object.entries(allReference)) {
      if (!('seeAlso' in ref) || !ref.seeAlso) {
        continue
      }
      for (const target of ref.seeAlso) {
        const targetRef = allReference[target]
        if (!targetRef) {
          continue // missing target is caught by other tests
        }
        if (!('seeAlso' in targetRef) || !targetRef.seeAlso || !targetRef.seeAlso.includes(key as ApiName)) {
          asymmetric.push(`${key} -> ${target} (but ${target} does not link back)`)
        }
      }
    }
    expect(asymmetric, `Asymmetric seeAlso:\n${asymmetric.join('\n')}`).toEqual([])
  })

  it('all seeAlso entries are valid ApiNames', () => {
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

describe('moduleReference', () => {
  describe('examples', () => {
    Object.entries(moduleReference).forEach(([key, obj]) => {
      test(key, () => {
        obj.examples.forEach((example, index) => {
          expect(example, `${obj.category}:${key}. Example number ${index + 1} ended with ;`).not.toMatch(/;\s*$/)
          expect(() => lits.run(example), `${obj.category}:${key}. Example number ${index + 1}`).not.toThrow()
        })
      })
    })
  })
})

describe('no orphaned reference data', () => {
  it('every documented special expression has a reference entry', () => {
    const docNames: string[] = []
    for (const [name, index] of Object.entries(specialExpressionTypes)) {
      if (specialExpressions[index]?.docs)
        docNames.push(name)
    }
    const missing = docNames.filter(n => !(n in allReference))
    expect(missing, `Special expressions missing from allReference: ${missing.join(', ')}`).toEqual([])
  })

  it('every module function has a reference entry in allReference', () => {
    const missing: string[] = []
    for (const module of allBuiltinModules) {
      for (const key of Object.keys(module.functions)) {
        const qualifiedKey = `${module.name}.${key}`
        if (!(qualifiedKey in allReference)) {
          missing.push(qualifiedKey)
        }
      }
    }
    expect(missing, `Module functions missing from allReference: ${missing.join(', ')}`).toEqual([])
  })
})

describe('core and module category names are disjoint', () => {
  it('no category name appears in both api and module references', () => {
    const apiCategories = new Set(Object.values(apiReference).map(r => r.category))
    const nsCategories = new Set(Object.values(moduleReference).map(r => r.category))
    const overlap = Array.from(apiCategories).filter(c => nsCategories.has(c))
    expect(overlap, `Overlapping categories: ${overlap.join(', ')}`).toEqual([])
  })
})

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
