import { describe, expect, it, test } from 'vitest'
import { apiReference, getLinkName, isFunctionReference, namespaceReference, normalExpressionReference } from '../reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'
import { isUnknownRecord } from '../src/typeGuards'
import { canBeOperator } from '../src/utils/arity'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { isReservedSymbol } from '../src/tokenizer/reservedNames'
import { Lits } from '../src/Lits/Lits'

const lits = new Lits()
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

describe('namespaceReference', () => {
  describe('examples', () => {
    Object.entries(namespaceReference).forEach(([key, obj]) => {
      test(key, () => {
        obj.examples.forEach((example, index) => {
          expect(example, `${obj.category}:${key}. Example number ${index + 1} ended with ;`).not.toMatch(/;\s*$/)
          expect(() => lits.run(example), `${obj.category}:${key}. Example number ${index + 1}`).not.toThrow()
        })
      })
    })
  })
})
