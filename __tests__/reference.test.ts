/* eslint-disable ts/no-unsafe-return */
/* eslint-disable ts/no-unsafe-member-access */

import { describe, expect, it, test } from 'vitest'
import { apiReference, isFunctionReference, normalExpressionReference } from '../reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'
import { canBeOperator, isUnknownRecord } from '../src/typeGuards'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { isReservedSymbol } from '../src/tokenizer/reservedNames'
import { Lits } from '../src/Lits/Lits'

function getLinkName(name: string): string {
  name = name.replace(/≠/g, '-ne')
  name = name.replace(/≤/g, '-lte')
  name = name.replace(/</g, '-lt')
  name = name.replace(/≥/g, '-gte')
  name = name.replace(/>/g, '-gt')
  name = name.replace(/=/g, '-equal')
  name = name.replace(/\+/g, '-plus')
  name = name.replace(/-$/g, '-minus')
  name = name.replace(/\*/g, '-star')
  name = name.replace(/%/g, '-percent')
  name = name.replace(/\//g, '-slash')
  name = name.replace(/\?/g, '-question')
  name = name.replace(/!/g, '-exclamation')
  name = name.replace(/&/g, '-and')
  name = name.replace(/\|/g, '-or')
  name = name.replace(/~/g, '-tilde')
  name = name.replace(/\^/g, '-caret')
  name = name.replace(/√/g, 'sqrt')
  name = name.replace(/∛/g, 'cbrt')
  return name
}

const lits = new Lits()
describe('apiReference', () => {
  const referenceAliases = Object.values(apiReference)
    .filter(obj => isFunctionReference(obj))
    .flatMap(obj => obj.aliases ?? [])

  Object.entries(apiReference).forEach(([key, obj]) => {
    if (!isFunctionReference(obj))
      return
    if (referenceAliases.includes(key))
      return
    it(key, () => {
      expect(obj.title).toBe(key)
      expect(obj.linkName).toEqual(getLinkName(key))
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
    const linkNames = Object.values(apiReference).map((obj: any) => obj.linkName)
    const linkNameSet = new Set(linkNames)
    linkNameSet.forEach(linkName => linkNames.splice(linkNames.indexOf(linkName), 1))
    expect(linkNames).toEqual([])
  })

  it('everything documented', () => {
    const functionReferenceKeys = Object.entries(apiReference)
      .filter(([, obj]) => isFunctionReference(obj))
      .map(([key]) => key)

    const duplicate = referenceAliases.find(name => functionReferenceKeys.includes(name))
    expect(duplicate, `Both alias and reference key: ${duplicate}`).toBeUndefined()

    const duplicateAliases = referenceAliases.filter((item, index) => referenceAliases.indexOf(item) !== index)
    expect(duplicateAliases.length, `Duplicate aliases found: ${duplicateAliases}`).toBe(0)

    const allReferenceKeys = [...functionReferenceKeys, ...referenceAliases]

    const builtinKeys = [...specialExpressionKeys, ...normalExpressionKeys]
    const missingReference = allReferenceKeys.find(key => !builtinKeys.includes(key))
    expect(missingReference, `Missing reference: ${missingReference}`).toBeUndefined()

    const missingImplementation = builtinKeys.find(key => !allReferenceKeys.includes(key))
    expect(missingImplementation, `Missing application: ${missingImplementation}`).toBeUndefined()
  })

  describe('argument names', () => {
    const allBuiltins = [...normalExpressionKeys
      .flatMap(key => ([
        key,
        ...normalExpressions[key]!.aliases ?? [],
      ])), ...specialExpressionKeys]
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
          const paramCount = normalExpressions[key]!.paramCount
          if (canBeOperator(paramCount) && !obj.noOperatorDocumentation) {
            expect(obj.args.a, `${obj.category} - ${key} is missing "a" arg`).toBeDefined()
            expect(obj.args.b, `${obj.category} - ${key} is missing "b" arg`).toBeDefined()
          }
        })
      })
  })
})
