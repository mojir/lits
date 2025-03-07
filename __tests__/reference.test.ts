/* eslint-disable ts/no-unsafe-return */
/* eslint-disable ts/no-unsafe-member-access */

import { describe, expect, it } from 'vitest'
import { apiReference, isFunctionReference } from '../reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'
import { isUnknownRecord } from '../src/typeGuards'

function getLinkName(name: string): string {
  name = name.replace(/≠/g, '-ne2')
  name = name.replace(/<=/g, '-lte')
  name = name.replace(/≤/g, '-lte2')
  name = name.replace(/</g, '-lt')
  name = name.replace(/>=/g, '-gte')
  name = name.replace(/≥/g, '-gte2')
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
  return name
}

describe('apiReference', () => {
  Object.entries(apiReference).forEach(([key, obj]) => {
    if (!isFunctionReference(obj))
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
    const referenceKeys = Object.keys(apiReference)
    const builtinKeys = [...specialExpressionKeys, ...normalExpressionKeys]
    referenceKeys.forEach(key => builtinKeys.splice(builtinKeys.indexOf(key), 1))
    expect(builtinKeys).toEqual([])
  })
})
