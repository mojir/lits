/* eslint-disable ts/no-unsafe-return */
/* eslint-disable ts/no-unsafe-member-access */

import { describe, expect, it } from 'vitest'
import { apiReference, isFunctionReference } from '../reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'
import { isUnknownRecord } from '../src/typeGuards'

function getLinkName(name: string): string {
  name = name.replace(/<=/g, '_lte')
  name = name.replace(/</g, '_lt')
  name = name.replace(/>=/g, '_gte')
  name = name.replace(/>/g, '_gt')
  name = name.replace(/=/g, '_equal')
  name = name.replace(/\+/g, '_plus')
  name = name.replace(/-$/g, '_minus')
  name = name.replace(/\*/g, '_star')
  name = name.replace(/\//g, '_slash')
  name = name.replace(/\?/g, '_question')
  name = name.replace(/!/g, '_exclamation')
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
