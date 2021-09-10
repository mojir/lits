/* eslint-disable @typescript-eslint/no-explicit-any */
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import reference from '../reference/reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'

describe('reference', () => {
  Object.entries(reference).forEach(([key, obj]: [key: string, obj: any]) => {
    test(key, () => {
      expect(obj.name).toBe(key)
      expect(obj.syntax.startsWith(key)).toBe(true)
      expect(obj.linkName.length).toBeGreaterThanOrEqual(1)
      expect(obj.shortDescription.length).toBeGreaterThanOrEqual(1)
      expect(obj.longDescription.length).toBeGreaterThanOrEqual(obj.shortDescription.length)
      expect(obj.examples.length).toBeGreaterThan(0)
      expect(Array.isArray(obj.sideEffects)).toBe(true)
      expect(Array.isArray(obj.arguments)).toBe(true)
      if (normalExpressionKeys.includes(key)) {
        expect(obj.specialExpression).toBeFalsy()
      } else if (specialExpressionKeys.includes(key)) {
        expect(obj.specialExpression).toBe(true)
      } else {
        throw Error(`${key} is not a builtin function`)
      }
    })
  })

  test('unique linkNames', () => {
    const linkNames = Object.values(reference).map((obj: any) => obj.linkName)
    const linkNameSet = new Set(linkNames)
    linkNameSet.forEach(linkName => linkNames.splice(linkNames.indexOf(linkName), 1))
    expect(linkNames).toEqual([])
  })

  xtest('Everything documented', () => {
    const referenceKeys = Object.keys(reference)
    const builtinKeys = [...specialExpressionKeys, ...normalExpressionKeys]
    referenceKeys.forEach(key => builtinKeys.splice(builtinKeys.indexOf(key), 1))
    expect(builtinKeys).toEqual([])
  })
})
