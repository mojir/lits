/* eslint-disable @typescript-eslint/no-explicit-any */
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import reference from '../cli/reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'

function getLinkName(name: string): string {
  name = name.replace(/<=/g, '_lte')
  name = name.replace(/</g, '_lt')
  name = name.replace(/>=/g, '_gte')
  name = name.replace(/>/g, '_gt')
  name = name.replace(/!=/g, '_notequal')
  name = name.replace(/=/g, '_equal')
  name = name.replace(/%/g, '_percent')
  name = name.replace(/\+/g, '_plus')
  name = name.replace(/-$/g, '_minus')
  name = name.replace(/\*/g, '_star')
  name = name.replace(/\//g, '_slash')
  name = name.replace(/\?/g, '_question')
  return name
}

describe('reference', () => {
  Object.entries(reference).forEach(([key, obj]: [key: string, obj: any]) => {
    test(key, () => {
      expect(obj.name).toBe(key)
      expect(
        ['Special expression', 'Math', 'Predicate', 'String', 'List', 'Object', 'Regular expression', 'Misc'].includes(
          obj.category,
        ),
      ).toBe(true)
      expect(obj.syntax.startsWith(key)).toBe(true)
      expect(obj.linkName).toEqual(getLinkName(key))
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

  test('Everything documented', () => {
    const referenceKeys = Object.keys(reference)
    const builtinKeys = [...specialExpressionKeys, ...normalExpressionKeys]
    referenceKeys.forEach(key => builtinKeys.splice(builtinKeys.indexOf(key), 1))
    expect(builtinKeys).toEqual([])
  })
})
