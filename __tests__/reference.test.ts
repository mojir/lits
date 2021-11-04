/* eslint-disable @typescript-eslint/no-explicit-any */
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import { functionReference, categories } from '../cli/reference'
import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'

function getLinkName(name: string): string {
  name = name.replace(/<=/g, `_lte`)
  name = name.replace(/</g, `_lt`)
  name = name.replace(/>=/g, `_gte`)
  name = name.replace(/>/g, `_gt`)
  name = name.replace(/=/g, `_equal`)
  name = name.replace(/\+/g, `_plus`)
  name = name.replace(/-$/g, `_minus`)
  name = name.replace(/\*/g, `_star`)
  name = name.replace(/\//g, `_slash`)
  name = name.replace(/\?/g, `_question`)
  name = name.replace(/!/g, `_exclamation`)
  return name
}

describe(`functionReference`, () => {
  Object.entries(functionReference).forEach(([key, obj]: [key: string, obj: any]) => {
    test(key, () => {
      expect(obj.name).toBe(key)
      expect(categories.includes(obj.category)).toBe(true)
      expect(obj.linkName).toEqual(getLinkName(key))
      expect(obj.description.length).toBeGreaterThanOrEqual(1)
      expect(obj.returns.type.length).toBeGreaterThanOrEqual(1)
      expect(obj.description[obj.description.length - 1]).toBe(`.`)

      expect(obj.examples.length).toBeGreaterThan(0)
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

  test(`unique linkNames`, () => {
    const linkNames = Object.values(functionReference).map((obj: any) => obj.linkName)
    const linkNameSet = new Set(linkNames)
    linkNameSet.forEach(linkName => linkNames.splice(linkNames.indexOf(linkName), 1))
    expect(linkNames).toEqual([])
  })

  test(`Everything documented`, () => {
    const referenceKeys = Object.keys(functionReference)
    const builtinKeys = [...specialExpressionKeys, ...normalExpressionKeys]
    referenceKeys.forEach(key => builtinKeys.splice(builtinKeys.indexOf(key), 1))
    expect(builtinKeys).toEqual([])
  })
})
