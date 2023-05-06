import { Lits } from '../../../src'
import { Type } from '../../../src/Lits/Lits'

const lits = new Lits()
describe(`type functions`, () => {
  // for (const lits of [new Lits(), new Lits({ debug: true })]) {
  describe(`type-of`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-of)`)).toThrow()
      expect(() => lits.run(`(type-of 1 2)`)).toThrow()
      expect(lits.run(`(type-of 1)`)).toEqual(Type.positiveInteger)
      expect(lits.run(`(type-of 1.1)`)).toEqual(Type.positiveFloat)
      expect(lits.run(`(type-of 0)`)).toEqual(Type.positiveZero)
      expect(lits.run(`(type-of -1)`)).toEqual(Type.negativeInteger)
      expect(lits.run(`(type-of -1.1)`)).toEqual(Type.negativeFloat)
    })
  })
  describe(`type-or`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-or)`)).toThrow()
      expect(lits.run(`(type-or ::integer)`)).toEqual(Type.integer)
      expect(lits.run(`(type-or ::integer ::float)`)).toEqual(Type.float)
      expect(lits.run(`(type-or ::empty-array ::non-empty-array ::object)`)).toEqual(Type.array.or(Type.object))
    })
  })
  describe(`type-and`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-and)`)).toThrow()
      expect(lits.run(`(type-and ::integer)`)).toEqual(Type.integer)
      expect(lits.run(`(type-and ::integer ::float)`)).toEqual(Type.integer)
      expect(lits.run(`(type-and ::integer ::float ::string)`)).toEqual(Type.never)
      expect(lits.run(`(type-and ::falsy (type-or ::boolean ::float))`)).toEqual(Type.false.or(Type.zero))
    })
  })
  describe(`type-exclude`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-exclude)`)).toThrow()
      expect(lits.run(`(type-exclude ::falsy)`)).toEqual(Type.falsy)
      expect(lits.run(`(type-exclude ::falsy (type-or ::boolean ::float ::string))`)).toEqual(Type.nan.or(Type.nil))
      expect(lits.run(`(type-exclude ::falsy ::boolean ::float ::string)`)).toEqual(Type.nan.or(Type.nil))
    })
  })
  describe(`type-exclude`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-exclude)`)).toThrow()
      expect(lits.run(`(type-exclude ::falsy)`)).toEqual(Type.falsy)
      expect(lits.run(`(type-exclude ::falsy (type-or ::boolean ::float ::string))`)).toEqual(Type.nan.or(Type.nil))
      expect(lits.run(`(type-exclude ::falsy ::boolean ::float ::string)`)).toEqual(Type.nan.or(Type.nil))
    })
  })
  describe(`type-is?`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-is?)`)).toThrow()
      expect(() => lits.run(`(type-is? ::nil)`)).toThrow()
      expect(() => lits.run(`(type-is? ::nil ::unknown ::unknown)`)).toThrow()
      expect(lits.run(`(type-is? nil ::unknown)`)).toBe(true)
      expect(lits.run(`(type-is? ::nil ::unknown)`)).toBe(true)
      expect(lits.run(`(type-is? ::integer ::float)`)).toBe(true)
      expect(lits.run(`(type-is? ::float ::integer)`)).toBe(false)
    })
  })
  describe(`type-equals?`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-equals?)`)).toThrow()
      expect(() => lits.run(`(type-equals? ::nil)`)).toThrow()
      expect(() => lits.run(`(type-equals? nil ::unknown)`)).toThrow()
      expect(lits.run(`(type-equals? ::integer ::float)`)).toBe(false)
    })
  })
  describe(`type-intersects?`, () => {
    test(`samples`, () => {
      expect(() => lits.run(`(type-intersects?)`)).toThrow()
      expect(() => lits.run(`(type-intersects? ::nil)`)).toThrow()
      expect(() => lits.run(`(type-intersects? nil ::unknown)`)).toThrow()
      expect(lits.run(`(type-intersects? ::integer ::float)`)).toBe(true)
      expect(lits.run(`(type-intersects? ::float ::integer)`)).toBe(true)
      expect(lits.run(`(type-intersects? ::float ::string)`)).toBe(false)
    })
  })
})
