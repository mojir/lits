import { Lits } from '../src'
import { Type } from '../src/types/Type'
import { builtinTypesBitMasks, orderedTypeNames, typeToBitRecord } from '../src/types/typeUtils'
import { MAX_NUMBER, MIN_NUMBER } from '../src/utils'

const lits = new Lits()

describe(`Type`, () => {
  test(`orderedTypeNames`, () => {
    const set = new Set(orderedTypeNames)
    expect(set.size + 1).toBe(Object.keys(builtinTypesBitMasks).length)
    expect(orderedTypeNames.includes(`never`)).toBe(false)
  })
  test(`isType`, () => {
    const isType = Type.isType
    expect(isType({})).toBe(false)
    expect(isType(Type.string)).toBe(true)
  })
  test(`assertType`, () => {
    const assertType = Type.assertType
    expect(() => assertType({}, undefined)).toThrow()
    expect(() => assertType(Type.string, undefined)).not.toThrow()
  })
  test(`assertType`, () => {
    const asType = Type.asType
    expect(() => asType({}, undefined)).toThrow()
    expect(() => asType(Type.string, undefined)).not.toThrow()
  })
  test(`isNotType`, () => {
    const isNotType = Type.isNotType
    expect(isNotType({})).toBe(true)
    expect(isNotType(Type.string)).toBe(false)
  })
  test(`Type.of.`, () => {
    expect(Type.of(null)).toEqual(Type.nil)
    expect(Type.of(``)).toEqual(Type.emptyString)
    expect(Type.of(`Albert`)).toEqual(Type.nonEmptyString)
    expect(Type.of(true)).toEqual(Type.true)
    expect(Type.of(false)).toEqual(Type.false)
    expect(Type.of(MAX_NUMBER + 1)).toEqual(Type.positiveInfinity)
    expect(Type.of(MIN_NUMBER - 1)).toEqual(Type.negativeInfinity)
    expect(Type.of(NaN)).toEqual(Type.nan)
    expect(Type.of(0)).toEqual(Type.positiveZero)
    expect(Type.of(-0)).toEqual(Type.negativeZero)
    expect(Type.of(1)).toEqual(Type.positiveInteger)
    expect(Type.of(-1)).toEqual(Type.negativeInteger)
    expect(Type.of(1.1)).toEqual(Type.positiveFloat)
    expect(Type.of(-1.1)).toEqual(Type.negativeFloat)
    expect(Type.of(Infinity)).toEqual(Type.positiveInfinity)
    expect(Type.of(-Infinity)).toEqual(Type.negativeInfinity)
    expect(Type.of([])).toEqual(Type.emptyArray)
    expect(Type.of([1])).toEqual(Type.createNonEmpyTypedArray(Type.positiveInteger))
    expect(Type.of({})).toEqual(Type.emptyObject)
    expect(Type.of({ foo: true })).toEqual(Type.nonEmptyObject)
    expect(Type.of(lits.run(`#"^$"`))).toEqual(Type.regexp)
    expect(Type.of(lits.run(`#(1)`))).toEqual(Type.function)
    expect(() => Type.of(() => undefined)).toThrow()
  })

  test(`Type.toValue.`, () => {
    expect(Type.toValue(Type.nil)).toBe(null)
    expect(Type.toValue(Type.positiveZero)).toBe(0)
    expect(Type.toValue(Type.negativeZero)).toBe(-0)
    expect(Type.toValue(Type.emptyString)).toBe(``)
    expect(Type.toValue(Type.true)).toBe(true)
    expect(Type.toValue(Type.false)).toBe(false)
    expect(Type.toValue(Type.nan)).toBe(NaN)
    expect(Type.toValue(Type.positiveInfinity)).toBe(Infinity)
    expect(Type.toValue(Type.negativeInfinity)).toBe(-Infinity)
    expect(Type.toValue(Type.emptyArray)).toEqual([])
    expect(Type.toValue(Type.emptyObject)).toEqual({})

    expect(Type.nil.toValue()).toBe(null)
    expect(Type.positiveZero.toValue()).toBe(0)
    expect(Type.negativeZero.toValue()).toBe(-0)
    expect(Type.emptyString.toValue()).toBe(``)
    expect(Type.true.toValue()).toBe(true)
    expect(Type.false.toValue()).toBe(false)
    expect(Type.nan.toValue()).toBe(NaN)
    expect(Type.positiveInfinity.toValue()).toBe(Infinity)
    expect(Type.negativeInfinity.toValue()).toBe(-Infinity)
    expect(Type.emptyObject.toValue()).toEqual({})
  })

  test(`Type.toNumberOrNan.`, () => {
    expect(Type.toNumberOrNan(Type.nil)).toEqual(Type.nil)
    expect(Type.toNumberOrNan(Type.positiveZero)).toBe(0)
    expect(Type.toNumberOrNan(Type.negativeZero)).toBe(-0)
    expect(Type.toNumberOrNan(Type.emptyString)).toEqual(Type.emptyString)
    expect(Type.toNumberOrNan(Type.true)).toEqual(Type.true)
    expect(Type.toNumberOrNan(Type.false)).toEqual(Type.false)
    expect(Type.toNumberOrNan(Type.nan)).toEqual(NaN)
    expect(Type.toNumberOrNan(Type.positiveInfinity)).toBe(Infinity)
    expect(Type.toNumberOrNan(Type.negativeInfinity)).toBe(-Infinity)
    expect(Type.toNumberOrNan(Type.emptyObject)).toEqual(Type.emptyObject)
  })

  test(`Type.split.`, () => {
    expect(Type.split(Type.string)).toEqual([Type.emptyString, Type.nonEmptyString])
    expect(Type.split(Type.emptyString)).toEqual([Type.emptyString])
    expect(Type.string.split()).toEqual([Type.emptyString, Type.nonEmptyString])
    expect(Type.emptyString.split()).toEqual([Type.emptyString])
  })

  test(`assertIs.`, () => {
    expect(() => Type.emptyString.assertIs(Type.string, undefined)).not.toThrow()
    expect(() => Type.string.assertIs(Type.emptyString, undefined)).toThrow()
  })

  test(`assertEquals.`, () => {
    expect(() => Type.emptyString.assertEquals(Type.emptyString, undefined)).not.toThrow()
    expect(() => Type.string.assertEquals(Type.emptyString, undefined)).toThrow()
  })

  test(`assertIntersects.`, () => {
    expect(() => Type.emptyString.assertIntersects(Type.string, undefined)).not.toThrow()
    expect(() => Type.string.assertIntersects(Type.emptyString, undefined)).not.toThrow()
    expect(() => Type.number.assertIntersects(Type.emptyString, undefined)).toThrow()
  })

  test(`Type.split.`, () => {
    expect(Type.split(Type.string)).toEqual([Type.emptyString, Type.nonEmptyString])
    expect(Type.split(Type.emptyString)).toEqual([Type.emptyString])
    expect(Type.string.split()).toEqual([Type.emptyString, Type.nonEmptyString])
    expect(Type.emptyString.split()).toEqual([Type.emptyString])
  })

  test(`standard types.`, () => {
    expect(Type.nil.bitmask).toBe(typeToBitRecord.nil)

    expect(Type.true.bitmask).toBe(typeToBitRecord.true)
    expect(Type.false.bitmask).toBe(typeToBitRecord.false)
    expect(Type.boolean.bitmask).toBe(typeToBitRecord.true | typeToBitRecord.false)
    expect(Type.emptyString.bitmask).toBe(typeToBitRecord[`empty-string`])
    expect(Type.nonEmptyString.bitmask).toBe(typeToBitRecord[`non-empty-string`])
    expect(Type.string.bitmask).toBe(typeToBitRecord[`empty-string`] | typeToBitRecord[`non-empty-string`])
    expect(Type.zero.bitmask).toBe(typeToBitRecord[`positive-zero`] | typeToBitRecord[`negative-zero`])
    expect(Type.positiveFloat.bitmask).toBe(
      typeToBitRecord[`positive-integer`] | typeToBitRecord[`positive-non-integer`],
    )
    expect(Type.negativeFloat.bitmask).toBe(
      typeToBitRecord[`negative-integer`] | typeToBitRecord[`negative-non-integer`],
    )
    expect(Type.float.bitmask).toBe(
      typeToBitRecord[`positive-zero`] |
        typeToBitRecord[`negative-zero`] |
        typeToBitRecord[`positive-integer`] |
        typeToBitRecord[`positive-non-integer`] |
        typeToBitRecord[`negative-integer`] |
        typeToBitRecord[`negative-non-integer`],
    )
    expect(Type.integer.bitmask).toBe(
      typeToBitRecord[`positive-zero`] |
        typeToBitRecord[`negative-zero`] |
        typeToBitRecord[`positive-integer`] |
        typeToBitRecord[`negative-integer`],
    )
    expect(Type.nonEmptyObject.bitmask).toBe(typeToBitRecord[`non-empty-object`])
    expect(Type.emptyObject.bitmask).toBe(typeToBitRecord[`empty-object`])
    expect(Type.object.bitmask).toBe(typeToBitRecord[`non-empty-object`] | typeToBitRecord[`empty-object`])
    expect(Type.regexp.bitmask).toBe(typeToBitRecord.regexp)

    expect(Type.unknown.bitmask).toBe(Object.values(typeToBitRecord).reduce((result, value) => result | value, 0))

    expect(Type.falsy.bitmask).toBe(
      typeToBitRecord.nan |
        typeToBitRecord.nil |
        typeToBitRecord[`empty-string`] |
        typeToBitRecord[`positive-zero`] |
        typeToBitRecord[`negative-zero`] |
        typeToBitRecord.false,
    )
    expect(Type.truthy.bitmask).toBe(
      typeToBitRecord[`non-empty-string`] |
        typeToBitRecord[`positive-integer`] |
        typeToBitRecord[`positive-non-integer`] |
        typeToBitRecord[`negative-integer`] |
        typeToBitRecord[`negative-non-integer`] |
        typeToBitRecord.true |
        typeToBitRecord.array |
        typeToBitRecord[`non-empty-object`] |
        typeToBitRecord[`empty-object`] |
        typeToBitRecord.function |
        typeToBitRecord.regexp |
        typeToBitRecord[`positive-infinity`] |
        typeToBitRecord[`negative-infinity`],
    )
  })
  describe(`Type.and`, () => {
    test(`samples`, () => {
      expect(Type.truthy.and(Type.string)).toEqual(Type.nonEmptyString)
      expect(Type.truthy.and(Type.string.or(Type.float))).toEqual(
        Type.nonEmptyString.or(Type.positiveFloat).or(Type.negativeFloat),
      )
    })
    test(`array.`, () => {
      const program = `
        (def a  (type-or (::non-empty-array ::string) (::array ::number)))
        (def b  (type-or (::array ::string)))

        (type-and a b)`

      expect(lits.run(program)).toEqual(Type.createNonEmpyTypedArray(Type.string))
    })
  })

  describe(`Type.exclude`, () => {
    test(`samples`, () => {
      expect(Type.truthy.exclude(Type.string)).toEqual(
        Type.or(
          Type.positiveFloat,
          Type.negativeFloat,
          Type.true,
          Type.array,
          Type.object,
          Type.regexp,
          Type.function,
          Type.positiveInfinity,
          Type.negativeInfinity,
        ),
      )

      expect(Type.truthy.exclude(Type.string.or(Type.float))).toEqual(
        Type.or(
          Type.true,
          Type.array,
          Type.object,
          Type.function,
          Type.regexp,
          Type.positiveInfinity,
          Type.negativeInfinity,
        ),
      )

      expect(Type.unknown.exclude(Type.truthy)).toEqual(Type.falsy)
      expect(Type.unknown.exclude(Type.falsy)).toEqual(Type.truthy)

      expect(Type.createTypedArray(Type.integer).exclude(Type.emptyArray)).toEqual(
        Type.createNonEmpyTypedArray(Type.integer),
      )

      expect(Type.emptyArray.exclude(Type.createTypedArray(Type.string))).toEqual(Type.never)
      expect(Type.createTypedArray(Type.string).exclude(Type.emptyArray)).toEqual(
        Type.createNonEmpyTypedArray(Type.string),
      )
    })
  })

  describe(`Type.intersects`, () => {
    test(`samples`, () => {
      expect(Type.nonZeroInteger.intersects(Type.negativeFloat)).toBe(true)
    })
  })

  describe(`negateNumber`, () => {
    test(`samples`, () => {
      expect(Type.zero.negateNumber()).toEqual(Type.zero)
      expect(Type.float.negateNumber()).toEqual(Type.float)
      expect(Type.integer.negateNumber()).toEqual(Type.integer)
      expect(Type.nonZeroFloat.negateNumber()).toEqual(Type.nonZeroFloat)
      expect(Type.nonZeroInteger.negateNumber()).toEqual(Type.nonZeroInteger)
      expect(Type.positiveFloat.negateNumber().toString()).toEqual(Type.negativeFloat.toString())
      expect(Type.nonPositiveFloat.negateNumber()).toEqual(Type.nonNegativeFloat)
      expect(Type.positiveInteger.negateNumber()).toEqual(Type.negativeInteger)
      expect(Type.nonPositiveInteger.negateNumber()).toEqual(Type.nonNegativeInteger)
      expect(Type.negativeFloat.negateNumber()).toEqual(Type.positiveFloat)
      expect(Type.nonNegativeFloat.negateNumber()).toEqual(Type.nonPositiveFloat)
      expect(Type.negativeInteger.negateNumber()).toEqual(Type.positiveInteger)
      expect(Type.nonNegativeInteger.negateNumber()).toEqual(Type.nonPositiveInteger)
    })
  })

  describe(`Type.or`, () => {
    test(`samples`, () => {
      expect(Type.or(Type.truthy, Type.falsy)).toEqual(Type.unknown)
    })
    test(`create nilable types`, () => {
      expect(Type.or(Type.nil, Type.boolean).bitmask).toBe(
        typeToBitRecord.true | typeToBitRecord.false | typeToBitRecord.nil,
      )
      expect(Type.or(Type.nil, Type.string).bitmask).toBe(
        typeToBitRecord[`empty-string`] | typeToBitRecord[`non-empty-string`] | typeToBitRecord.nil,
      )
      expect(Type.or(Type.nil, Type.float).bitmask).toBe(
        typeToBitRecord[`positive-zero`] |
          typeToBitRecord[`negative-zero`] |
          typeToBitRecord[`positive-integer`] |
          typeToBitRecord[`positive-non-integer`] |
          typeToBitRecord[`negative-integer`] |
          typeToBitRecord[`negative-non-integer`] |
          typeToBitRecord.nil,
      )
      expect(Type.or(Type.nil, Type.array).bitmask).toBe(typeToBitRecord.array | typeToBitRecord.nil)
      expect(Type.or(Type.nil, Type.object).bitmask).toBe(
        typeToBitRecord[`non-empty-object`] | typeToBitRecord[`empty-object`] | typeToBitRecord.nil,
      )
      expect(Type.or(Type.nil, Type.function).bitmask).toBe(typeToBitRecord.function | typeToBitRecord.nil)
      expect(Type.or(Type.nil, Type.regexp).bitmask).toBe(typeToBitRecord.regexp | typeToBitRecord.nil)
    })
    test(`the disjunction over all types should be unknown`, () => {
      const unknown1 = Type.or(
        Type.nil,
        Type.boolean,
        Type.string,
        Type.float,
        Type.array,
        Type.object,
        Type.regexp,
        Type.function,
        Type.infinity,
        Type.nan,
      )
      const unknown2 = Type.or(
        Type.nil,
        Type.true,
        Type.false,
        Type.emptyString,
        Type.nonEmptyString,
        Type.zero,
        Type.positiveFloat,
        Type.negativeFloat,
        Type.array,
        Type.object,
        Type.regexp,
        Type.function,
        Type.nan,
        Type.positiveInfinity,
        Type.negativeInfinity,
      )
      expect(unknown1.isUnknown()).toBe(true)
      expect(unknown1).toEqual(Type.unknown)
      expect(unknown2.isUnknown()).toBe(true)
      expect(unknown2).toEqual(Type.unknown)
    })
  })
  describe(`primitives`, () => {
    test(`isFalsy`, () => {
      expect(Type.nil.is(Type.falsy)).toBe(true)

      expect(Type.true.is(Type.falsy)).toBe(false)
      expect(Type.false.is(Type.falsy)).toBe(true)
      expect(Type.boolean.is(Type.falsy)).toBe(false)
      expect(Type.emptyString.is(Type.falsy)).toBe(true)
      expect(Type.nonEmptyString.is(Type.falsy)).toBe(false)
      expect(Type.string.is(Type.falsy)).toBe(false)
      expect(Type.zero.is(Type.falsy)).toBe(true)
      expect(Type.positiveFloat.is(Type.falsy)).toBe(false)
      expect(Type.negativeFloat.is(Type.falsy)).toBe(false)
      expect(Type.float.is(Type.falsy)).toBe(false)
      expect(Type.array.is(Type.falsy)).toBe(false)
      expect(Type.object.is(Type.falsy)).toBe(false)
      expect(Type.regexp.is(Type.falsy)).toBe(false)
      expect(Type.function.is(Type.falsy)).toBe(false)
      expect(Type.unknown.is(Type.falsy)).toBe(false)
      expect(Type.or(Type.nil, Type.false, Type.emptyString, Type.zero).is(Type.falsy)).toBe(true)
      expect(Type.or(Type.array, Type.nil, Type.false, Type.emptyString, Type.zero).is(Type.falsy)).toBe(false)
    })
    test(`isTruthy`, () => {
      expect(Type.nil.is(Type.truthy)).toBe(false)
      expect(Type.true.is(Type.truthy)).toBe(true)
      expect(Type.false.is(Type.truthy)).toBe(false)
      expect(Type.boolean.is(Type.truthy)).toBe(false)
      expect(Type.emptyString.is(Type.truthy)).toBe(false)
      expect(Type.nonEmptyString.is(Type.truthy)).toBe(true)
      expect(Type.string.is(Type.truthy)).toBe(false)
      expect(Type.zero.is(Type.truthy)).toBe(false)
      expect(Type.positiveFloat.is(Type.truthy)).toBe(true)
      expect(Type.negativeFloat.is(Type.truthy)).toBe(true)
      expect(Type.float.is(Type.truthy)).toBe(false)
      expect(Type.array.is(Type.truthy)).toBe(true)
      expect(Type.object.is(Type.truthy)).toBe(true)
      expect(Type.regexp.is(Type.truthy)).toBe(true)
      expect(Type.function.is(Type.truthy)).toBe(true)
      expect(Type.unknown.is(Type.truthy)).toBe(false)
      expect(
        Type.or(
          Type.true,
          Type.nonEmptyString,
          Type.positiveFloat,
          Type.negativeFloat,
          Type.function,
          Type.regexp,
          Type.function,
          Type.array,
          Type.object,
        ).is(Type.truthy),
      ).toBe(true)
      expect(
        Type.or(
          Type.string,
          Type.true,
          Type.nonEmptyString,
          Type.positiveFloat,
          Type.negativeFloat,
          Type.function,
          Type.regexp,
          Type.function,
          Type.array,
          Type.object,
        ).is(Type.truthy),
      ).toBe(false)
    })
  })
  describe(`Type.is`, () => {
    test(`Everything is unknown`, () => {
      expect(Type.nil.is(Type.unknown)).toBe(true)

      expect(Type.true.is(Type.unknown)).toBe(true)
      expect(Type.false.is(Type.unknown)).toBe(true)
      expect(Type.boolean.is(Type.unknown)).toBe(true)
      expect(Type.emptyString.is(Type.unknown)).toBe(true)
      expect(Type.nonEmptyString.is(Type.unknown)).toBe(true)
      expect(Type.string.is(Type.unknown)).toBe(true)
      expect(Type.zero.is(Type.unknown)).toBe(true)
      expect(Type.positiveFloat.is(Type.unknown)).toBe(true)
      expect(Type.negativeFloat.is(Type.unknown)).toBe(true)
      expect(Type.float.is(Type.unknown)).toBe(true)
      expect(Type.array.is(Type.unknown)).toBe(true)
      expect(Type.function.is(Type.unknown)).toBe(true)
      expect(Type.object.is(Type.unknown)).toBe(true)
      expect(Type.regexp.is(Type.unknown)).toBe(true)

      expect(Type.unknown.is(Type.unknown)).toBe(true)
      expect(Type.falsy.is(Type.unknown)).toBe(true)
      expect(Type.truthy.is(Type.unknown)).toBe(true)

      expect(Type.or(Type.string, Type.float, Type.boolean).is(Type.unknown)).toBe(true)
    })
    test(`nil samples`, () => {
      expect(Type.nil.is(Type.or(Type.array, Type.true, Type.nil))).toBe(true)
    })
    test(`arrays.`, () => {
      expect(Type.is(Type.emptyArray, Type.emptyArray)).toBe(true)

      expect(
        Type.is(
          Type.createTypedArray(Type.or(Type.emptyString, Type.number)),
          Type.or(
            Type.createTypedArray(Type.or(Type.nonEmptyString, Type.number)),
            Type.createTypedArray(Type.or(Type.emptyString, Type.number)),
          ),
        ),
      ).toBe(true)
      expect(
        Type.is(
          Type.createTypedArray(Type.or(Type.emptyString, Type.number)),
          Type.or(
            Type.createTypedArray(Type.or(Type.nonEmptyString, Type.number)),
            Type.createTypedArray(Type.or(Type.emptyString, Type.integer)),
          ),
        ),
      ).toBe(false)
    })
  })

  describe(`Type.equals`, () => {
    test(`samples`, () => {
      expect(Type.truthy.or(Type.falsy).equals(Type.unknown)).toBe(true)
    })
    test(`arrays.`, () => {
      const program = `
        (def a  (type-or (::array ::string)))
        (def b  (type-or (::empty-array ::string) (::non-empty-array ::string)))

        (type-equals? a b)`

      expect(lits.run(program)).toBe(true)
    })
  })

  describe(`Type.toString`, () => {
    test(`samples`, () => {
      expect(Type.never.toString()).toBe(`::never [Bitmask = 0000 0000 0000 0000 0000  (0)]`)
      expect(Type.unknown.toString()).toBe(`::unknown [Bitmask = 1111 0111 1111 1111 1111  (1015807)]`)
      expect(Type.nil.toString()).toBe(`::nil [Bitmask = 0000 0000 0000 0000 0001  (1)]`)
      expect(Type.string.toString()).toBe(`::string [Bitmask = 0000 0011 0000 0000 0000  (12288)]`)
      expect(Type.string.nilable().toString()).toBe(`::string | ::nil [Bitmask = 0000 0011 0000 0000 0001  (12289)]`)
      expect(Type.never.toString({ showDetails: false })).toBe(`::never`)
      expect(Type.unknown.toString({ showDetails: false })).toBe(`::unknown`)
      expect(Type.nil.toString({ showDetails: false })).toBe(`::nil`)
      expect(Type.string.toString({ showDetails: false })).toBe(`::string`)
      expect(Type.string.nilable().toString({ showDetails: false })).toBe(`::string | ::nil`)
    })
  })

  describe(`Misc.`, () => {
    test(`::empty-array litteral`, () => {
      const emptyArray1 = lits.run(`::empty-array`)
      const emptyArray2 = Type.emptyArray

      expect(emptyArray1).toEqual(emptyArray2)
    })
    test(`maximum call stack`, () => {
      expect(lits.run(`(type-is? ::empty-array ::empty-array)`)).toBe(true)
    })
  })
})
