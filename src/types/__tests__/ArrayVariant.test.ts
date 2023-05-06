import { ArrayVariant, simplifyArrayVariants } from '../ArrayVariant'
import { Type } from '../Type'

ArrayVariant.unknownType = Type.unknown

describe(`ArrayVariant`, () => {
  test(`ArrayVariant.createEmpty`, () => {
    const v1 = ArrayVariant.createEmpty()
    expect(v1.size).toBe(ArrayVariant.Size.Empty)
    expect(v1.type).toBeNull()
  })

  test(`ArrayVariant.create`, () => {
    const v1 = ArrayVariant.create(null)
    expect(v1.size).toBe(ArrayVariant.Size.Unknown)
    expect(v1.type).toBeNull()

    const v2 = ArrayVariant.create(Type.boolean)
    expect(v2.size).toBe(ArrayVariant.Size.Unknown)
    expect(v2.type).toEqual(Type.boolean)
  })

  test(`ArrayVariant.createNonEmpty`, () => {
    const v1 = ArrayVariant.createNonEmpty(null)
    expect(v1.size).toBe(ArrayVariant.Size.NonEmpty)
    expect(v1.type).toBeNull()

    const v2 = ArrayVariant.createNonEmpty(Type.boolean)
    expect(v2.size).toBe(ArrayVariant.Size.NonEmpty)
    expect(v2.type).toEqual(Type.boolean)
  })

  test(`clone`, () => {
    const v1 = ArrayVariant.createNonEmpty(Type.boolean)
    const v2 = v1.clone()
    expect(v1).not.toBe(v2)
    expect(v1.size).toBe(v2.size)
    expect(v1.type).toEqual(v2.type)
  })

  test(`ArrayVariant.or`, () => {
    const v1 = [ArrayVariant.createNonEmpty(Type.string)]
    const v2 = [ArrayVariant.createEmpty()]
    const result = ArrayVariant.or(v1, v2)
    expect(result).toEqual([ArrayVariant.create(Type.string)])
  })

  // string[1..] & [0] => null
  test(`ArrayVariant.and 1`, () => {
    const v1 = [ArrayVariant.createNonEmpty(Type.string)]
    const v2 = [ArrayVariant.createEmpty()]
    const result1 = ArrayVariant.and(v1, v2)
    expect(result1).toBe(null)
    const result2 = ArrayVariant.and(v2, v1)
    expect(result2).toBe(null)
  })

  // string[0..] & [0] => [0]
  test(`ArrayVariant.and 2`, () => {
    const v1 = [ArrayVariant.create(Type.string)]
    const v2 = [ArrayVariant.createEmpty()]
    const result1 = ArrayVariant.and(v1, v2)
    expect(result1).toEqual([ArrayVariant.createEmpty()])
    const result2 = ArrayVariant.and(v2, v1)
    expect(result2).toEqual([ArrayVariant.createEmpty()])
  })

  // const a: ((`a` | ``)[] | `a`[]) & ((`a` | 0)[] | (`a` | ``)[]) = [`a`, ``]
  test(`ArrayVariant.and 3`, () => {
    const v1 = [ArrayVariant.create(Type.string), ArrayVariant.create(Type.nonEmptyString)]
    const v2 = [ArrayVariant.create(Type.string.or(Type.number)), ArrayVariant.create(Type.string)]
    const result1 = ArrayVariant.and(v1, v2)
    expect(result1).toEqual([ArrayVariant.create(Type.string)])
  })

  test(`ArrayVariant.and 4`, () => {
    const v1 = [ArrayVariant.create(Type.string)]
    const v2 = [ArrayVariant.create(Type.string.or(Type.number))]
    const result1 = ArrayVariant.and(v1, v2)
    expect(result1).toEqual([ArrayVariant.create(Type.string)])
  })

  test(`ArrayVariant.and 5`, () => {
    const v1 = [ArrayVariant.create(Type.string)]
    const v2 = [ArrayVariant.create(Type.nonEmptyString)]
    const result1 = ArrayVariant.and(v1, v2)
    expect(result1).toBeNull()
  })

  describe(`simplifyArrayVariants`, () => {
    test(`samples`, () => {
      expect(simplifyArrayVariants(null)).toBeNull()
      expect(simplifyArrayVariants([])).toBeNull()
      expect(
        simplifyArrayVariants([ArrayVariant.create(Type.string), ArrayVariant.create(Type.nonEmptyString)]),
      ).toEqual([ArrayVariant.create(Type.string)])
      expect(simplifyArrayVariants([ArrayVariant.createEmpty()])).toEqual([ArrayVariant.createEmpty()])
      expect(simplifyArrayVariants([ArrayVariant.createEmpty(), ArrayVariant.createEmpty()])).toEqual([
        ArrayVariant.createEmpty(),
      ])
      expect(simplifyArrayVariants([ArrayVariant.create(null), ArrayVariant.createEmpty()])).toEqual([
        ArrayVariant.create(null),
      ])
      expect(
        simplifyArrayVariants([ArrayVariant.createNonEmpty(Type.string), ArrayVariant.create(Type.string)]),
      ).toEqual([ArrayVariant.create(Type.string)])
      expect(simplifyArrayVariants([ArrayVariant.createNonEmpty(Type.string), ArrayVariant.createEmpty()])).toEqual([
        ArrayVariant.create(Type.string),
      ])
      expect(
        simplifyArrayVariants([
          ArrayVariant.createNonEmpty(Type.string),
          ArrayVariant.createNonEmpty(Type.number),
          ArrayVariant.createNonEmpty(Type.boolean),
          ArrayVariant.createEmpty(),
        ]),
      ).toEqual([ArrayVariant.create(Type.string), ArrayVariant.create(Type.number), ArrayVariant.create(Type.boolean)])
      expect(
        simplifyArrayVariants([
          ArrayVariant.createEmpty(),
          ArrayVariant.createNonEmpty(Type.string),
          ArrayVariant.createNonEmpty(Type.number),
          ArrayVariant.createNonEmpty(Type.boolean),
          ArrayVariant.create(Type.number),
          ArrayVariant.create(Type.string),
        ]),
      ).toEqual([ArrayVariant.create(Type.number), ArrayVariant.create(Type.string), ArrayVariant.create(Type.boolean)])
    })
  })
})
