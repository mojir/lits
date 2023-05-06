const typeNames = [
  `never`,
  `nil`,
  `nan`,
  `positive-infinity`,
  `negative-infinity`,
  `infinity`,
  `empty-string`,
  `non-empty-string`,
  `string`,
  `positive-zero`,
  `negative-zero`,
  `zero`,
  `number`,
  `float`,
  `positive-float`,
  `negative-float`,
  `positive-number`,
  `negative-number`,
  `non-zero-number`,
  `non-positive-number`,
  `non-negative-number`,
  `non-zero-float`,
  `non-positive-float`,
  `non-negative-float`,
  `integer`,
  `non-zero-integer`,
  `positive-integer`,
  `negative-integer`,
  `non-positive-integer`,
  `non-negative-integer`,
  `true`,
  `false`,
  `boolean`,
  `empty-array`,
  `non-empty-array`,
  `array`,
  `empty-object`,
  `non-empty-object`,
  `object`,
  `regexp`,
  `function`,
  `unknown`,
  `truthy`,
  `falsy`,
] as const

export type TypeName = typeof typeNames[number]

export function isTypeName(typeName: string): typeName is TypeName {
  return typeNames.includes(typeName as TypeName)
}

export type PrimitiveTypeName =
  | `nil`
  | `nan`
  | `true`
  | `false`
  | `positive-zero`
  | `negative-zero`
  | `positive-integer`
  | `negative-integer`
  | `positive-non-integer`
  | `negative-non-integer`
  | `positive-infinity`
  | `negative-infinity`
  | `empty-string`
  | `non-empty-string`
  | `array`
  | `empty-object`
  | `non-empty-object`
  | `regexp`
  | `function`

export const typeToBitRecord: Record<PrimitiveTypeName, number> = {
  nil: 1 << 0,
  nan: 1 << 1,
  true: 1 << 2,
  false: 1 << 3,

  'positive-zero': 1 << 4,
  'negative-zero': 1 << 5,
  'positive-integer': 1 << 6,
  'negative-integer': 1 << 7,

  'positive-non-integer': 1 << 8,
  'negative-non-integer': 1 << 9,
  'positive-infinity': 1 << 10,
  'negative-infinity': 1 << 11,

  'empty-string': 1 << 12,
  'non-empty-string': 1 << 13,
  array: 1 << 14,

  'empty-object': 1 << 16,
  'non-empty-object': 1 << 17,
  regexp: 1 << 18,
  function: 1 << 19,
}

const allBitValues = Object.values(typeToBitRecord)

// All bits set to 1
export const UNKNWON_BITS = allBitValues.reduce((result, bit) => result | bit, 0)

const FALSY_BITS =
  typeToBitRecord.nil |
  typeToBitRecord[`positive-zero`] |
  typeToBitRecord[`negative-zero`] |
  typeToBitRecord[`empty-string`] |
  typeToBitRecord.false |
  typeToBitRecord.nan

// All non falsy bits
const TRUTHY_BITS = UNKNWON_BITS & ~FALSY_BITS

// Used for stringify Type only
export const orderedTypeNames: TypeName[] = [
  `unknown`,
  `number`,
  `non-zero-number`,
  `float`,
  `non-zero-number`,
  `non-zero-float`,
  `non-positive-number`,
  `non-positive-float`,
  `non-negative-number`,
  `non-negative-float`,
  `positive-number`,
  `positive-float`,
  `negative-number`,
  `negative-float`,
  `integer`,
  `non-zero-integer`,
  `non-positive-integer`,
  `non-negative-integer`,
  `positive-integer`,
  `negative-integer`,
  `zero`,
  `positive-zero`,
  `negative-zero`,
  `infinity`,
  `positive-infinity`,
  `negative-infinity`,
  `nan`,
  `boolean`,
  `true`,
  `false`,
  `array`,
  `empty-array`,
  `non-empty-array`,
  `object`,
  `empty-object`,
  `non-empty-object`,
  `string`,
  `non-empty-string`,
  `empty-string`,
  `regexp`,
  `function`,
  `nil`,
  `truthy`,
  `falsy`,
]

export const arrayTypeNames: TypeName[] = [`array`, `empty-array`, `non-empty-array`]

export const builtinTypesBitMasks: Record<TypeName, number> = {
  never: 0,

  nil: typeToBitRecord.nil,

  'empty-string': typeToBitRecord[`empty-string`],
  'non-empty-string': typeToBitRecord[`non-empty-string`],
  string: typeToBitRecord[`empty-string`] | typeToBitRecord[`non-empty-string`],

  // Numbers
  number:
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`] |
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`] |
    typeToBitRecord[`positive-infinity`] |
    typeToBitRecord[`negative-infinity`],

  float:
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`] |
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`],

  nan: typeToBitRecord.nan,
  'positive-infinity': typeToBitRecord[`positive-infinity`],
  'negative-infinity': typeToBitRecord[`negative-infinity`],

  infinity: typeToBitRecord[`negative-infinity`] | typeToBitRecord[`positive-infinity`],

  'positive-zero': typeToBitRecord[`positive-zero`],
  'negative-zero': typeToBitRecord[`negative-zero`],
  zero: typeToBitRecord[`positive-zero`] | typeToBitRecord[`negative-zero`],

  'non-zero-number':
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`] |
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`] |
    typeToBitRecord[`positive-infinity`] |
    typeToBitRecord[`negative-infinity`],

  'non-zero-float':
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`] |
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`],

  'positive-number':
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`] |
    typeToBitRecord[`positive-infinity`],

  'negative-number':
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`] |
    typeToBitRecord[`negative-infinity`],

  'non-positive-number':
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`] |
    typeToBitRecord[`negative-infinity`],

  'non-negative-number':
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`] |
    typeToBitRecord[`positive-infinity`],

  'positive-float': typeToBitRecord[`positive-non-integer`] | typeToBitRecord[`positive-integer`],
  'non-positive-float':
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`negative-non-integer`] |
    typeToBitRecord[`negative-integer`],

  'negative-float': typeToBitRecord[`negative-non-integer`] | typeToBitRecord[`negative-integer`],
  'non-negative-float':
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`positive-non-integer`] |
    typeToBitRecord[`positive-integer`],

  integer:
    typeToBitRecord[`positive-zero`] |
    typeToBitRecord[`negative-zero`] |
    typeToBitRecord[`positive-integer`] |
    typeToBitRecord[`negative-integer`],

  'non-zero-integer': typeToBitRecord[`negative-integer`] | typeToBitRecord[`positive-integer`],
  'positive-integer': typeToBitRecord[`positive-integer`],
  'negative-integer': typeToBitRecord[`negative-integer`],

  'non-positive-integer':
    typeToBitRecord[`positive-zero`] | typeToBitRecord[`negative-zero`] | typeToBitRecord[`negative-integer`],
  'non-negative-integer':
    typeToBitRecord[`positive-zero`] | typeToBitRecord[`negative-zero`] | typeToBitRecord[`positive-integer`],

  true: typeToBitRecord.true,
  false: typeToBitRecord.false,
  boolean: typeToBitRecord.true | typeToBitRecord.false,

  'empty-array': typeToBitRecord.array,
  'non-empty-array': typeToBitRecord.array,
  array: typeToBitRecord.array,

  'empty-object': typeToBitRecord[`empty-object`],
  'non-empty-object': typeToBitRecord[`non-empty-object`],
  object: typeToBitRecord[`empty-object`] | typeToBitRecord[`non-empty-object`],

  regexp: typeToBitRecord.regexp,

  function: typeToBitRecord.function,

  unknown: UNKNWON_BITS,

  truthy: TRUTHY_BITS,

  falsy: FALSY_BITS,
}

export function stringifyBitMask(bitmask: number): string {
  let mask = ``

  for (let index = 19; index >= 0; index -= 1) {
    const bitValue = 1 << index
    const zeroOrOne = bitmask & bitValue ? `1` : `0`
    const space = index !== 19 && (index + 1) % 4 === 0 ? ` ` : ``
    mask += `${space}${zeroOrOne}`
  }
  return mask
}
