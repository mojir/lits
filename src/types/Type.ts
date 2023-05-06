import { LitsError } from '../errors'
import { Any } from '../interface'
import { DebugInfo } from '../tokenizer/interface'
import { MAX_NUMBER, MIN_NUMBER } from '../utils'
import {
  any,
  array,
  asNotNull,
  assertNotNull,
  assertNull,
  litsFunction,
  object,
  regularExpression,
} from '../utils/assertion'
import { ArrayVariant } from './ArrayVariant'
import {
  TypeName,
  UNKNWON_BITS,
  arrayTypeNames,
  builtinTypesBitMasks,
  orderedTypeNames,
  stringifyBitMask,
  typeToBitRecord,
} from './typeUtils'

export class Type {
  public readonly __TYPE__ = true
  public readonly bitmask: number
  arrayVariants: ArrayVariant[] | null

  private constructor(bitmask: number, arrayVariants: ArrayVariant[] | null = null) {
    if (bitmask & builtinTypesBitMasks.array) {
      assertNotNull(arrayVariants)
    }
    if (!(bitmask & builtinTypesBitMasks.array)) {
      assertNull(arrayVariants)
    }

    this.arrayVariants = arrayVariants
    if (bitmask & typeToBitRecord[`positive-non-integer`]) {
      bitmask |= typeToBitRecord[`positive-integer`]
    }
    if (bitmask & typeToBitRecord[`negative-non-integer`]) {
      bitmask |= typeToBitRecord[`negative-integer`]
    }
    this.bitmask = bitmask
  }

  public static readonly never = new Type(builtinTypesBitMasks.never)

  public static readonly nil = new Type(builtinTypesBitMasks.nil)

  public static readonly nan = new Type(builtinTypesBitMasks.nan)
  public static readonly positiveInfinity = new Type(builtinTypesBitMasks[`positive-infinity`])
  public static readonly negativeInfinity = new Type(builtinTypesBitMasks[`negative-infinity`])
  public static readonly infinity = new Type(builtinTypesBitMasks[`infinity`])

  public static readonly emptyString = new Type(builtinTypesBitMasks[`empty-string`])
  public static readonly nonEmptyString = new Type(builtinTypesBitMasks[`non-empty-string`])
  public static readonly string = new Type(builtinTypesBitMasks.string)

  public static readonly number = new Type(builtinTypesBitMasks.number)
  public static readonly positiveZero = new Type(builtinTypesBitMasks[`positive-zero`])
  public static readonly negativeZero = new Type(builtinTypesBitMasks[`negative-zero`])
  public static readonly zero = new Type(builtinTypesBitMasks.zero)
  public static readonly nonZeroNumber = new Type(builtinTypesBitMasks[`non-zero-number`])
  public static readonly positiveNumber = new Type(builtinTypesBitMasks[`positive-number`])
  public static readonly negativeNumber = new Type(builtinTypesBitMasks[`negative-number`])
  public static readonly nonPositiveNumber = new Type(builtinTypesBitMasks[`non-positive-number`])
  public static readonly nonNegativeNumber = new Type(builtinTypesBitMasks[`non-negative-number`])
  public static readonly float = new Type(builtinTypesBitMasks.float)
  public static readonly integer = new Type(builtinTypesBitMasks.integer)
  public static readonly nonZeroFloat = new Type(builtinTypesBitMasks[`non-zero-float`])
  public static readonly positiveFloat = new Type(builtinTypesBitMasks[`positive-float`])
  public static readonly negativeFloat = new Type(builtinTypesBitMasks[`negative-float`])
  public static readonly nonPositiveFloat = new Type(builtinTypesBitMasks[`non-positive-float`])
  public static readonly nonNegativeFloat = new Type(builtinTypesBitMasks[`non-negative-float`])
  public static readonly nonZeroInteger = new Type(builtinTypesBitMasks[`non-zero-integer`])
  public static readonly positiveInteger = new Type(builtinTypesBitMasks[`positive-integer`])
  public static readonly negativeInteger = new Type(builtinTypesBitMasks[`negative-integer`])
  public static readonly nonPositiveInteger = new Type(builtinTypesBitMasks[`non-positive-integer`])
  public static readonly nonNegativeInteger = new Type(builtinTypesBitMasks[`non-negative-integer`])

  public static readonly true = new Type(builtinTypesBitMasks.true)
  public static readonly false = new Type(builtinTypesBitMasks.false)
  public static readonly boolean = new Type(builtinTypesBitMasks.boolean)

  public static readonly emptyArray = new Type(builtinTypesBitMasks.array, [ArrayVariant.createEmpty()])
  public static readonly nonEmptyArray = new Type(builtinTypesBitMasks.array, [ArrayVariant.createNonEmpty(null)])
  public static readonly array = new Type(builtinTypesBitMasks.array, [ArrayVariant.create(null)])
  public static readonly createTypedArray = (type: Type) =>
    new Type(builtinTypesBitMasks.array, [ArrayVariant.create(type)])
  public static readonly createNonEmpyTypedArray = (type: Type) =>
    new Type(builtinTypesBitMasks.array, [ArrayVariant.createNonEmpty(type)])

  public static readonly emptyObject = new Type(builtinTypesBitMasks[`empty-object`])
  public static readonly nonEmptyObject = new Type(builtinTypesBitMasks[`non-empty-object`])
  public static readonly object = new Type(builtinTypesBitMasks.object)

  public static readonly regexp = new Type(builtinTypesBitMasks.regexp)

  public static readonly truthy = new Type(builtinTypesBitMasks.truthy, [ArrayVariant.create(null)])
  public static readonly falsy = new Type(builtinTypesBitMasks.falsy)

  public static readonly unknown = new Type(builtinTypesBitMasks.unknown, [ArrayVariant.create(null)])

  public static readonly function = new Type(builtinTypesBitMasks.function)

  public static isType(value: unknown): value is Type {
    return value instanceof Type
  }

  public static assertType(value: unknown, debugInfo: DebugInfo | undefined): asserts value is Type {
    if (!(value instanceof Type)) {
      throw new LitsError(`Expected instance of Type, got ${value}`, debugInfo)
    }
  }

  public static asType(value: unknown, debugInfo: DebugInfo | undefined): Type {
    if (!(value instanceof Type)) {
      throw new LitsError(`Expected instance of Type, got ${value}`, debugInfo)
    }
    return value
  }

  public static isNotType(value: unknown): boolean {
    return !(value instanceof Type)
  }

  public static of(input: unknown): Type {
    any.assert(input)
    if (input instanceof Type) {
      return input
    }
    if (input === null) {
      return Type.nil
    } else if (input === true) {
      return Type.true
    } else if (input === false) {
      return Type.false
    } else if (Number.isNaN(input)) {
      return Type.nan
    } else if (input === Infinity) {
      return Type.positiveInfinity
    } else if (input === -Infinity) {
      return Type.negativeInfinity
    } else if (typeof input === `string`) {
      return input ? Type.nonEmptyString : Type[`emptyString`]
    } else if (typeof input === `number`) {
      return input === 0
        ? Object.is(input, -0)
          ? Type.negativeZero
          : Type.positiveZero
        : input > MAX_NUMBER
        ? Type.positiveInfinity
        : input < MIN_NUMBER
        ? Type.negativeInfinity
        : Number.isInteger(input)
        ? input > 0
          ? Type.positiveInteger
          : Type.negativeInteger
        : input > 0
        ? Type.positiveFloat
        : Type.negativeFloat
    } else if (array.is(input)) {
      if (input.length === 0) {
        return Type.emptyArray
      }
      const type = Type.or(...input.map(i => Type.of(i)))
      return Type.createNonEmpyTypedArray(type)
    } else if (object.is(input)) {
      return Object.keys(input).length === 0 ? Type.emptyObject : Type.nonEmptyObject
    } else if (regularExpression.is(input)) {
      return Type.regexp
    } else if (litsFunction.is(input)) {
      return Type.function
    }
    throw Error(`Unexpected error, could not figure out type of ${input}`)
  }

  public static or(...types: Type[]): Type {
    const bitmask = types.reduce((result, type) => {
      return result | type.bitmask
    }, 0)

    const arrayVariants = types.reduce(
      (result: ArrayVariant[] | null, type) => ArrayVariant.or(result, type.arrayVariants),
      null,
    )

    return new Type(bitmask, arrayVariants)
  }

  public static and(...types: Type[]): Type {
    const bitmask = types.reduce((result, type) => {
      return result & type.bitmask
    }, UNKNWON_BITS)

    const first = types[0]?.arrayVariants ?? null
    const arrayVariants = types
      .slice(1)
      .reduce((result: ArrayVariant[] | null, type) => ArrayVariant.and(result, type.arrayVariants), first)

    return new Type(bitmask, arrayVariants)
  }

  public static exclude(first: Type, ...rest: Type[]): Type {
    return rest.reduce((result, type) => {
      if (result.bitmask & typeToBitRecord.array && type.bitmask & typeToBitRecord.array) {
        const arrayVariants = ArrayVariant.exclude(result.arrayVariants, type.arrayVariants)
        const bitmask = arrayVariants
          ? (result.bitmask & ~type.bitmask) | typeToBitRecord.array
          : result.bitmask & ~(type.bitmask | typeToBitRecord.array)
        return new Type(bitmask, arrayVariants)
      } else {
        return new Type(result.bitmask & ~type.bitmask, result.arrayVariants)
      }
    }, first)
  }

  public static is(a: unknown, bType: Type): boolean {
    const aType = Type.of(a)
    const { bitmask: bitmaskA } = aType
    const { bitmask: bitmaskB } = bType

    const bitmaskOK = !!(bitmaskA & bitmaskB && !(bitmaskA & ~bitmaskB))
    if (!bitmaskOK) {
      return false
    }
    return bitmaskA & typeToBitRecord.array
      ? ArrayVariant.is(aType.arrayVariants, bType.arrayVariants, Type.unknown)
      : true
  }

  public static equals(a: Type, b: Type, ...rest: Type[]): boolean {
    return [b, ...rest].every(type => {
      return a.bitmask === type.bitmask && ArrayVariant.equals(a.arrayVariants, type.arrayVariants)
    })
  }

  public static intersects(a: Type, b: Type): boolean {
    return a.and(b).bitmask !== 0
  }

  public static toValue(type: Any): Any {
    if (Type.isType(type)) {
      if (type.equals(Type.positiveZero)) {
        return 0
      }
      if (type.equals(Type.negativeZero)) {
        return -0
      }
      if (type.equals(Type.nan)) {
        return NaN
      }
      if (type.equals(Type.positiveInfinity)) {
        return Infinity
      }
      if (type.equals(Type.negativeInfinity)) {
        return -Infinity
      }
      if (type.equals(Type.emptyString)) {
        return ``
      }
      if (type.equals(Type.true)) {
        return true
      }
      if (type.equals(Type.false)) {
        return false
      }
      if (type.equals(Type.nil)) {
        return null
      }
      if (type.equals(Type.emptyArray)) {
        return []
      }
      if (type.equals(Type.emptyObject)) {
        return {}
      }
    }
    return type
  }

  public static toNumberOrNan(type: Type): Type | number {
    if (type.equals(Type.positiveZero)) {
      return 0
    }
    if (type.equals(Type.negativeZero)) {
      return -0
    }
    if (type.equals(Type.nan)) {
      return NaN
    }
    if (type.equals(Type.positiveInfinity)) {
      return Infinity
    }
    if (type.equals(Type.negativeInfinity)) {
      return -Infinity
    }
    return type
  }

  public static toSingelBits(type: Type): number[] {
    const result: number[] = []
    Object.values(typeToBitRecord).forEach(bitValue => {
      if (type.bitmask & bitValue) {
        result.push(bitValue)
      }
    })
    return result
  }

  public static split(type: Type): Type[] {
    return Type.toSingelBits(type).flatMap(bits => {
      if (bits === builtinTypesBitMasks.array && type.arrayVariants) {
        return type.arrayVariants.map(arrayVariant => new Type(bits, [arrayVariant]))
      }
      return new Type(bits)
    })
  }

  public or(...types: Type[]): Type {
    return Type.or(this, ...types)
  }

  public and(...types: Type[]): Type {
    return Type.and(this, ...types)
  }

  public exclude(...types: Type[]): Type {
    return Type.exclude(this, ...types)
  }

  public is(type: Type): boolean {
    if (type.isNever()) {
      return this.isNever()
    }
    return Type.is(this, type)
  }

  public intersects(type: Type): boolean {
    return Type.intersects(this, type)
  }

  public intersectsNonNumber(): boolean {
    return !!(this.bitmask & ~builtinTypesBitMasks.number)
  }

  public assertIs(type: Type, debugInfo: DebugInfo | undefined): void {
    if (!this.is(type)) {
      throw new LitsError(`Expected to be of type ${type.toString()}, but was ${this.toString()}`, debugInfo)
    }
  }

  public assertEquals(type: Type, debugInfo: DebugInfo | undefined): void {
    if (!this.equals(type)) {
      throw new LitsError(`Expected to be ${type.toString()}, but was ${this.toString()}`, debugInfo)
    }
  }

  public assertIntersects(type: Type, debugInfo: DebugInfo | undefined): void {
    if (!this.intersects(type)) {
      throw new LitsError(`Expected to intersect ${type.toString()}, but was ${this.toString()}`, debugInfo)
    }
  }

  public equals(type: Type, ...rest: Type[]): boolean {
    return Type.equals(this, type, ...rest)
  }

  public nilable(): Type {
    return this.or(Type.nil)
  }

  public isNever(): boolean {
    return this.bitmask === 0
  }

  public negateNumber() {
    let bitmask = this.bitmask
    if (this.bitmask & typeToBitRecord[`positive-infinity`] && !(this.bitmask & typeToBitRecord[`negative-infinity`])) {
      bitmask = (bitmask | typeToBitRecord[`negative-infinity`]) & ~typeToBitRecord[`positive-infinity`]
    }
    if (this.bitmask & typeToBitRecord[`negative-infinity`] && !(this.bitmask & typeToBitRecord[`positive-infinity`])) {
      bitmask = (bitmask | typeToBitRecord[`positive-infinity`]) & ~typeToBitRecord[`negative-infinity`]
    }

    if (this.bitmask & typeToBitRecord[`negative-integer`] && !(this.bitmask & typeToBitRecord[`positive-integer`])) {
      bitmask = (bitmask | typeToBitRecord[`positive-integer`]) & ~typeToBitRecord[`negative-integer`]
    }
    if (
      this.bitmask & typeToBitRecord[`negative-non-integer`] &&
      !(this.bitmask & typeToBitRecord[`positive-non-integer`])
    ) {
      bitmask = (bitmask | typeToBitRecord[`positive-non-integer`]) & ~typeToBitRecord[`negative-non-integer`]
    }
    if (this.bitmask & typeToBitRecord[`positive-integer`] && !(this.bitmask & typeToBitRecord[`negative-integer`])) {
      bitmask = (bitmask | typeToBitRecord[`negative-integer`]) & ~typeToBitRecord[`positive-integer`]
    }
    if (
      this.bitmask & typeToBitRecord[`positive-non-integer`] &&
      !(this.bitmask & typeToBitRecord[`negative-non-integer`])
    ) {
      bitmask = (bitmask | typeToBitRecord[`negative-non-integer`]) & ~typeToBitRecord[`positive-non-integer`]
    }

    if (this.bitmask & typeToBitRecord[`positive-zero`] && !(this.bitmask & typeToBitRecord[`negative-zero`])) {
      bitmask = (bitmask | typeToBitRecord[`negative-zero`]) & ~typeToBitRecord[`positive-zero`]
    }
    if (this.bitmask & typeToBitRecord[`negative-zero`] && !(this.bitmask & typeToBitRecord[`positive-zero`])) {
      bitmask = (bitmask | typeToBitRecord[`positive-zero`]) & ~typeToBitRecord[`negative-zero`]
    }

    return new Type(bitmask, this.arrayVariants)
  }

  public isUnknown(): boolean {
    return this.bitmask === UNKNWON_BITS
  }

  public isInteger(): boolean {
    return (
      this.intersects(Type.float) &&
      !(this.bitmask & (typeToBitRecord[`positive-non-integer`] | typeToBitRecord[`negative-non-integer`]))
    )
  }

  public toSingelBits(): number[] {
    return Type.toSingelBits(this)
  }

  public split(): Type[] {
    return Type.split(this)
  }

  public toValue(): Any {
    return Type.toValue(this)
  }

  public toNumberValue(): Type | number {
    return Type.toNumberOrNan(this)
  }

  public toString({ showDetails } = { showDetails: true }): string {
    const suffix = ` [Bitmask = ${stringifyBitMask(this.bitmask)}  (${this.bitmask})]`
    const typeString = this.getTypeString(showDetails)
    return `${typeString}${showDetails ? suffix : ``}`
  }

  private getTypeString(showDetails: boolean): string {
    const typeStrings: string[] = []
    let bits = this.bitmask

    for (const typeName of orderedTypeNames) {
      if (bits === 0) {
        break
      }
      const bitmask = builtinTypesBitMasks[typeName]
      if ((bits & bitmask) === bitmask) {
        if (arrayTypeNames.includes(typeName)) {
          asNotNull(this.arrayVariants).forEach(elem => {
            const arrayTypeName: TypeName =
              elem.size === ArrayVariant.Size.Empty
                ? `empty-array`
                : elem.size === ArrayVariant.Size.NonEmpty
                ? `non-empty-array`
                : `array`
            const innerArrayTypeString = elem.type ? `<${elem.type.toString({ showDetails })}>` : ``
            typeStrings.push(`::${arrayTypeName}${innerArrayTypeString}`)
          })
        } else {
          typeStrings.push(`::${typeName}`)
        }
        bits &= ~bitmask
      }
    }

    return typeStrings.length > 0 ? typeStrings.join(` | `) : `::never`
  }
}

ArrayVariant.unknownType = Type.unknown
