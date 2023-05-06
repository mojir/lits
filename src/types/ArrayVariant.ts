import { type Type } from './Type'

enum Size {
  Empty = 0,
  NonEmpty = 1,
  Unknown = 2,
}

export class ArrayVariant {
  static Size = Size
  static unknownType: Type
  size: Size
  type: Type | null

  private constructor(size: Size, type: Type | null) {
    this.size = size
    this.type = type
  }

  static createEmpty(): ArrayVariant {
    return new ArrayVariant(Size.Empty, null)
  }

  static createNonEmpty(type: Type | null): ArrayVariant {
    return new ArrayVariant(Size.NonEmpty, type)
  }

  static create(type: Type | null): ArrayVariant {
    return new ArrayVariant(Size.Unknown, type)
  }

  static or(a: ArrayVariant[] | null, b: ArrayVariant[] | null): ArrayVariant[] | null {
    return simplifyArrayVariants([...(a ?? []), ...(b ?? [])])
  }

  static and(a: ArrayVariant[] | null, b: ArrayVariant[] | null): ArrayVariant[] | null {
    if (a === null || b === null) {
      return null
    }

    return simplifyArrayVariants(
      a.flatMap(aVariant => {
        const aType = aVariant.type ?? ArrayVariant.unknownType
        const aSize = aVariant.size
        const variants: ArrayVariant[] = []
        for (const bVariant of b) {
          const bType = bVariant.type ?? ArrayVariant.unknownType
          const bSize = bVariant.size
          if (aType.equals(bType)) {
            const size: null | Size =
              aSize === Size.Empty && bSize !== Size.NonEmpty
                ? Size.Empty
                : aSize === Size.NonEmpty && bSize !== Size.Empty
                ? Size.NonEmpty
                : aSize === Size.Unknown
                ? bSize
                : null
            if (size === null) {
              return []
            }
            variants.push(new ArrayVariant(size, aType))
          }
        }
        return variants
      }),
    )
  }
  static equals(a: ArrayVariant[] | null, b: ArrayVariant[] | null): boolean {
    if (!a && !b) {
      return true
    }
    if (!a || !b) {
      return false
    }
    if (a.length !== b.length) {
      return false
    }

    return a.every(aVariant => {
      const aSize = aVariant.size
      const aType = aVariant.type
      for (const bVariant of b) {
        const bSize = bVariant.size
        const bType = bVariant.type

        if (aSize === bSize) {
          if (!aType && !bType) {
            return true
          }
          if (!aType || !bType) {
            continue
          }
          if (aType.equals(bType)) {
            return true
          }
        }
      }
      return false
    })
  }

  static is(a: ArrayVariant[] | null, b: ArrayVariant[] | null, unknownType: Type): boolean {
    const simpleA = simplifyArrayVariants(a)
    const simpleB = simplifyArrayVariants(b)
    if (!simpleA && !simpleB) {
      return true
    }
    if (!simpleA || !simpleB) {
      return false
    }
    return simpleA.every(aElem => {
      const aType = aElem.type ?? unknownType
      return simpleB.some(bElem => {
        const bType = bElem.type
        if (!bType) {
          return true
        }
        if (!aType.is(bType)) {
          return false
        }
        return bElem.size === Size.Unknown || bElem.size === aElem.size
      })
    })
  }

  static exclude(aTypeVariants: ArrayVariant[] | null, bTypeVariants: ArrayVariant[] | null): ArrayVariant[] | null {
    if (!aTypeVariants) {
      return null
    }
    if (!bTypeVariants) {
      return aTypeVariants
    }

    return simplifyArrayVariants(
      aTypeVariants.flatMap(aVariant => {
        const typeVariant = aVariant.clone()
        for (const bVariant of bTypeVariants) {
          const bSize = bVariant.size
          if (bSize === Size.Empty || bSize === Size.Unknown) {
            if (typeVariant.size === Size.Empty) {
              return []
            } else {
              typeVariant.size = Size.NonEmpty
            }
            typeVariant.size = bSize === Size.Empty ? Size.NonEmpty : Size.Empty
          }
          if ((typeVariant.type ?? ArrayVariant.unknownType).equals(bVariant.type ?? ArrayVariant.unknownType)) {
            switch (typeVariant.size) {
              case Size.Empty:
                return bSize !== Size.NonEmpty ? [] : typeVariant
              case Size.NonEmpty:
                return bSize !== Size.Empty ? [] : typeVariant
              case Size.Unknown:
                if (bSize === Size.Unknown) {
                  return []
                }
                typeVariant.size = bSize === Size.Empty ? Size.NonEmpty : Size.Empty
                return typeVariant
            }
          }
        }
        return typeVariant
      }),
    )
  }

  clone(): ArrayVariant {
    return new ArrayVariant(this.size, this.type)
  }
}

export function simplifyArrayVariants(arrayVariants: ArrayVariant[] | null): ArrayVariant[] | null {
  if (!arrayVariants || arrayVariants.length === 0) {
    return null
  }

  // Sorting is important, see below
  const input: Array<ArrayVariant[][number] | null> = [...arrayVariants].sort((a, b) => b.size - a.size)
  const resultArrayVariants: ArrayVariant[] = []
  const size = arrayVariants.length

  for (let i = 0; i < size; i += 1) {
    const aVariant = input[i]

    if (!aVariant) {
      continue
    }
    const aType = aVariant.type ?? ArrayVariant.unknownType
    const resultVariant: ArrayVariant = aVariant.clone()

    for (let j = i + 1; j < size; j += 1) {
      const bVariant = input[j]
      if (!bVariant) {
        continue
      }
      const bType = bVariant.type ?? ArrayVariant.unknownType
      const bSize = bVariant.size

      if (aType.equals(bType)) {
        if (resultVariant.size === Size.Unknown || bSize === Size.Unknown || resultVariant.size !== bSize) {
          resultVariant.size = Size.Unknown
        }
        input[j] = null
      }

      // This would not work unless input is reversed sorted by Size
      if (bSize === Size.Empty) {
        if (resultVariant.size === Size.NonEmpty) {
          resultVariant.size = Size.Unknown
        }
      }
    }
    resultArrayVariants.push(resultVariant)
  }
  let emptyArrayFound = false
  return resultArrayVariants.filter(variant => {
    if (emptyArrayFound && variant.size === Size.Empty) {
      return false
    }
    if (variant.size !== Size.NonEmpty) {
      emptyArrayFound = true
    }
    return true
  })
}
