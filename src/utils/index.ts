import { LitsError, UnexpectedNodeTypeError } from '../errors'
import { Context } from '../evaluator/interface'
import { Any, Arr, Coll, Obj, Seq } from '../interface'
import {
  AstNode,
  ExpressionNode,
  FUNCTION_SYMBOL,
  LitsFunction,
  NameNode,
  NormalExpressionNode,
  NormalExpressionNodeName,
  SpecialExpressionNode,
} from '../parser/interface'
import { TokenMeta } from '../tokenizer/interface'

export function asAstNode(node: AstNode | undefined, meta: TokenMeta): AstNode {
  if (node === undefined) {
    throw new LitsError(`Expected an AST node, got undefined`, meta)
  }
  return node
}

export function asLitsFunction(value: unknown, meta: TokenMeta): LitsFunction {
  if (isLitsFunction(value)) {
    return value
  }
  throw new LitsError(`Expected a Lits function, got ${value}`, meta)
}

export function asNameNode(node: AstNode | undefined, meta: TokenMeta): NameNode {
  if (node === undefined || node.type !== `Name`) {
    throw new UnexpectedNodeTypeError(`Name`, node, meta)
  }
  return node
}

export function assertNameNode(node: AstNode | undefined, meta: TokenMeta): asserts node is NameNode {
  if (node === undefined || node.type !== `Name`) {
    throw new UnexpectedNodeTypeError(`Name`, node, meta)
  }
}

export function asAny(value: unknown, meta: TokenMeta): Any {
  if (value === undefined) {
    throw new LitsError(`Unexpected end of input`, meta)
  }
  return value as Any
}

export function asNotUndefined<T>(value: T | undefined, meta: TokenMeta): T {
  if (value === undefined) {
    throw new LitsError(`Unexpected nil`, meta)
  }
  return value
}

export function assertNotUndefined<T>(value: T | undefined, meta: TokenMeta): asserts value is T {
  if (value === undefined) {
    throw new LitsError(`Unexpected nil`, meta)
  }
}

export function assertFiniteNumber(value: unknown, meta: TokenMeta): asserts value is number {
  if (typeof value !== `number` || !isFinite(value)) {
    throw new LitsError(`Expected number, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asFiniteNumber(value: unknown, meta: TokenMeta): number {
  assertFiniteNumber(value, meta)
  return value
}

export function assertPositiveNumber(value: unknown, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value <= 0) {
    throw new LitsError(`Expected positive number, got ${value}`, meta)
  }
}

export function assertNegativeNumber(value: unknown, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value >= 0) {
    throw new LitsError(`Expected negative number, got ${value}`, meta)
  }
}

export function assertNonNegativeNumber(value: unknown, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value < 0) {
    throw new LitsError(`Expected non negative number, got ${value}`, meta)
  }
}

export function assertNonNegativeInteger(value: unknown, meta: TokenMeta): asserts value is number {
  assertNonNegativeNumber(value, meta)
  assertInteger(value, meta)
}

export function assertNonPositiveNumber(value: unknown, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value > 0) {
    throw new LitsError(`Expected non positive number, got ${value}`, meta)
  }
}

export function assertInteger(value: unknown, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (!Number.isInteger(value)) {
    throw new LitsError(`Expected integer, got ${value}`, meta)
  }
}

export function assertNumberGte(value: unknown, x: number, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value < x) {
    throw new LitsError(`Expected parameter (${value}) to be a number equal or grater than ${x}`, meta)
  }
}

export function assertNumberGt(value: unknown, x: number, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value <= x) {
    throw new LitsError(`Expected parameter (${value}) to be a number grater than ${x}`, meta)
  }
}

export function assertNumberLte(value: unknown, x: number, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value > x) {
    throw new LitsError(`Expected parameter (${value}) to be a number equal or less than ${x}`, meta)
  }
}

export function assertNumberLt(value: unknown, x: number, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value >= x) {
    throw new LitsError(`Expected parameter (${value}) to be a number less than ${x}`, meta)
  }
}

export function isString(value: unknown): value is string {
  return typeof value === `string`
}

export function assertString(value: unknown, meta: TokenMeta): asserts value is string {
  if (!isString(value)) {
    throw new LitsError(`Expected string, got: ${value} type="${typeof value}"`, meta)
  }
}

export function assertStringOrRegExp(value: unknown, meta: TokenMeta): asserts value is RegExp | string {
  if (!(value instanceof RegExp || typeof value === `string`)) {
    throw new LitsError(`Expected RegExp or string, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asString(value: unknown, meta: TokenMeta): string {
  if (!isString(value)) {
    throw new LitsError(`Expected string, got: ${value} type="${typeof value}"`, meta)
  }
  return value
}

export function assertNonEmptyString(value: unknown, meta: TokenMeta): asserts value is string {
  assertString(value, meta)
  if (value.length === 0) {
    throw new LitsError(`Expected non empty string, got: ${value} type="${typeof value}"`, meta)
  }
}

export function isChar(value: unknown): value is string {
  return isString(value) && value.length === 1
}

export function assertChar(value: unknown, meta: TokenMeta): asserts value is string {
  if (!isChar(value)) {
    throw new LitsError(`Expected char, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asChar(value: unknown, meta: TokenMeta): string {
  assertChar(value, meta)
  return value
}

export function isStringOrNumber(value: unknown): boolean {
  return typeof value === `string` || typeof value === `number`
}

export function assertStringOrNumber(value: unknown, meta: TokenMeta): asserts value is string {
  if (!isStringOrNumber(value)) {
    throw new LitsError(`Expected string or number, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asStringOrNumber(value: unknown, meta: TokenMeta): string {
  assertStringOrNumber(value, meta)
  return value
}

export function asNonEmptyString(value: unknown, meta: TokenMeta): string {
  if (typeof value !== `string` || value.length === 0) {
    throw new LitsError(`Expected non empty string, got: ${value} type="${typeof value}"`, meta)
  }
  return value
}

export function isRegExp(value: unknown): value is RegExp {
  return value instanceof RegExp
}

export function assertRegExp(value: unknown, meta: TokenMeta): asserts value is RegExp {
  if (!(value instanceof RegExp)) {
    throw new LitsError(`Expected RegExp, got: ${value} type="${typeof value}"`, meta)
  }
}

export function assertNumberNotZero(value: unknown, meta: TokenMeta): asserts value is number {
  assertFiniteNumber(value, meta)
  if (value === 0) {
    throw new LitsError(`Expected non zero value`, meta)
  }
}

export function assertLength(
  count: number | { min?: number; max?: number },
  node: NormalExpressionNode | SpecialExpressionNode,
): void {
  const length = node.params.length
  if (typeof count === `number`) {
    if (length !== count) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected ${count}, got ${length}`,
        node.token.meta,
      )
    }
  } else {
    const { min, max } = count
    if (min === undefined && max === undefined) {
      throw new LitsError(`Min or max must be specified`, node.token.meta)
    }

    if (typeof min === `number` && length < min) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected at least ${min}, got ${length}`,
        node.token.meta,
      )
    }

    if (typeof max === `number` && length > max) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected at most ${max}, got ${length}`,
        node.token.meta,
      )
    }
  }
}

export function assertLengthEven(node: NormalExpressionNode): void {
  const length = node.params.length
  if (length % 2 !== 0) {
    throw new LitsError(`Wrong number of arguments, expected an even number, got ${length}`, node.token.meta)
  }
}

export function isLitsFunction(func: unknown): func is LitsFunction {
  if (func === null || typeof func !== `object`) {
    return false
  }
  return !!(func as LitsFunction)[FUNCTION_SYMBOL]
}

export function assertLitsFunction(func: unknown, meta: TokenMeta): asserts func is LitsFunction {
  if (!isLitsFunction(func)) {
    throw new LitsError(`Expected lits function, got ${JSON.stringify(func)}`, meta)
  }
}

export function assertStringArray(value: unknown, meta: TokenMeta): asserts value is string[] {
  if (!Array.isArray(value) || value.some(v => typeof v !== `string`)) {
    throw new LitsError(`Expected an array of strings, got ${value}`, meta)
  }
}

export function assertCharArray(arr: unknown, meta: TokenMeta): asserts arr is string[] {
  if (!Array.isArray(arr) || arr.some(v => typeof v !== `string` || v.length !== 1)) {
    throw new LitsError(`Expected an array of chars, got ${arr}`, meta)
  }
}

export function isExpressionNode(node: AstNode): node is ExpressionNode {
  return (
    node.type === `NormalExpression` ||
    node.type === `SpecialExpression` ||
    node.type === `Number` ||
    node.type === `String`
  )
}

export function assertNumber(value: unknown, meta: TokenMeta): asserts value is number {
  if (!isNumber(value)) {
    throw new LitsError(`Expected a number, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asNumber(value: unknown, meta: TokenMeta): number {
  assertNumber(value, meta)
  return value
}

export function assertArr(value: unknown, meta: TokenMeta): asserts value is Arr {
  if (!isArr(value)) {
    throw new LitsError(`Expected Arr, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asArr(value: unknown, meta: TokenMeta): Arr {
  assertArr(value, meta)
  return value
}

export function isAny(value: unknown): value is Any {
  return value !== undefined
}

export function assertAny(value: unknown, meta: TokenMeta): asserts value is Any {
  if (!isAny(value)) {
    throw new LitsError(`Expected Any, got: ${value} type="${typeof value}"`, meta)
  }
}

export function isSeq(value: unknown): value is Seq {
  return Array.isArray(value) || isString(value)
}

export function assertSeq(value: unknown, meta: TokenMeta): asserts value is Seq {
  if (!isSeq(value)) {
    throw new LitsError(`Expected string or array, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asSeq(value: unknown, meta: TokenMeta): Seq {
  if (!isSeq(value)) {
    throw new LitsError(`Expected string or array, got: ${value} type="${typeof value}"`, meta)
  }
  return value as Seq
}

export function assertObj(value: unknown, meta: TokenMeta): asserts value is Obj {
  if (!isObj(value)) {
    throw new LitsError(`Expected object, got: ${value} type="${typeof value}"`, meta)
  }
}

export function isObj(value: unknown): value is Obj {
  return !(
    value === null ||
    typeof value !== `object` ||
    Array.isArray(value) ||
    value instanceof RegExp ||
    isLitsFunction(value)
  )
}

export function isArr(value: unknown): value is Arr {
  return Array.isArray(value)
}

export function isColl(value: unknown): value is Coll {
  return isSeq(value) || isObj(value)
}

export function assertColl(value: unknown, meta: TokenMeta): asserts value is Coll {
  if (!isColl(value)) {
    throw new LitsError(`Expected collection, got: ${value} type="${typeof value}"`, meta)
  }
}

export function asColl(value: unknown, meta: TokenMeta): Coll {
  if (!isColl(value)) {
    throw new LitsError(`Expected collection, got: ${value} type="${typeof value}"`, meta)
  }
  return value
}

export function isNumber(value: unknown): value is number {
  return typeof value === `number`
}

export function isInteger(value: unknown): value is number {
  return Number.isInteger(value)
}

export function collHasKey(coll: unknown, key: string | number): boolean {
  if (!isColl(coll)) {
    return false
  }
  if (isString(coll) || isArr(coll)) {
    if (!isInteger(key)) {
      return false
    }
    return key >= 0 && key < coll.length
  }
  return !!Object.getOwnPropertyDescriptor(coll, key)
}

type Type = `null` | `boolean` | `number` | `string` | `object` | `array` | `regexp` | `unknown`

const sortOrderByType: Record<Type, number> = {
  boolean: 0,
  number: 1,
  string: 2,
  array: 3,
  object: 4,
  regexp: 5,
  unknown: 6,
  null: 7,
}

function getType(value: unknown): Type {
  if (value === null) {
    return `null`
  } else if (typeof value === `boolean`) {
    return `boolean`
  } else if (typeof value === `number`) {
    return `number`
  } else if (typeof value === `string`) {
    return `string`
  } else if (isArr(value)) {
    return `array`
  } else if (isObj(value)) {
    return `object`
  } else if (isRegExp(value)) {
    return `regexp`
  } else {
    return `unknown`
  }
}

export function compare(a: unknown, b: unknown): number {
  const aType = getType(a)
  const bType = getType(b)
  if (aType !== bType) {
    return Math.sign(sortOrderByType[aType] - sortOrderByType[bType])
  }

  switch (aType) {
    case `null`:
      return 0
    case `boolean`:
      if (a === b) {
        return 0
      }
      return a === false ? -1 : 1
    case `number`:
      return Math.sign((a as number) - (b as number))
    case `string`: {
      const aString = a as string
      const bString = b as string
      return aString < bString ? -1 : aString > bString ? 1 : 0
    }
    case `array`: {
      const aArray = a as Arr
      const bArray = b as Arr
      if (aArray.length < bArray.length) {
        return -1
      } else if (aArray.length > bArray.length) {
        return 1
      }
      for (let i = 0; i < aArray.length; i += 1) {
        const innerComp = compare(aArray[i], bArray[i])
        if (innerComp !== 0) {
          return innerComp
        }
      }
      return 0
    }
    case `object`: {
      const aObj = a as Obj
      const bObj = b as Obj
      return Math.sign(Object.keys(aObj).length - Object.keys(bObj).length)
    }
    case `regexp`: {
      const aString = (a as RegExp).source
      const bString = (b as RegExp).source
      return aString < bString ? -1 : aString > bString ? 1 : 0
    }
    case `unknown`:
      return 0
  }
}

export function isNormalExpressionNodeName(node: NormalExpressionNode): node is NormalExpressionNodeName {
  return typeof node.name === `string`
}

export function deepEqual(a: Any, b: Any, meta: TokenMeta): boolean {
  if (a === b) {
    return true
  }

  if (typeof a === `number` && typeof b === `number`) {
    return Math.abs(a - b) < Number.EPSILON
  }

  if (isArr(a) && isArr(b)) {
    if (a.length !== b.length) {
      return false
    }
    for (let i = 0; i < a.length; i += 1) {
      if (!deepEqual(asAny(a[i], meta), asAny(b[i], meta), meta)) {
        return false
      }
    }
    return true
  }
  if (a instanceof RegExp && b instanceof RegExp) {
    return a.toString() === b.toString()
  }
  if (typeof a === `object` && a !== null && typeof b === `object` && b !== null) {
    const aObj = a as Record<string, unknown>
    const bObj = b as Record<string, unknown>
    const aKeys = Object.keys(aObj)
    const bKeys = Object.keys(bObj)
    if (aKeys.length !== bKeys.length) {
      return false
    }
    for (let i = 0; i < aKeys.length; i += 1) {
      const key = asNotUndefined(aKeys[i], meta)
      if (!deepEqual(toAny(aObj[key]), toAny(bObj[key]), meta)) {
        return false
      }
    }
    return true
  }
  return false
}

export function toNonNegativeInteger(number: number): number {
  return Math.max(0, Math.ceil(number))
}

export function assertMax(value: number, maxNumber: number, meta: TokenMeta): void {
  if (value > maxNumber) {
    throw new LitsError(`Expected number less than or equal to ${maxNumber}'`, meta)
  }
}

export function toAny(value: unknown): Any {
  return (value ?? null) as Any
}

function clone<T>(value: T): T {
  if (isObj(value)) {
    return Object.entries(value).reduce((result: Obj, entry) => {
      const [key, val] = entry
      result[key] = clone(val)
      return result
    }, {}) as T
  }
  if (isArr(value)) {
    return value.map(item => clone(item)) as unknown as T
  }
  return value
}

export function cloneColl<T extends Coll>(value: T): T {
  return clone(value)
}

export function createContextFromValues(values?: Obj): Context {
  if (!values) {
    return {}
  }
  return Object.entries(values).reduce((context: Context, [key, value]) => {
    context[key] = { value: toAny(value) }
    return context
  }, {})
}
