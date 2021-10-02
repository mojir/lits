import { UnexpectedNodeTypeError } from './errors'
import {
  AstNode,
  functionSymbol,
  LispishFunction,
  NameNode,
  NormalExpressionNode,
  SpecialExpressionNode,
  UserDefinedLispishFunction,
} from './parser/interface'

export function asAstNode(node: AstNode | undefined): AstNode {
  if (node === undefined) {
    throw Error('Expected an AST node, got undefined')
  }
  return node
}

export function asLispishFunction(value: unknown): LispishFunction {
  if (isLispishFunction(value)) {
    return value
  }
  throw Error(`Expected a Lispish function, got ${value}`)
}

export function asNameNode(node: AstNode | undefined): NameNode {
  if (node === undefined || node.type !== 'Name') {
    throw new UnexpectedNodeTypeError('Name', node)
  }
  return node
}

export function assertNameNode(node: AstNode | undefined): asserts node is NameNode {
  if (node === undefined || node.type !== 'Name') {
    throw new UnexpectedNodeTypeError('Name', node)
  }
}

export function asNotUndefined<T>(value: T | undefined, message = 'Unexpected end of input'): T {
  if (value === undefined) {
    throw Error(message)
  }
  return value
}

export function assertNotUndefined<T>(value: T | undefined, message = 'Unexpected end of input'): asserts value is T {
  if (value === undefined) {
    throw Error(message)
  }
}

export function assertFiniteNumber(value: unknown): asserts value is number {
  if (typeof value !== 'number' || !isFinite(value)) {
    throw TypeError(`Expected number, got: ${value} type="${typeof value}"`)
  }
}

export function asFiniteNumber(value: unknown): number {
  assertFiniteNumber(value)
  return value
}

export function assertPositiveNumber(value: unknown): asserts value is number {
  assertFiniteNumber(value)
  if (value <= 0) {
    throw TypeError(`Expected positive number, got ${value}`)
  }
}

export function assertNegativeNumber(value: unknown): asserts value is number {
  assertFiniteNumber(value)
  if (value >= 0) {
    throw TypeError(`Expected negative number, got ${value}`)
  }
}

export function assertNonNegativeNumber(value: unknown): asserts value is number {
  assertFiniteNumber(value)
  if (value < 0) {
    throw TypeError(`Expected non negative number, got ${value}`)
  }
}

export function assertNonPositiveNumber(value: unknown): asserts value is number {
  assertFiniteNumber(value)
  if (value > 0) {
    throw TypeError(`Expected non positive number, got ${value}`)
  }
}

export function assertInteger(value: unknown): asserts value is number {
  assertFiniteNumber(value)
  if (!Number.isInteger(value)) {
    throw TypeError(`Expected integer, got ${value}`)
  }
}

export function assertNumberGte(value: unknown, x: number): asserts value is number {
  assertFiniteNumber(value)
  if (value < x) {
    throw TypeError(`Expected parameter (${value}) to be a number equal or grater than ${x}`)
  }
}

export function assertNumberGt(value: unknown, x: number): asserts value is number {
  assertFiniteNumber(value)
  if (value <= x) {
    throw TypeError(`Expected parameter (${value}) to be a number grater than ${x}`)
  }
}

export function assertNumberLte(value: unknown, x: number): asserts value is number {
  assertFiniteNumber(value)
  if (value > x) {
    throw TypeError(`Expected parameter (${value}) to be a number equal or less than ${x}`)
  }
}

export function assertNumberLt(value: unknown, x: number): asserts value is number {
  assertFiniteNumber(value)
  if (value >= x) {
    throw TypeError(`Expected parameter (${value}) to be a number less than ${x}`)
  }
}

export function assertString(value: unknown): asserts value is string {
  if (typeof value !== 'string') {
    throw TypeError(`Expected string, got: ${value} type="${typeof value}"`)
  }
}

export function asNonEmptyString(value: unknown): string {
  if (typeof value !== 'string' || value.length === 0) {
    throw TypeError(`Expected non empty string, got: ${value} type="${typeof value}"`)
  }
  return value
}

export function assertRegExp(value: unknown): asserts value is RegExp {
  if (!(value instanceof RegExp)) {
    throw TypeError(`Expected RegExp, got: ${value} type="${typeof value}"`)
  }
}

export function assertStringOrRegExp(value: unknown): asserts value is RegExp | string {
  if (!(value instanceof RegExp || typeof value === 'string')) {
    throw TypeError(`Expected RegExp or string, got: ${value} type="${typeof value}"`)
  }
}

export function assertArray(value: unknown): asserts value is Array<unknown> {
  if (!Array.isArray(value)) {
    throw TypeError(`Expected list, got: ${value} type="${typeof value}"`)
  }
}

export function assertStringOrArray(value: unknown): asserts value is Array<unknown> | string {
  if (!(Array.isArray(value) || typeof value === 'string')) {
    throw TypeError(`Expected string or array, got: ${value} type="${typeof value}"`)
  }
}

export function assertObject(value: unknown): asserts value is Record<string, unknown> {
  if (
    value === null ||
    typeof value !== 'object' ||
    Array.isArray(value) ||
    value instanceof RegExp ||
    isLispishFunction(value)
  ) {
    throw TypeError(`Expected object, got: ${value} type="${typeof value}"`)
  }
}

export function assertObjectOrArray(value: unknown): asserts value is Record<string, unknown> | unknown[] {
  if (
    (value === null ||
      typeof value !== 'object' ||
      Array.isArray(value) ||
      value instanceof RegExp ||
      isLispishFunction(value)) &&
    !Array.isArray(value)
  ) {
    throw TypeError(`Expected object or array, got: ${value} type="${typeof value}"`)
  }
}

export function assertNumberNotZero(value: unknown): asserts value is number {
  assertFiniteNumber(value)
  if (value === 0) {
    throw TypeError(`Expected non zero value`)
  }
}

export function assertLength(
  count: number | { min?: number; max?: number },
  node: NormalExpressionNode | SpecialExpressionNode,
): void {
  const length = node.params.length
  if (typeof count === 'number') {
    if (length !== count) {
      throw Error(`Wrong number of arguments to "${node.name}", expected ${count}, got ${length}`)
    }
  } else {
    const { min, max } = count
    if (min === undefined && max === undefined) {
      throw Error('Min or max must be specified')
    }

    if (typeof min === 'number' && length < min) {
      throw Error(`Wrong number of arguments to "${node.name}", expected at least ${min}, got ${length}`)
    }

    if (typeof max === 'number' && length > max) {
      throw Error(`Wrong number of arguments to "${node.name}", expected at most ${max}, got ${length}`)
    }
  }
}

export function assertLengthEven(node: NormalExpressionNode): void {
  const length = node.params.length
  if (length % 2 !== 0) {
    throw Error(`Wrong number of arguments, expected an even number, got ${length}`)
  }
}

export function isLispishFunction(func: unknown): func is LispishFunction {
  if (func === null || typeof func !== 'object') {
    return false
  }
  return !!(func as LispishFunction)[functionSymbol]
}

export function assertLispishFunction(func: unknown): asserts func is LispishFunction {
  if (func === null || typeof func !== 'object') {
    throw Error(`Expected lispish function, got ${func}`)
  }
  if (!(func as LispishFunction)[functionSymbol]) {
    throw Error(`Expected lispish function, got ${JSON.stringify(func)}`)
  }
}

export function isUserDefinedLispishFunction(func: unknown): func is UserDefinedLispishFunction {
  if (isLispishFunction(func)) {
    return !!(func as UserDefinedLispishFunction).arguments
  }
  return false
}

export function isBuiltinLispishFunction(func: unknown): func is UserDefinedLispishFunction {
  return isLispishFunction(func) && !isUserDefinedLispishFunction(func)
}

export function assertStringArray(value: unknown): asserts value is string[] {
  if (!Array.isArray(value) || value.some(v => typeof v !== 'string')) {
    throw Error(`Expected an array of strings, got ${value}`)
  }
}
