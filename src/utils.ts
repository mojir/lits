import { AstNode, functionSymbol, LispishFunction, NameNode, UserDefinedLispishFunction } from './parser/interface'

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
    throw Error(`Expected a Name node, got ${node ? `${node.type} node` : 'undefined'}.`)
  }
  return node
}

export function assertNameNode(node: AstNode | undefined): asserts node is NameNode {
  if (node === undefined || node.type !== 'Name') {
    throw Error(`Expected a Name node, got ${node ? `${node.type} node` : 'undefined'}.`)
  }
}

export function asNotUndefined<T>(value: T | undefined): T {
  if (value === undefined) {
    throw Error(`Expected anything but undefined, got undefined`)
  }
  return value
}

export function assertNumber(value: unknown): asserts value is number {
  if (typeof value !== 'number') {
    throw TypeError(`Expected number, got: ${value} type="${typeof value}"`)
  }
}

export function assertPositiveNumber(value: unknown): asserts value is number {
  assertNumber(value)
  if (value <= 0) {
    throw TypeError(`Expected positive number, got ${value}`)
  }
}

export function assertNonNegativeNumber(value: unknown): asserts value is number {
  assertNumber(value)
  if (value < 0) {
    throw TypeError(`Expected non negative number, got ${value}`)
  }
}

export function assertInteger(value: unknown): asserts value is number {
  assertNumber(value)
  if (!Number.isInteger(value)) {
    throw TypeError(`Expected integer, got ${value}`)
  }
}

export function assertNonNegativeInteger(value: unknown): asserts value is number {
  assertInteger(value)
  if (value < 0) {
    throw TypeError(`Expected non negative integer, got ${value}`)
  }
}

export function assertNumberGte(value: unknown, x: number): asserts value is number {
  assertNumber(value)
  if (value < x) {
    throw TypeError(`Expected parameter (${value}) to be a number equal or grater than ${x}`)
  }
}

export function assertNumberGt(value: unknown, x: number): asserts value is number {
  assertNumber(value)
  if (value <= x) {
    throw TypeError(`Expected parameter (${value}) to be a number grater than ${x}`)
  }
}

export function assertNumberLte(value: unknown, x: number): asserts value is number {
  assertNumber(value)
  if (value > x) {
    throw TypeError(`Expected parameter (${value}) to be a number equal or less than ${x}`)
  }
}

export function assertNumberLt(value: unknown, x: number): asserts value is number {
  assertNumber(value)
  if (value >= x) {
    throw TypeError(`Expected parameter (${value}) to be a number less than ${x}`)
  }
}

export function assertString(value: unknown): asserts value is string {
  if (typeof value !== 'string') {
    throw TypeError(`Expected string, got: ${value} type="${typeof value}"`)
  }
}

export function assertRegExp(value: unknown): asserts value is RegExp {
  if (!(value instanceof RegExp)) {
    throw TypeError(`Expected RegExp, got: ${value} type="${typeof value}"`)
  }
}

export function assertArray(value: unknown): asserts value is Array<unknown> {
  if (!Array.isArray(value)) {
    throw TypeError(`Expected list, got: ${value} type="${typeof value}"`)
  }
}

export function assertNumberNotZero(value: unknown): asserts value is number {
  if (typeof value !== 'number') {
    throw TypeError(`Expected number, got: ${value} type="${typeof value}"`)
  }
  if (value === 0) {
    throw TypeError(`Expected non zero value, got: ${value}"`)
  }
}

export function assertLengthZero(params: unknown[]): void {
  if (params.length !== 0) {
    throw Error(`Wrong number of arguments, expected 0, got ${params.length}`)
  }
}

export function assertLengthOne(params: unknown[]): void {
  if (params.length !== 1) {
    throw Error(`Wrong number of arguments, expected 1, got ${params.length}`)
  }
}

export function assertLengthTwo(params: unknown[]): void {
  if (params.length !== 2) {
    throw Error(`Wrong number of arguments, expected 2, got ${params.length}`)
  }
}

export function assertLengthThree(params: unknown[]): void {
  if (params.length !== 3) {
    throw Error(`Wrong number of arguments, expected 3, got ${params.length}`)
  }
}

export function assertLengthOneOrMore(params: unknown[]): void {
  if (params.length < 1) {
    throw Error(`Wrong number of arguments, expected 1 or more, got ${params.length}`)
  }
}

export function assertLengthTwoOrThree(params: unknown[]): void {
  if (params.length !== 2 && params.length !== 3) {
    throw Error(`Wrong number of arguments, expected 2 or 3, got ${params.length}`)
  }
}

export function assertLengthOneOrTwo(params: unknown[]): void {
  if (params.length !== 1 && params.length !== 2) {
    throw Error(`Wrong number of arguments, expected 1 or 2, got ${params.length}`)
  }
}

export function assertLengthOneOrTwoOrThree(params: unknown[]): void {
  if (params.length !== 1 && params.length !== 2 && params.length !== 3) {
    throw Error(`Wrong number of arguments, expected 1, 2 or 3, got ${params.length}`)
  }
}

export function assertLengthEven(params: unknown[]): void {
  if (params.length % 2 !== 0) {
    throw Error(`Wrong number of arguments, expected an even number, got ${params.length}`)
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
    throw Error('Not a lispish function')
  }
  if (!(func as LispishFunction)[functionSymbol]) {
    throw Error('Not a lispish function')
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
