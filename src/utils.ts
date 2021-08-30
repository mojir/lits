import { AstNode, NameNode } from './parser/interface'

export function asAstNode(node: AstNode | undefined): AstNode {
  if (node === undefined) {
    throw Error('Expected an AST node, got undefined')
  }
  return node
}

export function asNameNode(node: AstNode | undefined): NameNode {
  if (node === undefined || node.type !== 'Name') {
    throw Error(`Expected an AST node, got ${node ? `${node.type} node` : 'undefined'}.`)
  }
  return node
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

export function assertNonNegativeNumber(value: unknown): asserts value is number {
  assertNumber(value)
  if (value < 0) {
    throw TypeError(`Expected non negative number, got ${value}`)
  }
}

export function assertNumberGte(value: unknown, x: number): asserts value is number {
  assertNumber(value)
  if (value < x) {
    throw TypeError(`Expected parameter (${value}) to be a number equal or grater than ${x}`)
  }
}

export function assertString(value: unknown): asserts value is string {
  if (typeof value !== 'string') {
    throw TypeError(`Expected string, got: ${value} type="${typeof value}"`)
  }
}

export function assertStringOrArray(value: unknown): asserts value is string | Array<unknown> {
  if (typeof value !== 'string' && !Array.isArray(value)) {
    throw TypeError(`Expected string or array, got: ${value} type="${typeof value}"`)
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

export function assertLengthEven(params: unknown[]): void {
  if (params.length % 2 !== 0) {
    throw Error(`Wrong number of arguments, expected an even number, got ${params.length}`)
  }
}
