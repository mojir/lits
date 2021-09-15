import { AstNode, functionSymbol, LispishFunction, NameNode } from '../src/parser/interface'
import {
  asAstNode,
  asLispishFunction,
  asNameNode,
  asNotUndefined,
  assertArray,
  assertInteger,
  assertLengthEven,
  assertLengthOne,
  assertLengthOneOrMore,
  assertLengthOneOrTwoOrThree,
  assertLengthThree,
  assertLengthTwo,
  assertLengthTwoOrMore,
  assertLengthTwoOrThree,
  assertLengthZero,
  assertLispishFunction,
  assertNameNode,
  assertNegativeNumber,
  assertNonNegativeNumber,
  assertNonPositiveNumber,
  assertFiniteNumber,
  assertNumberGt,
  assertNumberGte,
  assertNumberLt,
  assertNumberLte,
  assertNumberNotZero,
  assertObject,
  assertObjectOrArray,
  assertPositiveNumber,
  assertRegExp,
  assertString,
  isBuiltinLispishFunction,
  isLispishFunction,
  isUserDefinedLispishFunction,
  asFiniteNumber,
} from '../src/utils'
describe('utils', () => {
  test('asAstNode', () => {
    expect(() => asAstNode(undefined)).toThrow()
    const node: AstNode = { type: 'Name', value: 'test' }
    expect(asAstNode(node)).toBe(node)
  })
  test('asLispishFunction', () => {
    expect(() => asLispishFunction(undefined)).toThrow()
    const lf: LispishFunction = {
      [functionSymbol]: true,
      arguments: [],
      varArgs: false,
      body: [],
    }
    expect(asLispishFunction(lf)).toBe(lf)
  })
  test('asNameNode', () => {
    expect(() => asNameNode(undefined)).toThrow()
    expect(() => asNameNode({ type: 'Number', value: 12 })).toThrow()
    const nameNode: NameNode = {
      type: 'Name',
      value: 'a-name',
    }
    expect(asNameNode(nameNode)).toBe(nameNode)
  })
  test('assertNameNode', () => {
    expect(() => assertNameNode(undefined)).toThrow()
    const nameNode: NameNode = {
      type: 'Name',
      value: 'a-name',
    }
    asNameNode(nameNode)
  })
  test('asNotUndefined', () => {
    expect(() => asNotUndefined(undefined)).toThrow()
    expect(asNotUndefined(null)).toBe(null)
    expect(asNotUndefined(false)).toBe(false)
    expect(asNotUndefined(true)).toBe(true)
    expect(asNotUndefined(0)).toBe(0)
    const obj = {}
    expect(asNotUndefined(obj)).toBe(obj)
  })
  test('assertArray', () => {
    expect(() => assertArray(0)).toThrow()
    expect(() => assertArray({})).toThrow()
    expect(() => assertArray([])).not.toThrow()
    expect(() => assertArray([1])).not.toThrow()
    expect(() => assertArray(true)).toThrow()
    expect(() => assertArray(null)).toThrow()
    expect(() => assertArray(undefined)).toThrow()
  })
  test('assertObject', () => {
    expect(() => assertObject(0)).toThrow()
    expect(() => assertObject({})).not.toThrow()
    expect(() => assertObject({ [functionSymbol]: true })).toThrow()
    expect(() => assertObject({ a: 1 })).not.toThrow()
    expect(() => assertObject(/test/)).toThrow()
    expect(() => assertObject([])).toThrow()
    expect(() => assertObject([1])).toThrow()
    expect(() => assertObject(true)).toThrow()
    expect(() => assertObject(null)).toThrow()
    expect(() => assertObject(undefined)).toThrow()
  })
  test('assertObjectOrArray', () => {
    expect(() => assertObjectOrArray(0)).toThrow()
    expect(() => assertObjectOrArray({})).not.toThrow()
    expect(() => assertObjectOrArray({ [functionSymbol]: true })).toThrow()
    expect(() => assertObjectOrArray({ a: 1 })).not.toThrow()
    expect(() => assertObjectOrArray(/test/)).toThrow()
    expect(() => assertObjectOrArray([])).not.toThrow()
    expect(() => assertObjectOrArray([1])).not.toThrow()
    expect(() => assertObjectOrArray(true)).toThrow()
    expect(() => assertObjectOrArray(null)).toThrow()
    expect(() => assertObjectOrArray(undefined)).toThrow()
  })
  test('assertInteger', () => {
    expect(() => assertInteger(-0)).not.toThrow()
    expect(() => assertInteger(-1)).not.toThrow()
    expect(() => assertInteger(1)).not.toThrow()
    expect(() => assertInteger(-0.1)).toThrow()
    expect(() => assertInteger(1.00001)).toThrow()
    expect(() => assertInteger('k')).toThrow()
    expect(() => assertInteger(false)).toThrow()
    expect(() => assertInteger(undefined)).toThrow()
    expect(() => assertInteger(null)).toThrow()
    expect(() => assertInteger([])).toThrow()
  })
  test('assertRegExp', () => {
    expect(() => assertRegExp(/a/)).not.toThrow()
    expect(() => assertRegExp(new RegExp('a'))).not.toThrow()
    expect(() => assertRegExp(0)).toThrow()
    expect(() => assertRegExp('0')).toThrow()
    expect(() => assertRegExp(null)).toThrow()
    expect(() => assertRegExp(undefined)).toThrow()
    expect(() => assertRegExp(false)).toThrow()
    expect(() => assertRegExp(true)).toThrow()
    expect(() => assertRegExp([])).toThrow()
    expect(() => assertRegExp({})).toThrow()
  })
  test('assertLengthEven', () => {
    expect(() => assertLengthEven([])).not.toThrow()
    expect(() => assertLengthEven([0])).toThrow()
    expect(() => assertLengthEven([0, 1])).not.toThrow()
    expect(() => assertLengthEven([0, 1, 2])).toThrow()
    expect(() => assertLengthEven([0, 1, 2, 3])).not.toThrow()
    expect(() => assertLengthEven([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthEven([0, 1, 2, 3, 4, 5])).not.toThrow()
  })
  test('assertLengthOne', () => {
    expect(() => assertLengthOne([])).toThrow()
    expect(() => assertLengthOne([0])).not.toThrow()
    expect(() => assertLengthOne([0, 1])).toThrow()
    expect(() => assertLengthOne([0, 1, 2])).toThrow()
    expect(() => assertLengthOne([0, 1, 2, 3])).toThrow()
    expect(() => assertLengthOne([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthOne([0, 1, 2, 3, 4, 5])).toThrow()
  })
  test('assertLengthOneOrMore', () => {
    expect(() => assertLengthOneOrMore([])).toThrow()
    expect(() => assertLengthOneOrMore([0])).not.toThrow()
    expect(() => assertLengthOneOrMore([0, 1])).not.toThrow()
    expect(() => assertLengthOneOrMore([0, 1, 2])).not.toThrow()
    expect(() => assertLengthOneOrMore([0, 1, 2, 3])).not.toThrow()
    expect(() => assertLengthOneOrMore([0, 1, 2, 3, 4])).not.toThrow()
    expect(() => assertLengthOneOrMore([0, 1, 2, 3, 4, 5])).not.toThrow()
  })
  test('assertLengthTwoOrMore', () => {
    expect(() => assertLengthTwoOrMore([])).toThrow()
    expect(() => assertLengthTwoOrMore([0])).toThrow()
    expect(() => assertLengthTwoOrMore([0, 1])).not.toThrow()
    expect(() => assertLengthTwoOrMore([0, 1, 2])).not.toThrow()
    expect(() => assertLengthTwoOrMore([0, 1, 2, 3])).not.toThrow()
    expect(() => assertLengthTwoOrMore([0, 1, 2, 3, 4])).not.toThrow()
    expect(() => assertLengthTwoOrMore([0, 1, 2, 3, 4, 5])).not.toThrow()
  })
  test('assertLengthOneOrTwoOrThree', () => {
    expect(() => assertLengthOneOrTwoOrThree([])).toThrow()
    expect(() => assertLengthOneOrTwoOrThree([0])).not.toThrow()
    expect(() => assertLengthOneOrTwoOrThree([0, 1])).not.toThrow()
    expect(() => assertLengthOneOrTwoOrThree([0, 1, 2])).not.toThrow()
    expect(() => assertLengthOneOrTwoOrThree([0, 1, 2, 3])).toThrow()
    expect(() => assertLengthOneOrTwoOrThree([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthOneOrTwoOrThree([0, 1, 2, 3, 4, 5])).toThrow()
  })
  test('assertLengthThree', () => {
    expect(() => assertLengthThree([])).toThrow()
    expect(() => assertLengthThree([0])).toThrow()
    expect(() => assertLengthThree([0, 1])).toThrow()
    expect(() => assertLengthThree([0, 1, 2])).not.toThrow()
    expect(() => assertLengthThree([0, 1, 2, 3])).toThrow()
    expect(() => assertLengthThree([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthThree([0, 1, 2, 3, 4, 5])).toThrow()
  })
  test('assertLengthTwo', () => {
    expect(() => assertLengthTwo([])).toThrow()
    expect(() => assertLengthTwo([0])).toThrow()
    expect(() => assertLengthTwo([0, 1])).not.toThrow()
    expect(() => assertLengthTwo([0, 1, 2])).toThrow()
    expect(() => assertLengthTwo([0, 1, 2, 3])).toThrow()
    expect(() => assertLengthTwo([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthTwo([0, 1, 2, 3, 4, 5])).toThrow()
  })
  test('assertLengthTwoOrThree', () => {
    expect(() => assertLengthTwoOrThree([])).toThrow()
    expect(() => assertLengthTwoOrThree([0])).toThrow()
    expect(() => assertLengthTwoOrThree([0, 1])).not.toThrow()
    expect(() => assertLengthTwoOrThree([0, 1, 2])).not.toThrow()
    expect(() => assertLengthTwoOrThree([0, 1, 2, 3])).toThrow()
    expect(() => assertLengthTwoOrThree([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthTwoOrThree([0, 1, 2, 3, 4, 5])).toThrow()
  })
  test('assertLengthZero', () => {
    expect(() => assertLengthZero([])).not.toThrow()
    expect(() => assertLengthZero([0])).toThrow()
    expect(() => assertLengthZero([0, 1])).toThrow()
    expect(() => assertLengthZero([0, 1, 2])).toThrow()
    expect(() => assertLengthZero([0, 1, 2, 3])).toThrow()
    expect(() => assertLengthZero([0, 1, 2, 3, 4])).toThrow()
    expect(() => assertLengthZero([0, 1, 2, 3, 4, 5])).toThrow()
  })
  test('assertLispishFunction', () => {
    const lf: LispishFunction = {
      [functionSymbol]: true,
      arguments: [],
      varArgs: false,
      body: [],
    }
    expect(() => assertLispishFunction(lf)).not.toThrow()
    expect(() => assertLispishFunction(1)).toThrow()
    expect(() => assertLispishFunction({})).toThrow()
  })
  test('assertPositiveNumber', () => {
    expect(() => assertPositiveNumber(-1)).toThrow()
    expect(() => assertPositiveNumber(-0.5)).toThrow()
    expect(() => assertPositiveNumber(0)).toThrow()
    expect(() => assertPositiveNumber(0.5)).not.toThrow()
    expect(() => assertPositiveNumber(1)).not.toThrow()
    expect(() => assertPositiveNumber('1')).toThrow()
    expect(() => assertPositiveNumber([])).toThrow()
    expect(() => assertPositiveNumber({})).toThrow()
    expect(() => assertPositiveNumber(true)).toThrow()
    expect(() => assertPositiveNumber(false)).toThrow()
    expect(() => assertPositiveNumber(null)).toThrow()
    expect(() => assertPositiveNumber(undefined)).toThrow()
  })
  test('assertNegativeNumber', () => {
    expect(() => assertNegativeNumber(-1)).not.toThrow()
    expect(() => assertNegativeNumber(-0.5)).not.toThrow()
    expect(() => assertNegativeNumber(0)).toThrow()
    expect(() => assertNegativeNumber(0.5)).toThrow()
    expect(() => assertNegativeNumber(1)).toThrow()
    expect(() => assertNegativeNumber('1')).toThrow()
    expect(() => assertNegativeNumber([])).toThrow()
    expect(() => assertNegativeNumber({})).toThrow()
    expect(() => assertNegativeNumber(true)).toThrow()
    expect(() => assertNegativeNumber(false)).toThrow()
    expect(() => assertNegativeNumber(null)).toThrow()
    expect(() => assertNegativeNumber(undefined)).toThrow()
  })
  test('assertNonNegativeNumber', () => {
    expect(() => assertNonNegativeNumber(-1)).toThrow()
    expect(() => assertNonNegativeNumber(-1.1)).toThrow()
    expect(() => assertNonNegativeNumber(0)).not.toThrow()
    expect(() => assertNonNegativeNumber(0.1)).not.toThrow()
    expect(() => assertNonNegativeNumber(1)).not.toThrow()
    expect(() => assertNonNegativeNumber(1.1)).not.toThrow()
    expect(() => assertNonNegativeNumber('1')).toThrow()
    expect(() => assertNonNegativeNumber([])).toThrow()
    expect(() => assertNonNegativeNumber({})).toThrow()
    expect(() => assertNonNegativeNumber(true)).toThrow()
    expect(() => assertNonNegativeNumber(false)).toThrow()
    expect(() => assertNonNegativeNumber(null)).toThrow()
    expect(() => assertNonNegativeNumber(undefined)).toThrow()
  })
  test('assertNonPositiveNumber', () => {
    expect(() => assertNonPositiveNumber(-1)).not.toThrow()
    expect(() => assertNonPositiveNumber(-1.1)).not.toThrow()
    expect(() => assertNonPositiveNumber(0)).not.toThrow()
    expect(() => assertNonPositiveNumber(0.1)).toThrow()
    expect(() => assertNonPositiveNumber(1)).toThrow()
    expect(() => assertNonPositiveNumber(1.1)).toThrow()
    expect(() => assertNonPositiveNumber('1')).toThrow()
    expect(() => assertNonPositiveNumber([])).toThrow()
    expect(() => assertNonPositiveNumber({})).toThrow()
    expect(() => assertNonPositiveNumber(true)).toThrow()
    expect(() => assertNonPositiveNumber(false)).toThrow()
    expect(() => assertNonPositiveNumber(null)).toThrow()
    expect(() => assertNonPositiveNumber(undefined)).toThrow()
  })
  test('assertFiniteNumber', () => {
    expect(() => assertFiniteNumber(-1)).not.toThrow()
    expect(() => assertFiniteNumber(-1.1)).not.toThrow()
    expect(() => assertFiniteNumber(0)).not.toThrow()
    expect(() => assertFiniteNumber(0.1)).not.toThrow()
    expect(() => assertFiniteNumber(1)).not.toThrow()
    expect(() => assertFiniteNumber(1.1)).not.toThrow()
    expect(() => assertFiniteNumber(Math.asin(2))).toThrow()
    expect(() => assertFiniteNumber(1 / 0)).toThrow()
    expect(() => assertFiniteNumber('1')).toThrow()
    expect(() => assertFiniteNumber([])).toThrow()
    expect(() => assertFiniteNumber({})).toThrow()
    expect(() => assertFiniteNumber(true)).toThrow()
    expect(() => assertFiniteNumber(false)).toThrow()
    expect(() => assertFiniteNumber(null)).toThrow()
    expect(() => assertFiniteNumber(undefined)).toThrow()
  })
  test('asFiniteNumber', () => {
    expect(asFiniteNumber(-1)).toBe(-1)
    expect(asFiniteNumber(-1.1)).toBe(-1.1)
    expect(asFiniteNumber(0)).toBe(0)
    expect(asFiniteNumber(0.1)).toBe(0.1)
    expect(asFiniteNumber(1)).toBe(1)
    expect(asFiniteNumber(1.1)).toBe(1.1)
    expect(() => asFiniteNumber(Math.asin(2))).toThrow()
    expect(() => asFiniteNumber(1 / 0)).toThrow()
    expect(() => asFiniteNumber('1')).toThrow()
    expect(() => asFiniteNumber('1')).toThrow()
    expect(() => asFiniteNumber([])).toThrow()
    expect(() => asFiniteNumber({})).toThrow()
    expect(() => asFiniteNumber(true)).toThrow()
    expect(() => asFiniteNumber(false)).toThrow()
    expect(() => asFiniteNumber(null)).toThrow()
    expect(() => asFiniteNumber(undefined)).toThrow()
  })
  test('assertNumberGt', () => {
    expect(() => assertNumberGt(0, 1)).toThrow()
    expect(() => assertNumberGt(0.5, 1)).toThrow()
    expect(() => assertNumberGt(1, 1)).toThrow()
    expect(() => assertNumberGt(1.5, 1)).not.toThrow()
    expect(() => assertNumberGt(2, 1)).not.toThrow()
    expect(() => assertNumberGt('2', 1)).toThrow()
    expect(() => assertNumberGt([], 1)).toThrow()
    expect(() => assertNumberGt(false, 1)).toThrow()
  })
  test('assertNumberGte', () => {
    expect(() => assertNumberGte(0, 1)).toThrow()
    expect(() => assertNumberGte(0.5, 1)).toThrow()
    expect(() => assertNumberGte(1, 1)).not.toThrow()
    expect(() => assertNumberGte(1.5, 1)).not.toThrow()
    expect(() => assertNumberGte(2, 1)).not.toThrow()
    expect(() => assertNumberGte('2', 1)).toThrow()
    expect(() => assertNumberGte([], 1)).toThrow()
    expect(() => assertNumberGte(false, 1)).toThrow()
  })
  test('assertNumberLt', () => {
    expect(() => assertNumberLt(0, 1)).not.toThrow()
    expect(() => assertNumberLt(0.5, 1)).not.toThrow()
    expect(() => assertNumberLt(1, 1)).toThrow()
    expect(() => assertNumberLt(1.5, 1)).toThrow()
    expect(() => assertNumberLt(2, 1)).toThrow()
    expect(() => assertNumberLt('2', 1)).toThrow()
    expect(() => assertNumberLt([], 1)).toThrow()
    expect(() => assertNumberLt(false, 1)).toThrow()
  })
  test('assertNumberLte', () => {
    expect(() => assertNumberLte(0, 1)).not.toThrow()
    expect(() => assertNumberLte(0.5, 1)).not.toThrow()
    expect(() => assertNumberLte(1, 1)).not.toThrow()
    expect(() => assertNumberLte(1.5, 1)).toThrow()
    expect(() => assertNumberLte(2, 1)).toThrow()
    expect(() => assertNumberLte('2', 1)).toThrow()
    expect(() => assertNumberLte([], 1)).toThrow()
    expect(() => assertNumberLte(false, 1)).toThrow()
  })
  test('assertNumberNotZero', () => {
    expect(() => assertNumberNotZero(-1)).not.toThrow()
    expect(() => assertNumberNotZero(-0.5)).not.toThrow()
    expect(() => assertNumberNotZero(0)).toThrow()
    expect(() => assertNumberNotZero(0.5)).not.toThrow()
    expect(() => assertNumberNotZero(1)).not.toThrow()
    expect(() => assertNumberNotZero('1')).toThrow()
    expect(() => assertNumberNotZero([])).toThrow()
    expect(() => assertNumberNotZero({})).toThrow()
    expect(() => assertNumberNotZero(true)).toThrow()
    expect(() => assertNumberNotZero(false)).toThrow()
    expect(() => assertNumberNotZero(null)).toThrow()
    expect(() => assertNumberNotZero(undefined)).toThrow()
  })
  test('assertString', () => {
    expect(() => assertString('')).not.toThrow()
    expect(() => assertString('1')).not.toThrow()
    expect(() => assertString(0)).toThrow()
    expect(() => assertString(1)).toThrow()
    expect(() => assertString(true)).toThrow()
    expect(() => assertString(false)).toThrow()
    expect(() => assertString(null)).toThrow()
    expect(() => assertString(undefined)).toThrow()
    expect(() => assertString([])).toThrow()
    expect(() => assertString({})).toThrow()
  })
  test('isLispishFunction', () => {
    const lf1: LispishFunction = {
      [functionSymbol]: true,
      arguments: [],
      varArgs: false,
      body: [],
    }
    const lf2: LispishFunction = {
      [functionSymbol]: true,
      builtin: '+',
    }
    expect(isLispishFunction(lf1)).toBe(true)
    expect(isLispishFunction(lf2)).toBe(true)
    expect(isLispishFunction('')).toBe(false)
    expect(isLispishFunction('1')).toBe(false)
    expect(isLispishFunction(0)).toBe(false)
    expect(isLispishFunction(1)).toBe(false)
    expect(isLispishFunction(true)).toBe(false)
    expect(isLispishFunction(false)).toBe(false)
    expect(isLispishFunction(null)).toBe(false)
    expect(isLispishFunction(undefined)).toBe(false)
    expect(isLispishFunction([])).toBe(false)
    expect(isLispishFunction({})).toBe(false)
  })

  test('isUserDefinedLispishFunction', () => {
    const lf1: LispishFunction = {
      [functionSymbol]: true,
      arguments: [],
      varArgs: false,
      body: [],
    }
    const lf2: LispishFunction = {
      [functionSymbol]: true,
      builtin: '+',
    }
    expect(isUserDefinedLispishFunction(lf1)).toBe(true)
    expect(isUserDefinedLispishFunction(lf2)).toBe(false)
    expect(isUserDefinedLispishFunction('')).toBe(false)
    expect(isUserDefinedLispishFunction('1')).toBe(false)
    expect(isUserDefinedLispishFunction(0)).toBe(false)
    expect(isUserDefinedLispishFunction(1)).toBe(false)
    expect(isUserDefinedLispishFunction(true)).toBe(false)
    expect(isUserDefinedLispishFunction(false)).toBe(false)
    expect(isUserDefinedLispishFunction(null)).toBe(false)
    expect(isUserDefinedLispishFunction(undefined)).toBe(false)
    expect(isUserDefinedLispishFunction([])).toBe(false)
    expect(isUserDefinedLispishFunction({})).toBe(false)
  })

  test('isBuiltinLispishFunction', () => {
    const lf1: LispishFunction = {
      [functionSymbol]: true,
      arguments: [],
      varArgs: false,
      body: [],
    }
    const lf2: LispishFunction = {
      [functionSymbol]: true,
      builtin: '+',
    }
    expect(isBuiltinLispishFunction(lf1)).toBe(false)
    expect(isBuiltinLispishFunction(lf2)).toBe(true)
    expect(isBuiltinLispishFunction('')).toBe(false)
    expect(isBuiltinLispishFunction('1')).toBe(false)
    expect(isBuiltinLispishFunction(0)).toBe(false)
    expect(isBuiltinLispishFunction(1)).toBe(false)
    expect(isBuiltinLispishFunction(true)).toBe(false)
    expect(isBuiltinLispishFunction(false)).toBe(false)
    expect(isBuiltinLispishFunction(null)).toBe(false)
    expect(isBuiltinLispishFunction(undefined)).toBe(false)
    expect(isBuiltinLispishFunction([])).toBe(false)
    expect(isBuiltinLispishFunction({})).toBe(false)
  })
})
