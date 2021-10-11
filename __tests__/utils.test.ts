import { AstNode, functionSymbol, LispishFunction, NameNode, NormalExpressionNode } from '../src/parser/interface'
import {
  asAstNode,
  asLispishFunction,
  asNameNode,
  asNotUndefined,
  assertArr,
  assertInteger,
  assertLengthEven,
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
  assertObj,
  assertObjectOrArray,
  assertPositiveNumber,
  assertRegExp,
  assertString,
  asNonEmptyString,
  isBuiltinLispishFunction,
  isLispishFunction,
  isUserDefinedLispishFunction,
  asFiniteNumber,
  assertNotUndefined,
  assertLength,
  assertSeq,
  assertStringOrRegExp,
  assertStringArray,
  isNumber,
  assertNumber,
  isInteger,
  hasKey,
  isRegExp,
} from '../src/utils'
describe(`utils`, () => {
  test(`asAstNode`, () => {
    expect(() => asAstNode(undefined)).toThrow()
    const node: AstNode = { type: `Name`, value: `test` }

    expect(asAstNode(node)).toBe(node)
  })
  test(`asLispishFunction`, () => {
    expect(() => asLispishFunction(undefined)).toThrow()
    const lf: LispishFunction = {
      [functionSymbol]: true,
      arguments: {
        mandatoryArguments: [],
        optionalArguments: [],
      },
      functionContext: {},
      name: undefined,
      body: [],
    }
    expect(asLispishFunction(lf)).toBe(lf)
  })
  test(`asNameNode`, () => {
    expect(() => asNameNode(undefined)).toThrow()
    expect(() => asNameNode({ type: `Number`, value: 12 })).toThrow()
    const nameNode: NameNode = {
      type: `Name`,
      value: `a-name`,
    }
    expect(asNameNode(nameNode)).toBe(nameNode)
  })
  test(`assertNameNode`, () => {
    expect(() => assertNameNode(undefined)).toThrow()
    const nameNode: NameNode = {
      type: `Name`,
      value: `a-name`,
    }
    asNameNode(nameNode)
  })
  test(`asNotUndefined`, () => {
    expect(() => asNotUndefined(undefined)).toThrow()
    expect(() => asNotUndefined(undefined, `XXX`)).toThrow()
    expect(asNotUndefined(null)).toBe(null)
    expect(asNotUndefined(false)).toBe(false)
    expect(asNotUndefined(true)).toBe(true)
    expect(asNotUndefined(0)).toBe(0)
    const obj = {}
    expect(asNotUndefined(obj)).toBe(obj)
  })
  test(`assertNotUndefined`, () => {
    expect(() => assertNotUndefined(undefined)).toThrow()
    expect(() => assertNotUndefined(undefined, `XXX`)).toThrow()
    expect(() => assertNotUndefined(null)).not.toThrow()
    expect(() => assertNotUndefined(false)).not.toThrow()
    expect(() => assertNotUndefined(true)).not.toThrow()
    expect(() => assertNotUndefined(0)).not.toThrow()
    expect(() => assertNotUndefined({})).not.toThrow()
  })
  test(`asNonEmptyString`, () => {
    expect(asNonEmptyString(`1`)).toBe(`1`)
    expect(() => asNonEmptyString(``)).toThrow()
    expect(() => asNonEmptyString(0)).toThrow()
    expect(() => asNonEmptyString(1)).toThrow()
    expect(() => asNonEmptyString(true)).toThrow()
    expect(() => asNonEmptyString(false)).toThrow()
    expect(() => asNonEmptyString(null)).toThrow()
    expect(() => asNonEmptyString(undefined)).toThrow()
    expect(() => asNonEmptyString([])).toThrow()
    expect(() => asNonEmptyString({})).toThrow()
  })

  test(`assertArr`, () => {
    expect(() => assertArr(0)).toThrow()
    expect(() => assertArr({})).toThrow()
    expect(() => assertArr([])).not.toThrow()
    expect(() => assertArr([1])).not.toThrow()
    expect(() => assertArr(true)).toThrow()
    expect(() => assertArr(null)).toThrow()
    expect(() => assertArr(undefined)).toThrow()
  })
  test(`assertObj`, () => {
    expect(() => assertObj(0)).toThrow()
    expect(() => assertObj({})).not.toThrow()
    expect(() => assertObj({ [functionSymbol]: true })).toThrow()
    expect(() => assertObj({ a: 1 })).not.toThrow()
    expect(() => assertObj(/test/)).toThrow()
    expect(() => assertObj([])).toThrow()
    expect(() => assertObj([1])).toThrow()
    expect(() => assertObj(true)).toThrow()
    expect(() => assertObj(null)).toThrow()
    expect(() => assertObj(undefined)).toThrow()
  })
  test(`assertObjectOrArray`, () => {
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
  test(`assertInteger`, () => {
    expect(() => assertInteger(-0)).not.toThrow()
    expect(() => assertInteger(-1)).not.toThrow()
    expect(() => assertInteger(1)).not.toThrow()
    expect(() => assertInteger(-0.1)).toThrow()
    expect(() => assertInteger(1.00001)).toThrow()
    expect(() => assertInteger(`k`)).toThrow()
    expect(() => assertInteger(false)).toThrow()
    expect(() => assertInteger(undefined)).toThrow()
    expect(() => assertInteger(null)).toThrow()
    expect(() => assertInteger([])).toThrow()
  })
  test(`assertRegExp`, () => {
    expect(() => assertRegExp(/a/)).not.toThrow()
    expect(() => assertRegExp(new RegExp(`a`))).not.toThrow()
    expect(() => assertRegExp(0)).toThrow()
    expect(() => assertRegExp(`0`)).toThrow()
    expect(() => assertRegExp(null)).toThrow()
    expect(() => assertRegExp(undefined)).toThrow()
    expect(() => assertRegExp(false)).toThrow()
    expect(() => assertRegExp(true)).toThrow()
    expect(() => assertRegExp([])).toThrow()
    expect(() => assertRegExp({})).toThrow()
  })

  function node(arr: number[]): NormalExpressionNode {
    const astNodes: AstNode[] = arr.map(n => ({ type: `Number`, value: n }))
    return {
      name: `let`,
      params: astNodes,
      type: `NormalExpression`,
    }
  }

  test(`assertLengthEven`, () => {
    expect(() => assertLengthEven(node([]))).not.toThrow()
    expect(() => assertLengthEven(node([0]))).toThrow()
    expect(() => assertLengthEven(node([0, 1]))).not.toThrow()
    expect(() => assertLengthEven(node([0, 1, 2]))).toThrow()
    expect(() => assertLengthEven(node([0, 1, 2, 3]))).not.toThrow()
    expect(() => assertLengthEven(node([0, 1, 2, 3, 4]))).toThrow()
    expect(() => assertLengthEven(node([0, 1, 2, 3, 4, 5]))).not.toThrow()
  })

  test(`assertLength`, () => {
    expect(() => assertLength(0, node([]))).not.toThrow()
    expect(() => assertLength(0, node([1]))).toThrow()
    expect(() => assertLength(1, node([1]))).not.toThrow()
    expect(() => assertLength(1, node([]))).toThrow()
    expect(() => assertLength(1, node([1, 2]))).toThrow()
    expect(() => assertLength(2, node([1, 2]))).not.toThrow()
    expect(() => assertLength(2, node([1]))).toThrow()
    expect(() => assertLength(2, node([1, 2, 3]))).toThrow()
    expect(() => assertLength({}, node([]))).toThrow()
    expect(() => assertLength({ min: 1 }, node([1, 2, 3, 4, 5]))).not.toThrow()
    expect(() => assertLength({ min: 1 }, node([1, 2, 3, 4]))).not.toThrow()
    expect(() => assertLength({ min: 1 }, node([1, 2, 3]))).not.toThrow()
    expect(() => assertLength({ min: 1 }, node([1, 2]))).not.toThrow()
    expect(() => assertLength({ min: 1 }, node([1]))).not.toThrow()
    expect(() => assertLength({ min: 1 }, node([]))).toThrow()
    expect(() => assertLength({ max: 3 }, node([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertLength({ max: 3 }, node([1, 2, 3, 4]))).toThrow()
    expect(() => assertLength({ max: 3 }, node([1, 2, 3]))).not.toThrow()
    expect(() => assertLength({ max: 3 }, node([1, 2]))).not.toThrow()
    expect(() => assertLength({ max: 3 }, node([1]))).not.toThrow()
    expect(() => assertLength({ max: 3 }, node([]))).not.toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([]))).toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([1]))).not.toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([1, 2]))).not.toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([1, 2, 3]))).not.toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([1, 2, 3, 4]))).toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertLength({ min: 1, max: 3 }, node([1, 2, 3, 4, 5, 6]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([1]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([1, 2]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([1, 2, 3]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([1, 2, 3, 4]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertLength({ min: 3, max: 1 }, node([1, 2, 3, 4, 5, 6]))).toThrow()
  })

  test(`assertLispishFunction`, () => {
    const lf: LispishFunction = {
      [functionSymbol]: true,
      arguments: {
        mandatoryArguments: [],
        optionalArguments: [],
      },
      functionContext: {},
      name: undefined,
      body: [],
    }
    expect(() => assertLispishFunction(lf)).not.toThrow()
    expect(() => assertLispishFunction(1)).toThrow()
    expect(() => assertLispishFunction({})).toThrow()
  })
  test(`assertPositiveNumber`, () => {
    expect(() => assertPositiveNumber(-1)).toThrow()
    expect(() => assertPositiveNumber(-0.5)).toThrow()
    expect(() => assertPositiveNumber(0)).toThrow()
    expect(() => assertPositiveNumber(0.5)).not.toThrow()
    expect(() => assertPositiveNumber(1)).not.toThrow()
    expect(() => assertPositiveNumber(`1`)).toThrow()
    expect(() => assertPositiveNumber([])).toThrow()
    expect(() => assertPositiveNumber({})).toThrow()
    expect(() => assertPositiveNumber(true)).toThrow()
    expect(() => assertPositiveNumber(false)).toThrow()
    expect(() => assertPositiveNumber(null)).toThrow()
    expect(() => assertPositiveNumber(undefined)).toThrow()
  })
  test(`assertNegativeNumber`, () => {
    expect(() => assertNegativeNumber(-1)).not.toThrow()
    expect(() => assertNegativeNumber(-0.5)).not.toThrow()
    expect(() => assertNegativeNumber(0)).toThrow()
    expect(() => assertNegativeNumber(0.5)).toThrow()
    expect(() => assertNegativeNumber(1)).toThrow()
    expect(() => assertNegativeNumber(`1`)).toThrow()
    expect(() => assertNegativeNumber([])).toThrow()
    expect(() => assertNegativeNumber({})).toThrow()
    expect(() => assertNegativeNumber(true)).toThrow()
    expect(() => assertNegativeNumber(false)).toThrow()
    expect(() => assertNegativeNumber(null)).toThrow()
    expect(() => assertNegativeNumber(undefined)).toThrow()
  })
  test(`assertNonNegativeNumber`, () => {
    expect(() => assertNonNegativeNumber(-1)).toThrow()
    expect(() => assertNonNegativeNumber(-1.1)).toThrow()
    expect(() => assertNonNegativeNumber(0)).not.toThrow()
    expect(() => assertNonNegativeNumber(0.1)).not.toThrow()
    expect(() => assertNonNegativeNumber(1)).not.toThrow()
    expect(() => assertNonNegativeNumber(1.1)).not.toThrow()
    expect(() => assertNonNegativeNumber(`1`)).toThrow()
    expect(() => assertNonNegativeNumber([])).toThrow()
    expect(() => assertNonNegativeNumber({})).toThrow()
    expect(() => assertNonNegativeNumber(true)).toThrow()
    expect(() => assertNonNegativeNumber(false)).toThrow()
    expect(() => assertNonNegativeNumber(null)).toThrow()
    expect(() => assertNonNegativeNumber(undefined)).toThrow()
  })
  test(`assertNonPositiveNumber`, () => {
    expect(() => assertNonPositiveNumber(-1)).not.toThrow()
    expect(() => assertNonPositiveNumber(-1.1)).not.toThrow()
    expect(() => assertNonPositiveNumber(0)).not.toThrow()
    expect(() => assertNonPositiveNumber(0.1)).toThrow()
    expect(() => assertNonPositiveNumber(1)).toThrow()
    expect(() => assertNonPositiveNumber(1.1)).toThrow()
    expect(() => assertNonPositiveNumber(`1`)).toThrow()
    expect(() => assertNonPositiveNumber([])).toThrow()
    expect(() => assertNonPositiveNumber({})).toThrow()
    expect(() => assertNonPositiveNumber(true)).toThrow()
    expect(() => assertNonPositiveNumber(false)).toThrow()
    expect(() => assertNonPositiveNumber(null)).toThrow()
    expect(() => assertNonPositiveNumber(undefined)).toThrow()
  })
  test(`assertFiniteNumber`, () => {
    expect(() => assertFiniteNumber(-1)).not.toThrow()
    expect(() => assertFiniteNumber(-1.1)).not.toThrow()
    expect(() => assertFiniteNumber(0)).not.toThrow()
    expect(() => assertFiniteNumber(0.1)).not.toThrow()
    expect(() => assertFiniteNumber(1)).not.toThrow()
    expect(() => assertFiniteNumber(1.1)).not.toThrow()
    expect(() => assertFiniteNumber(Math.asin(2))).toThrow()
    expect(() => assertFiniteNumber(1 / 0)).toThrow()
    expect(() => assertFiniteNumber(`1`)).toThrow()
    expect(() => assertFiniteNumber([])).toThrow()
    expect(() => assertFiniteNumber({})).toThrow()
    expect(() => assertFiniteNumber(true)).toThrow()
    expect(() => assertFiniteNumber(false)).toThrow()
    expect(() => assertFiniteNumber(null)).toThrow()
    expect(() => assertFiniteNumber(undefined)).toThrow()
  })
  test(`asFiniteNumber`, () => {
    expect(asFiniteNumber(-1)).toBe(-1)
    expect(asFiniteNumber(-1.1)).toBe(-1.1)
    expect(asFiniteNumber(0)).toBe(0)
    expect(asFiniteNumber(0.1)).toBe(0.1)
    expect(asFiniteNumber(1)).toBe(1)
    expect(asFiniteNumber(1.1)).toBe(1.1)
    expect(() => asFiniteNumber(Math.asin(2))).toThrow()
    expect(() => asFiniteNumber(1 / 0)).toThrow()
    expect(() => asFiniteNumber(`1`)).toThrow()
    expect(() => asFiniteNumber(`1`)).toThrow()
    expect(() => asFiniteNumber([])).toThrow()
    expect(() => asFiniteNumber({})).toThrow()
    expect(() => asFiniteNumber(true)).toThrow()
    expect(() => asFiniteNumber(false)).toThrow()
    expect(() => asFiniteNumber(null)).toThrow()
    expect(() => asFiniteNumber(undefined)).toThrow()
  })
  test(`assertNumberGt`, () => {
    expect(() => assertNumberGt(0, 1)).toThrow()
    expect(() => assertNumberGt(0.5, 1)).toThrow()
    expect(() => assertNumberGt(1, 1)).toThrow()
    expect(() => assertNumberGt(1.5, 1)).not.toThrow()
    expect(() => assertNumberGt(2, 1)).not.toThrow()
    expect(() => assertNumberGt(`2`, 1)).toThrow()
    expect(() => assertNumberGt([], 1)).toThrow()
    expect(() => assertNumberGt(false, 1)).toThrow()
  })
  test(`assertNumberGte`, () => {
    expect(() => assertNumberGte(0, 1)).toThrow()
    expect(() => assertNumberGte(0.5, 1)).toThrow()
    expect(() => assertNumberGte(1, 1)).not.toThrow()
    expect(() => assertNumberGte(1.5, 1)).not.toThrow()
    expect(() => assertNumberGte(2, 1)).not.toThrow()
    expect(() => assertNumberGte(`2`, 1)).toThrow()
    expect(() => assertNumberGte([], 1)).toThrow()
    expect(() => assertNumberGte(false, 1)).toThrow()
  })
  test(`assertNumberLt`, () => {
    expect(() => assertNumberLt(0, 1)).not.toThrow()
    expect(() => assertNumberLt(0.5, 1)).not.toThrow()
    expect(() => assertNumberLt(1, 1)).toThrow()
    expect(() => assertNumberLt(1.5, 1)).toThrow()
    expect(() => assertNumberLt(2, 1)).toThrow()
    expect(() => assertNumberLt(`2`, 1)).toThrow()
    expect(() => assertNumberLt([], 1)).toThrow()
    expect(() => assertNumberLt(false, 1)).toThrow()
  })
  test(`assertNumberLte`, () => {
    expect(() => assertNumberLte(0, 1)).not.toThrow()
    expect(() => assertNumberLte(0.5, 1)).not.toThrow()
    expect(() => assertNumberLte(1, 1)).not.toThrow()
    expect(() => assertNumberLte(1.5, 1)).toThrow()
    expect(() => assertNumberLte(2, 1)).toThrow()
    expect(() => assertNumberLte(`2`, 1)).toThrow()
    expect(() => assertNumberLte([], 1)).toThrow()
    expect(() => assertNumberLte(false, 1)).toThrow()
  })
  test(`assertNumberNotZero`, () => {
    expect(() => assertNumberNotZero(-1)).not.toThrow()
    expect(() => assertNumberNotZero(-0.5)).not.toThrow()
    expect(() => assertNumberNotZero(0)).toThrow()
    expect(() => assertNumberNotZero(0.5)).not.toThrow()
    expect(() => assertNumberNotZero(1)).not.toThrow()
    expect(() => assertNumberNotZero(`1`)).toThrow()
    expect(() => assertNumberNotZero([])).toThrow()
    expect(() => assertNumberNotZero({})).toThrow()
    expect(() => assertNumberNotZero(true)).toThrow()
    expect(() => assertNumberNotZero(false)).toThrow()
    expect(() => assertNumberNotZero(null)).toThrow()
    expect(() => assertNumberNotZero(undefined)).toThrow()
  })
  test(`assertString`, () => {
    expect(() => assertString(``)).not.toThrow()
    expect(() => assertString(`1`)).not.toThrow()
    expect(() => assertString(0)).toThrow()
    expect(() => assertString(1)).toThrow()
    expect(() => assertString(true)).toThrow()
    expect(() => assertString(false)).toThrow()
    expect(() => assertString(null)).toThrow()
    expect(() => assertString(undefined)).toThrow()
    expect(() => assertString([])).toThrow()
    expect(() => assertString({})).toThrow()
  })

  test(`assertStringOrArray`, () => {
    expect(() => assertSeq(``)).not.toThrow()
    expect(() => assertSeq(`1`)).not.toThrow()
    expect(() => assertSeq([])).not.toThrow()
    expect(() => assertSeq([1, 2, 3])).not.toThrow()
    expect(() => assertSeq(0)).toThrow()
    expect(() => assertSeq(1)).toThrow()
    expect(() => assertSeq(true)).toThrow()
    expect(() => assertSeq(false)).toThrow()
    expect(() => assertSeq(null)).toThrow()
    expect(() => assertSeq(undefined)).toThrow()
    expect(() => assertSeq({})).toThrow()
  })

  test(`assertStringOrRegExp`, () => {
    expect(() => assertStringOrRegExp(``)).not.toThrow()
    expect(() => assertStringOrRegExp(`1`)).not.toThrow()
    expect(() => assertStringOrRegExp(/^a/)).not.toThrow()
    expect(() => assertStringOrRegExp([])).toThrow()
    expect(() => assertStringOrRegExp([1, 2, 3])).toThrow()
    expect(() => assertStringOrRegExp(0)).toThrow()
    expect(() => assertStringOrRegExp(1)).toThrow()
    expect(() => assertStringOrRegExp(true)).toThrow()
    expect(() => assertStringOrRegExp(false)).toThrow()
    expect(() => assertStringOrRegExp(null)).toThrow()
    expect(() => assertStringOrRegExp(undefined)).toThrow()
    expect(() => assertStringOrRegExp({})).toThrow()
  })

  test(`isLispishFunction`, () => {
    const lf1: LispishFunction = {
      [functionSymbol]: true,
      arguments: {
        mandatoryArguments: [],
        optionalArguments: [],
      },
      functionContext: {},
      name: undefined,
      body: [],
    }
    const lf2: LispishFunction = {
      [functionSymbol]: true,
      builtin: `+`,
    }
    expect(isLispishFunction(lf1)).toBe(true)
    expect(isLispishFunction(lf2)).toBe(true)
    expect(isLispishFunction(``)).toBe(false)
    expect(isLispishFunction(`1`)).toBe(false)
    expect(isLispishFunction(0)).toBe(false)
    expect(isLispishFunction(1)).toBe(false)
    expect(isLispishFunction(true)).toBe(false)
    expect(isLispishFunction(false)).toBe(false)
    expect(isLispishFunction(null)).toBe(false)
    expect(isLispishFunction(undefined)).toBe(false)
    expect(isLispishFunction([])).toBe(false)
    expect(isLispishFunction({})).toBe(false)
  })

  test(`isUserDefinedLispishFunction`, () => {
    const lf1: LispishFunction = {
      [functionSymbol]: true,
      arguments: {
        mandatoryArguments: [],
        optionalArguments: [],
      },
      functionContext: {},
      name: undefined,
      body: [],
    }
    const lf2: LispishFunction = {
      [functionSymbol]: true,
      builtin: `+`,
    }
    expect(isUserDefinedLispishFunction(lf1)).toBe(true)
    expect(isUserDefinedLispishFunction(lf2)).toBe(false)
    expect(isUserDefinedLispishFunction(``)).toBe(false)
    expect(isUserDefinedLispishFunction(`1`)).toBe(false)
    expect(isUserDefinedLispishFunction(0)).toBe(false)
    expect(isUserDefinedLispishFunction(1)).toBe(false)
    expect(isUserDefinedLispishFunction(true)).toBe(false)
    expect(isUserDefinedLispishFunction(false)).toBe(false)
    expect(isUserDefinedLispishFunction(null)).toBe(false)
    expect(isUserDefinedLispishFunction(undefined)).toBe(false)
    expect(isUserDefinedLispishFunction([])).toBe(false)
    expect(isUserDefinedLispishFunction({})).toBe(false)
  })

  test(`isBuiltinLispishFunction`, () => {
    const lf1: LispishFunction = {
      [functionSymbol]: true,
      arguments: {
        mandatoryArguments: [],
        optionalArguments: [],
      },
      functionContext: {},
      name: undefined,
      body: [],
    }
    const lf2: LispishFunction = {
      [functionSymbol]: true,
      builtin: `+`,
    }
    expect(isBuiltinLispishFunction(lf1)).toBe(false)
    expect(isBuiltinLispishFunction(lf2)).toBe(true)
    expect(isBuiltinLispishFunction(``)).toBe(false)
    expect(isBuiltinLispishFunction(`1`)).toBe(false)
    expect(isBuiltinLispishFunction(0)).toBe(false)
    expect(isBuiltinLispishFunction(1)).toBe(false)
    expect(isBuiltinLispishFunction(true)).toBe(false)
    expect(isBuiltinLispishFunction(false)).toBe(false)
    expect(isBuiltinLispishFunction(null)).toBe(false)
    expect(isBuiltinLispishFunction(undefined)).toBe(false)
    expect(isBuiltinLispishFunction([])).toBe(false)
    expect(isBuiltinLispishFunction({})).toBe(false)
  })
  test(`assertStringArray`, () => {
    expect(() => assertStringArray(undefined)).toThrow()
    expect(() => assertStringArray(`undefined`)).toThrow()
    expect(() => assertStringArray([])).not.toThrow()
    expect(() => assertStringArray([`1`, `2`])).not.toThrow()
    expect(() => assertStringArray([`1`, `2`, 3])).toThrow()
  })

  test(`isNumber`, () => {
    expect(isNumber(1 / 0)).toBe(true)
    expect(isNumber(Number(`abc`))).toBe(true)
    expect(isNumber(0.12)).toBe(true)
    expect(isNumber(undefined)).toBe(false)
    expect(isNumber(`undefined`)).toBe(false)
    expect(isNumber([])).toBe(false)
  })

  test(`isInteger`, () => {
    expect(isInteger(1 / 0)).toBe(false)
    expect(isInteger(Number(`abc`))).toBe(false)
    expect(isInteger(0.12)).toBe(false)
    expect(isInteger(-12)).toBe(true)
    expect(isInteger(0)).toBe(true)
    expect(isInteger(12)).toBe(true)
    expect(isInteger(undefined)).toBe(false)
    expect(isInteger(`undefined`)).toBe(false)
    expect(isInteger([])).toBe(false)
  })

  test(`hasKey`, () => {
    expect(hasKey(`Albert`, 1)).toBe(true)
    expect(hasKey(`Albert`, -1)).toBe(false)
    expect(hasKey(`Albert`, 1.2)).toBe(false)
    expect(hasKey(`Albert`, 6)).toBe(false)
    expect(hasKey(``, 0)).toBe(false)

    expect(hasKey([1, 2, 3], 1)).toBe(true)
    expect(hasKey([1, 2, 3], 6)).toBe(false)
    expect(hasKey([], 0)).toBe(false)

    expect(hasKey({ a: 1, b: 2 }, `a`)).toBe(true)
    expect(hasKey({ a: 1, b: 2 }, `b`)).toBe(true)
    expect(hasKey({ a: 1, b: 2 }, `c`)).toBe(false)
    expect(hasKey({}, 0)).toBe(false)
    expect(hasKey({}, `a`)).toBe(false)
  })

  test(`assertNumber`, () => {
    expect(() => assertNumber(1 / 0)).not.toThrow()
    expect(() => assertNumber(Number(`abc`))).not.toThrow()
    expect(() => assertNumber(0.12)).not.toThrow()
    expect(() => assertNumber(undefined)).toThrow()
    expect(() => assertNumber(`undefined`)).toThrow()
    expect(() => assertNumber([])).toThrow()
  })

  test(`isRegexp`, () => {
    expect(isRegExp(`Hej`)).toBe(false)
    expect(isRegExp({})).toBe(false)
    expect(isRegExp(/^a/)).toBe(true)
  })
})
