import { AstNode, FUNCTION_SYMBOL, LispishFunction, NameNode, NormalExpressionNode } from '../src/parser/interface'
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
  isLispishFunction,
  asFiniteNumber,
  assertLength,
  assertSeq,
  assertStringOrRegExp,
  isNumber,
  assertNumber,
  isInteger,
  collHasKey,
  isRegExp,
  isNormalExpressionNodeName,
  deepEqual,
  assertNonEmptyString,
  asAny,
  asString,
  assertAny,
  assertNotUndefined,
  toNonNegativeInteger,
  assertMax,
  assertChar,
  asChar,
  asColl,
  cloneColl,
} from '../src/utils'
describe(`utils`, () => {
  test(`asAstNode`, () => {
    expect(() => asAstNode(undefined)).toThrow()
    const node: AstNode = { type: `Name`, value: `test` }

    expect(asAstNode(node)).toBe(node)
  })
  test(`asAny`, () => {
    expect(() => asAny(undefined)).toThrow()
    expect(() => asAny(undefined, `An error message`)).toThrow()
    const node: AstNode = { type: `Name`, value: `test` }

    expect(asAny(node)).toBe(node)
  })
  test(`assertAny`, () => {
    expect(() => assertAny(undefined)).toThrow()
    const node: AstNode = { type: `Name`, value: `test` }

    expect(() => assertAny(node)).not.toThrow()
  })
  test(`assertAny`, () => {
    expect(() => assertAny(undefined)).toThrow()
    const node: AstNode = { type: `Name`, value: `test` }

    expect(() => assertAny(node)).not.toThrow()
  })
  test(`asLispishFunction`, () => {
    expect(() => asLispishFunction(undefined)).toThrow()
    const lf: LispishFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
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
    expect(asNotUndefined(null)).toBe(null)
    expect(asNotUndefined(false)).toBe(false)
    expect(asNotUndefined(true)).toBe(true)
    expect(asNotUndefined(0)).toBe(0)
    const obj = {}
    expect(asNotUndefined(obj)).toBe(obj)
  })
  test(`assertNotUndefined`, () => {
    expect(() => assertNotUndefined(undefined)).toThrow()
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
    expect(() => assertObj({ [FUNCTION_SYMBOL]: true })).toThrow()
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
    expect(() => assertObjectOrArray({ [FUNCTION_SYMBOL]: true })).toThrow()
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
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
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
  test(`asString`, () => {
    expect(() => asString(``)).not.toThrow()
    expect(() => asString(`1`)).not.toThrow()
    expect(() => asString(0)).toThrow()
    expect(() => asString(1)).toThrow()
    expect(() => asString(true)).toThrow()
    expect(() => asString(false)).toThrow()
    expect(() => asString(null)).toThrow()
    expect(() => asString(undefined)).toThrow()
    expect(() => asString([])).toThrow()
    expect(() => asString({})).toThrow()
  })
  test(`assertNonEmptyString`, () => {
    expect(() => assertNonEmptyString(`1`)).not.toThrow()
    expect(() => assertNonEmptyString(`abc`)).not.toThrow()
    expect(() => assertNonEmptyString(``)).toThrow()
    expect(() => assertNonEmptyString(0)).toThrow()
    expect(() => assertNonEmptyString(1)).toThrow()
    expect(() => assertNonEmptyString(true)).toThrow()
    expect(() => assertNonEmptyString(false)).toThrow()
    expect(() => assertNonEmptyString(null)).toThrow()
    expect(() => assertNonEmptyString(undefined)).toThrow()
    expect(() => assertNonEmptyString([])).toThrow()
    expect(() => assertNonEmptyString({})).toThrow()
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
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
      arguments: {
        mandatoryArguments: [],
        optionalArguments: [],
      },
      functionContext: {},
      name: undefined,
      body: [],
    }
    const lf2: LispishFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `builtin`,
      name: `+`,
    }
    const lf3: LispishFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `partial`,
      fn: { a: 10, b: 20 },
      params: [],
    }
    const lf4: LispishFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `comp`,
      fns: [`x`],
    }
    const lf5: LispishFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `constantly`,
      value: 10,
    }
    expect(isLispishFunction(lf1)).toBe(true)
    expect(isLispishFunction(lf2)).toBe(true)
    expect(isLispishFunction(lf3)).toBe(true)
    expect(isLispishFunction(lf4)).toBe(true)
    expect(isLispishFunction(lf5)).toBe(true)
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

  test(`collHasKey`, () => {
    expect(collHasKey(10, 1)).toBe(false)

    expect(collHasKey(`Albert`, 1)).toBe(true)
    expect(collHasKey(`Albert`, -1)).toBe(false)
    expect(collHasKey(`Albert`, 1.2)).toBe(false)
    expect(collHasKey(`Albert`, 6)).toBe(false)
    expect(collHasKey(``, 0)).toBe(false)

    expect(collHasKey([1, 2, 3], 1)).toBe(true)
    expect(collHasKey([1, 2, 3], 6)).toBe(false)
    expect(collHasKey([], 0)).toBe(false)

    expect(collHasKey({ a: 1, b: 2 }, `a`)).toBe(true)
    expect(collHasKey({ a: 1, b: 2 }, `b`)).toBe(true)
    expect(collHasKey({ a: 1, b: 2 }, `c`)).toBe(false)
    expect(collHasKey({}, 0)).toBe(false)
    expect(collHasKey({}, `a`)).toBe(false)
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

  test(`isNormalExpressionNodeName`, () => {
    expect(isNormalExpressionNodeName({ type: `NormalExpression`, params: [], name: `object` })).toBe(true)
    expect(
      isNormalExpressionNodeName({
        type: `NormalExpression`,
        params: [],
        expression: { type: `NormalExpression`, name: `+`, params: [{ type: `Number`, value: 2 }] },
      }),
    ).toBe(false)
  })

  const primitives = [0, 1, true, false, null, `Albert`, `Mojir`]
  describe(`deepEqual`, () => {
    test(`primitives`, () => {
      for (const a of primitives) {
        for (const b of primitives) {
          expect(deepEqual(a, b)).toBe(a === b)
        }
      }
    })
    test(`RegExp`, () => {
      expect(deepEqual(/^ab/, /^ab/)).toBe(true)
      expect(deepEqual(/^ab/, new RegExp(`^ab`))).toBe(true)
      expect(deepEqual(/^ab/gi, new RegExp(`^ab`, `ig`))).toBe(true)
      expect(deepEqual(/^ab/g, /^ab/)).toBe(false)
      expect(deepEqual(/ab/, /^ab/)).toBe(false)
    })
    test(`nested structures`, () => {
      expect(deepEqual([1, 2, 3], [1, 2, 3])).toBe(true)
      expect(deepEqual({ a: 1, b: 2 }, { a: 1, b: 2 })).toBe(true)
      expect(deepEqual([1, 2, { a: 1, b: 2 }], [1, 2, { b: 2, a: 1 }])).toBe(true)
      expect(deepEqual(/^ab/, new RegExp(`^ab`))).toBe(true)
      expect(deepEqual(/^ab/gi, new RegExp(`^ab`, `ig`))).toBe(true)
      expect(deepEqual(/^ab/g, /^ab/)).toBe(false)
      expect(deepEqual(/ab/, /^ab/)).toBe(false)
    })
  })
  test(`toNonNegativeInteger`, () => {
    expect(toNonNegativeInteger(0)).toBe(0)
    expect(toNonNegativeInteger(-0.1)).toBe(0)
    expect(toNonNegativeInteger(-100)).toBe(0)
    expect(toNonNegativeInteger(0.01)).toBe(1)
    expect(toNonNegativeInteger(2.01)).toBe(3)
    expect(toNonNegativeInteger(4.0)).toBe(4)
  })
  test(`assertMax`, () => {
    expect(() => assertMax(12, 10)).toThrow()
    expect(() => assertMax(-12, -10)).not.toThrow()
    expect(() => assertMax(-8, -10)).toThrow()
    expect(() => assertMax(10, 10)).not.toThrow()
    expect(() => assertMax(0, 10)).not.toThrow()
  })
  test(`assertChar`, () => {
    expect(() => assertChar(`2`)).not.toThrow()
    expect(() => assertChar(`Albert`)).toThrow()
    expect(() => assertChar(0)).toThrow()
    expect(() => assertChar(null)).toThrow()
    expect(() => assertChar(true)).toThrow()
    expect(() => assertChar(false)).toThrow()
    expect(() => assertChar([`a`])).toThrow()
    expect(() => assertChar({ a: `a` })).toThrow()
  })
  test(`asChar`, () => {
    expect(asChar(`2`)).toBe(`2`)
    expect(() => asChar(`Albert`)).toThrow()
    expect(() => asChar(0)).toThrow()
    expect(() => asChar(null)).toThrow()
    expect(() => asChar(true)).toThrow()
    expect(() => asChar(false)).toThrow()
    expect(() => asChar([`a`])).toThrow()
    expect(() => asChar({ a: `a` })).toThrow()
  })

  test(`asColl`, () => {
    expect(asColl(`2`)).toEqual(`2`)
    expect(asColl({ a: 1 })).toEqual({ a: 1 })
    expect(asColl([2])).toEqual([2])
    expect(() => asColl(0)).toThrow()
    expect(() => asColl(null)).toThrow()
    expect(() => asColl(true)).toThrow()
    expect(() => asColl(false)).toThrow()
  })

  describe(`cloneColl`, () => {
    test(`samples`, () => {
      expect(cloneColl({ a: 10 })).toEqual({ a: 10 })
      expect(cloneColl({ a: [1, 2, 3] })).toEqual({ a: [1, 2, 3] })
    })
    test(`new instance`, () => {
      const original = { a: [1, 2, 3] }
      const second = cloneColl(original)
      expect(original).not.toBe(second)
      second.a[0] = 10
      expect(original.a[0]).toBe(1)
    })
  })
})
