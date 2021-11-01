import { AstNode, FUNCTION_SYMBOL, LitsFunction, NameNode, NormalExpressionNode } from '../src/parser/interface'
import { TokenMeta } from '../src/tokenizer/interface'
import {
  asAstNode,
  asLitsFunction,
  asNameNode,
  asNotUndefined,
  assertArr,
  assertInteger,
  assertLengthEven,
  assertLitsFunction,
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
  assertPositiveNumber,
  assertRegExp,
  assertString,
  asNonEmptyString,
  isLitsFunction,
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
  assertAny,
  assertNotUndefined,
  toNonNegativeInteger,
  assertMax,
  assertChar,
  asChar,
  asColl,
  cloneColl,
  asString,
} from '../src/utils'

const meta: TokenMeta = `EOF`
describe(`utils`, () => {
  test(`asAstNode`, () => {
    expect(() => asAstNode(undefined, meta)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }

    expect(asAstNode(node, meta)).toBe(node)
  })
  test(`asAny`, () => {
    expect(() => asAny(undefined, meta)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }

    expect(asAny(node, meta)).toBe(node)
  })
  test(`assertAny`, () => {
    expect(() => assertAny(undefined, meta)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }

    expect(() => assertAny(node, meta)).not.toThrow()
  })
  test(`assertAny`, () => {
    expect(() => assertAny(undefined, meta)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }

    expect(() => assertAny(node, meta)).not.toThrow()
  })
  test(`asLitsFunction`, () => {
    expect(() => asLitsFunction(undefined, meta)).toThrow()
    const lf: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
      name: undefined,
      overloads: [
        {
          arguments: {
            mandatoryArguments: [],
          },
          functionContext: {},
          body: [],
          arity: 0,
        },
      ],
    }
    expect(asLitsFunction(lf, meta)).toBe(lf)
  })
  test(`asNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => asNameNode(undefined, {} as any)).toThrow()
    expect(() =>
      asNameNode(
        { type: `Number`, value: 12, token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` } },
        { line: 0, column: 0 },
      ),
    ).toThrow()
    const nameNode: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }
    expect(asNameNode(nameNode, nameNode.token.meta)).toBe(nameNode)
  })
  test(`assertNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => assertNameNode(undefined, {} as any)).toThrow()
    const nameNode: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }
    asNameNode(nameNode, nameNode.token.meta)
  })
  test(`asNotUndefined`, () => {
    expect(() => asNotUndefined(undefined, `EOF`)).toThrow()
    expect(asNotUndefined(null, `EOF`)).toBe(null)
    expect(asNotUndefined(false, `EOF`)).toBe(false)
    expect(asNotUndefined(true, `EOF`)).toBe(true)
    expect(asNotUndefined(0, `EOF`)).toBe(0)
    const obj = {}
    expect(asNotUndefined(obj, `EOF`)).toBe(obj)
  })
  test(`assertNotUndefined`, () => {
    expect(() => assertNotUndefined(undefined, `EOF`)).toThrow()
    expect(() => assertNotUndefined(undefined, `EOF`)).toThrow()
    expect(() => assertNotUndefined(null, `EOF`)).not.toThrow()
    expect(() => assertNotUndefined(false, `EOF`)).not.toThrow()
    expect(() => assertNotUndefined(true, `EOF`)).not.toThrow()
    expect(() => assertNotUndefined(0, `EOF`)).not.toThrow()
    expect(() => assertNotUndefined({}, `EOF`)).not.toThrow()
  })
  test(`asNonEmptyString`, () => {
    expect(asNonEmptyString(`1`, meta)).toBe(`1`)
    expect(() => asNonEmptyString(``, meta)).toThrow()
    expect(() => asNonEmptyString(0, meta)).toThrow()
    expect(() => asNonEmptyString(1, meta)).toThrow()
    expect(() => asNonEmptyString(true, meta)).toThrow()
    expect(() => asNonEmptyString(false, meta)).toThrow()
    expect(() => asNonEmptyString(null, meta)).toThrow()
    expect(() => asNonEmptyString(undefined, meta)).toThrow()
    expect(() => asNonEmptyString([], meta)).toThrow()
    expect(() => asNonEmptyString({}, meta)).toThrow()
  })

  test(`assertArr`, () => {
    expect(() => assertArr(0, meta)).toThrow()
    expect(() => assertArr({}, meta)).toThrow()
    expect(() => assertArr([], meta)).not.toThrow()
    expect(() => assertArr([1], meta)).not.toThrow()
    expect(() => assertArr(true, meta)).toThrow()
    expect(() => assertArr(null, meta)).toThrow()
    expect(() => assertArr(undefined, meta)).toThrow()
  })
  test(`assertObj`, () => {
    expect(() => assertObj(0, meta)).toThrow()
    expect(() => assertObj({}, meta)).not.toThrow()
    expect(() => assertObj({ [FUNCTION_SYMBOL]: true }, meta)).toThrow()
    expect(() => assertObj({ a: 1 }, meta)).not.toThrow()
    expect(() => assertObj(/test/, meta)).toThrow()
    expect(() => assertObj([], meta)).toThrow()
    expect(() => assertObj([1], meta)).toThrow()
    expect(() => assertObj(true, meta)).toThrow()
    expect(() => assertObj(null, meta)).toThrow()
    expect(() => assertObj(undefined, meta)).toThrow()
  })
  test(`assertInteger`, () => {
    expect(() => assertInteger(-0, meta)).not.toThrow()
    expect(() => assertInteger(-1, meta)).not.toThrow()
    expect(() => assertInteger(1, meta)).not.toThrow()
    expect(() => assertInteger(-0.1, meta)).toThrow()
    expect(() => assertInteger(1.00001, meta)).toThrow()
    expect(() => assertInteger(`k`, meta)).toThrow()
    expect(() => assertInteger(false, meta)).toThrow()
    expect(() => assertInteger(undefined, meta)).toThrow()
    expect(() => assertInteger(null, meta)).toThrow()
    expect(() => assertInteger([], meta)).toThrow()
  })
  test(`assertRegExp`, () => {
    expect(() => assertRegExp(/a/, meta)).not.toThrow()
    expect(() => assertRegExp(new RegExp(`a`), meta)).not.toThrow()
    expect(() => assertRegExp(0, meta)).toThrow()
    expect(() => assertRegExp(`0`, meta)).toThrow()
    expect(() => assertRegExp(null, meta)).toThrow()
    expect(() => assertRegExp(undefined, meta)).toThrow()
    expect(() => assertRegExp(false, meta)).toThrow()
    expect(() => assertRegExp(true, meta)).toThrow()
    expect(() => assertRegExp([], meta)).toThrow()
    expect(() => assertRegExp({}, meta)).toThrow()
  })

  function node(arr: number[]): NormalExpressionNode {
    const astNodes: AstNode[] = arr.map(n => ({
      type: `Number`,
      value: n,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
    }))
    return {
      name: `let`,
      params: astNodes,
      type: `NormalExpression`,
      token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
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

  test(`assertLitsFunction`, () => {
    const lf: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
      name: undefined,
      overloads: [
        {
          arguments: {
            mandatoryArguments: [],
          },
          functionContext: {},
          body: [],
          arity: 0,
        },
      ],
    }
    expect(() => assertLitsFunction(lf, meta)).not.toThrow()
    expect(() => assertLitsFunction(1, meta)).toThrow()
    expect(() => assertLitsFunction({}, meta)).toThrow()
  })
  test(`assertPositiveNumber`, () => {
    expect(() => assertPositiveNumber(-1, meta)).toThrow()
    expect(() => assertPositiveNumber(-0.5, meta)).toThrow()
    expect(() => assertPositiveNumber(0, meta)).toThrow()
    expect(() => assertPositiveNumber(0.5, meta)).not.toThrow()
    expect(() => assertPositiveNumber(1, meta)).not.toThrow()
    expect(() => assertPositiveNumber(`1`, meta)).toThrow()
    expect(() => assertPositiveNumber([], meta)).toThrow()
    expect(() => assertPositiveNumber({}, meta)).toThrow()
    expect(() => assertPositiveNumber(true, meta)).toThrow()
    expect(() => assertPositiveNumber(false, meta)).toThrow()
    expect(() => assertPositiveNumber(null, meta)).toThrow()
    expect(() => assertPositiveNumber(undefined, meta)).toThrow()
  })
  test(`assertNegativeNumber`, () => {
    expect(() => assertNegativeNumber(-1, meta)).not.toThrow()
    expect(() => assertNegativeNumber(-0.5, meta)).not.toThrow()
    expect(() => assertNegativeNumber(0, meta)).toThrow()
    expect(() => assertNegativeNumber(0.5, meta)).toThrow()
    expect(() => assertNegativeNumber(1, meta)).toThrow()
    expect(() => assertNegativeNumber(`1`, meta)).toThrow()
    expect(() => assertNegativeNumber([], meta)).toThrow()
    expect(() => assertNegativeNumber({}, meta)).toThrow()
    expect(() => assertNegativeNumber(true, meta)).toThrow()
    expect(() => assertNegativeNumber(false, meta)).toThrow()
    expect(() => assertNegativeNumber(null, meta)).toThrow()
    expect(() => assertNegativeNumber(undefined, meta)).toThrow()
  })
  test(`assertNonNegativeNumber`, () => {
    expect(() => assertNonNegativeNumber(-1, meta)).toThrow()
    expect(() => assertNonNegativeNumber(-1.1, meta)).toThrow()
    expect(() => assertNonNegativeNumber(0, meta)).not.toThrow()
    expect(() => assertNonNegativeNumber(0.1, meta)).not.toThrow()
    expect(() => assertNonNegativeNumber(1, meta)).not.toThrow()
    expect(() => assertNonNegativeNumber(1.1, meta)).not.toThrow()
    expect(() => assertNonNegativeNumber(`1`, meta)).toThrow()
    expect(() => assertNonNegativeNumber([], meta)).toThrow()
    expect(() => assertNonNegativeNumber({}, meta)).toThrow()
    expect(() => assertNonNegativeNumber(true, meta)).toThrow()
    expect(() => assertNonNegativeNumber(false, meta)).toThrow()
    expect(() => assertNonNegativeNumber(null, meta)).toThrow()
    expect(() => assertNonNegativeNumber(undefined, meta)).toThrow()
  })
  test(`assertNonPositiveNumber`, () => {
    expect(() => assertNonPositiveNumber(-1, meta)).not.toThrow()
    expect(() => assertNonPositiveNumber(-1.1, meta)).not.toThrow()
    expect(() => assertNonPositiveNumber(0, meta)).not.toThrow()
    expect(() => assertNonPositiveNumber(0.1, meta)).toThrow()
    expect(() => assertNonPositiveNumber(1, meta)).toThrow()
    expect(() => assertNonPositiveNumber(1.1, meta)).toThrow()
    expect(() => assertNonPositiveNumber(`1`, meta)).toThrow()
    expect(() => assertNonPositiveNumber([], meta)).toThrow()
    expect(() => assertNonPositiveNumber({}, meta)).toThrow()
    expect(() => assertNonPositiveNumber(true, meta)).toThrow()
    expect(() => assertNonPositiveNumber(false, meta)).toThrow()
    expect(() => assertNonPositiveNumber(null, meta)).toThrow()
    expect(() => assertNonPositiveNumber(undefined, meta)).toThrow()
  })
  test(`assertFiniteNumber`, () => {
    expect(() => assertFiniteNumber(-1, meta)).not.toThrow()
    expect(() => assertFiniteNumber(-1.1, meta)).not.toThrow()
    expect(() => assertFiniteNumber(0, meta)).not.toThrow()
    expect(() => assertFiniteNumber(0.1, meta)).not.toThrow()
    expect(() => assertFiniteNumber(1, meta)).not.toThrow()
    expect(() => assertFiniteNumber(1.1, meta)).not.toThrow()
    expect(() => assertFiniteNumber(Math.asin(2), meta)).toThrow()
    expect(() => assertFiniteNumber(1 / 0, meta)).toThrow()
    expect(() => assertFiniteNumber(`1`, meta)).toThrow()
    expect(() => assertFiniteNumber([], meta)).toThrow()
    expect(() => assertFiniteNumber({}, meta)).toThrow()
    expect(() => assertFiniteNumber(true, meta)).toThrow()
    expect(() => assertFiniteNumber(false, meta)).toThrow()
    expect(() => assertFiniteNumber(null, meta)).toThrow()
    expect(() => assertFiniteNumber(undefined, meta)).toThrow()
  })
  test(`asFiniteNumber`, () => {
    expect(asFiniteNumber(-1, meta)).toBe(-1)
    expect(asFiniteNumber(-1.1, meta)).toBe(-1.1)
    expect(asFiniteNumber(0, meta)).toBe(0)
    expect(asFiniteNumber(0.1, meta)).toBe(0.1)
    expect(asFiniteNumber(1, meta)).toBe(1)
    expect(asFiniteNumber(1.1, meta)).toBe(1.1)
    expect(() => asFiniteNumber(Math.asin(2), meta)).toThrow()
    expect(() => asFiniteNumber(1 / 0, meta)).toThrow()
    expect(() => asFiniteNumber(`1`, meta)).toThrow()
    expect(() => asFiniteNumber(`1`, meta)).toThrow()
    expect(() => asFiniteNumber([], meta)).toThrow()
    expect(() => asFiniteNumber({}, meta)).toThrow()
    expect(() => asFiniteNumber(true, meta)).toThrow()
    expect(() => asFiniteNumber(false, meta)).toThrow()
    expect(() => asFiniteNumber(null, meta)).toThrow()
    expect(() => asFiniteNumber(undefined, meta)).toThrow()
  })
  test(`assertNumberGt`, () => {
    expect(() => assertNumberGt(0, 1, meta)).toThrow()
    expect(() => assertNumberGt(0.5, 1, meta)).toThrow()
    expect(() => assertNumberGt(1, 1, meta)).toThrow()
    expect(() => assertNumberGt(1.5, 1, meta)).not.toThrow()
    expect(() => assertNumberGt(2, 1, meta)).not.toThrow()
    expect(() => assertNumberGt(`2`, 1, meta)).toThrow()
    expect(() => assertNumberGt([], 1, meta)).toThrow()
    expect(() => assertNumberGt(false, 1, meta)).toThrow()
  })
  test(`assertNumberGte`, () => {
    expect(() => assertNumberGte(0, 1, meta)).toThrow()
    expect(() => assertNumberGte(0.5, 1, meta)).toThrow()
    expect(() => assertNumberGte(1, 1, meta)).not.toThrow()
    expect(() => assertNumberGte(1.5, 1, meta)).not.toThrow()
    expect(() => assertNumberGte(2, 1, meta)).not.toThrow()
    expect(() => assertNumberGte(`2`, 1, meta)).toThrow()
    expect(() => assertNumberGte([], 1, meta)).toThrow()
    expect(() => assertNumberGte(false, 1, meta)).toThrow()
  })
  test(`assertNumberLt`, () => {
    expect(() => assertNumberLt(0, 1, meta)).not.toThrow()
    expect(() => assertNumberLt(0.5, 1, meta)).not.toThrow()
    expect(() => assertNumberLt(1, 1, meta)).toThrow()
    expect(() => assertNumberLt(1.5, 1, meta)).toThrow()
    expect(() => assertNumberLt(2, 1, meta)).toThrow()
    expect(() => assertNumberLt(`2`, 1, meta)).toThrow()
    expect(() => assertNumberLt([], 1, meta)).toThrow()
    expect(() => assertNumberLt(false, 1, meta)).toThrow()
  })
  test(`assertNumberLte`, () => {
    expect(() => assertNumberLte(0, 1, meta)).not.toThrow()
    expect(() => assertNumberLte(0.5, 1, meta)).not.toThrow()
    expect(() => assertNumberLte(1, 1, meta)).not.toThrow()
    expect(() => assertNumberLte(1.5, 1, meta)).toThrow()
    expect(() => assertNumberLte(2, 1, meta)).toThrow()
    expect(() => assertNumberLte(`2`, 1, meta)).toThrow()
    expect(() => assertNumberLte([], 1, meta)).toThrow()
    expect(() => assertNumberLte(false, 1, meta)).toThrow()
  })
  test(`assertNumberNotZero`, () => {
    expect(() => assertNumberNotZero(-1, meta)).not.toThrow()
    expect(() => assertNumberNotZero(-0.5, meta)).not.toThrow()
    expect(() => assertNumberNotZero(0, meta)).toThrow()
    expect(() => assertNumberNotZero(0.5, meta)).not.toThrow()
    expect(() => assertNumberNotZero(1, meta)).not.toThrow()
    expect(() => assertNumberNotZero(`1`, meta)).toThrow()
    expect(() => assertNumberNotZero([], meta)).toThrow()
    expect(() => assertNumberNotZero({}, meta)).toThrow()
    expect(() => assertNumberNotZero(true, meta)).toThrow()
    expect(() => assertNumberNotZero(false, meta)).toThrow()
    expect(() => assertNumberNotZero(null, meta)).toThrow()
    expect(() => assertNumberNotZero(undefined, meta)).toThrow()
  })
  test(`assertString`, () => {
    expect(() => assertString(``, meta)).not.toThrow()
    expect(() => assertString(`1`, meta)).not.toThrow()
    expect(() => assertString(0, meta)).toThrow()
    expect(() => assertString(1, meta)).toThrow()
    expect(() => assertString(true, meta)).toThrow()
    expect(() => assertString(false, meta)).toThrow()
    expect(() => assertString(null, meta)).toThrow()
    expect(() => assertString(undefined, meta)).toThrow()
    expect(() => assertString([], meta)).toThrow()
    expect(() => assertString({}, meta)).toThrow()
  })
  test(`asString`, () => {
    expect(() => asString(``, meta)).not.toThrow()
    expect(() => asString(`1`, meta)).not.toThrow()
    expect(() => asString(0, meta)).toThrow()
    expect(() => asString(1, meta)).toThrow()
    expect(() => asString(true, meta)).toThrow()
    expect(() => asString(false, meta)).toThrow()
    expect(() => asString(null, meta)).toThrow()
    expect(() => asString(undefined, meta)).toThrow()
    expect(() => asString([], meta)).toThrow()
    expect(() => asString({}, meta)).toThrow()
  })
  test(`assertNonEmptyString`, () => {
    expect(() => assertNonEmptyString(`1`, meta)).not.toThrow()
    expect(() => assertNonEmptyString(`abc`, meta)).not.toThrow()
    expect(() => assertNonEmptyString(``, meta)).toThrow()
    expect(() => assertNonEmptyString(0, meta)).toThrow()
    expect(() => assertNonEmptyString(1, meta)).toThrow()
    expect(() => assertNonEmptyString(true, meta)).toThrow()
    expect(() => assertNonEmptyString(false, meta)).toThrow()
    expect(() => assertNonEmptyString(null, meta)).toThrow()
    expect(() => assertNonEmptyString(undefined, meta)).toThrow()
    expect(() => assertNonEmptyString([], meta)).toThrow()
    expect(() => assertNonEmptyString({}, meta)).toThrow()
  })

  test(`assertStringOrArray`, () => {
    expect(() => assertSeq(``, meta)).not.toThrow()
    expect(() => assertSeq(`1`, meta)).not.toThrow()
    expect(() => assertSeq([], meta)).not.toThrow()
    expect(() => assertSeq([1, 2, 3], meta)).not.toThrow()
    expect(() => assertSeq(0, meta)).toThrow()
    expect(() => assertSeq(1, meta)).toThrow()
    expect(() => assertSeq(true, meta)).toThrow()
    expect(() => assertSeq(false, meta)).toThrow()
    expect(() => assertSeq(null, meta)).toThrow()
    expect(() => assertSeq(undefined, meta)).toThrow()
    expect(() => assertSeq({}, meta)).toThrow()
  })

  test(`assertStringOrRegExp`, () => {
    expect(() => assertStringOrRegExp(``, meta)).not.toThrow()
    expect(() => assertStringOrRegExp(`1`, meta)).not.toThrow()
    expect(() => assertStringOrRegExp(/^a/, meta)).not.toThrow()
    expect(() => assertStringOrRegExp([], meta)).toThrow()
    expect(() => assertStringOrRegExp([1, 2, 3], meta)).toThrow()
    expect(() => assertStringOrRegExp(0, meta)).toThrow()
    expect(() => assertStringOrRegExp(1, meta)).toThrow()
    expect(() => assertStringOrRegExp(true, meta)).toThrow()
    expect(() => assertStringOrRegExp(false, meta)).toThrow()
    expect(() => assertStringOrRegExp(null, meta)).toThrow()
    expect(() => assertStringOrRegExp(undefined, meta)).toThrow()
    expect(() => assertStringOrRegExp({}, meta)).toThrow()
  })

  test(`isLitsFunction`, () => {
    const lf1: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `user-defined`,
      name: undefined,
      overloads: [
        {
          arguments: {
            mandatoryArguments: [],
          },
          functionContext: {},
          body: [],
          arity: 0,
        },
      ],
    }
    const lf2: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `builtin`,
      name: `+`,
    }
    const lf3: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `partial`,
      fn: { a: 10, b: 20 },
      params: [],
    }
    const lf4: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `comp`,
      fns: [`x`],
    }
    const lf5: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      type: `constantly`,
      value: 10,
    }
    expect(isLitsFunction(lf1)).toBe(true)
    expect(isLitsFunction(lf2)).toBe(true)
    expect(isLitsFunction(lf3)).toBe(true)
    expect(isLitsFunction(lf4)).toBe(true)
    expect(isLitsFunction(lf5)).toBe(true)
    expect(isLitsFunction(``)).toBe(false)
    expect(isLitsFunction(`1`)).toBe(false)
    expect(isLitsFunction(0)).toBe(false)
    expect(isLitsFunction(1)).toBe(false)
    expect(isLitsFunction(true)).toBe(false)
    expect(isLitsFunction(false)).toBe(false)
    expect(isLitsFunction(null)).toBe(false)
    expect(isLitsFunction(undefined)).toBe(false)
    expect(isLitsFunction([])).toBe(false)
    expect(isLitsFunction({})).toBe(false)
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
    expect(() => assertNumber(1 / 0, meta)).not.toThrow()
    expect(() => assertNumber(Number(`abc`), meta)).not.toThrow()
    expect(() => assertNumber(0.12, meta)).not.toThrow()
    expect(() => assertNumber(undefined, meta)).toThrow()
    expect(() => assertNumber(`undefined`, meta)).toThrow()
    expect(() => assertNumber([], meta)).toThrow()
  })

  test(`isRegexp`, () => {
    expect(isRegExp(`Hej`)).toBe(false)
    expect(isRegExp({})).toBe(false)
    expect(isRegExp(/^a/)).toBe(true)
  })

  test(`isNormalExpressionNodeName`, () => {
    expect(
      isNormalExpressionNodeName({
        type: `NormalExpression`,
        params: [],
        name: `object`,
        token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` },
      }),
    ).toBe(true)
    expect(
      isNormalExpressionNodeName({
        type: `NormalExpression`,
        params: [],
        expression: {
          type: `NormalExpression`,
          name: `+`,
          params: [{ type: `Number`, value: 2, token: { type: `name`, meta: { line: 0, column: 0 }, value: `X` } }],
        },
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } as any),
    ).toBe(false)
  })

  const primitives = [0, 1, true, false, null, `Albert`, `Mojir`]
  describe(`deepEqual`, () => {
    test(`primitives`, () => {
      for (const a of primitives) {
        for (const b of primitives) {
          expect(deepEqual(a, b, meta)).toBe(a === b)
        }
      }
    })
    test(`RegExp`, () => {
      expect(deepEqual(/^ab/, /^ab/, meta)).toBe(true)
      expect(deepEqual(/^ab/, new RegExp(`^ab`), meta)).toBe(true)
      expect(deepEqual(/^ab/gi, new RegExp(`^ab`, `ig`), meta)).toBe(true)
      expect(deepEqual(/^ab/g, /^ab/, meta)).toBe(false)
      expect(deepEqual(/ab/, /^ab/, meta)).toBe(false)
    })
    test(`nested structures`, () => {
      expect(deepEqual([1, 2, 3], [1, 2, 3], meta)).toBe(true)
      expect(deepEqual({ a: 1, b: 2 }, { a: 1, b: 2 }, meta)).toBe(true)
      expect(deepEqual([1, 2, { a: 1, b: 2 }], [1, 2, { b: 2, a: 1 }], meta)).toBe(true)
      expect(deepEqual(/^ab/, new RegExp(`^ab`), meta)).toBe(true)
      expect(deepEqual(/^ab/gi, new RegExp(`^ab`, `ig`), meta)).toBe(true)
      expect(deepEqual(/^ab/g, /^ab/, meta)).toBe(false)
      expect(deepEqual(/ab/, /^ab/, meta)).toBe(false)
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
    expect(() => assertMax(12, 10, meta)).toThrow()
    expect(() => assertMax(-12, -10, meta)).not.toThrow()
    expect(() => assertMax(-8, -10, meta)).toThrow()
    expect(() => assertMax(10, 10, meta)).not.toThrow()
    expect(() => assertMax(0, 10, meta)).not.toThrow()
  })
  test(`assertChar`, () => {
    expect(() => assertChar(`2`, meta)).not.toThrow()
    expect(() => assertChar(`Albert`, meta)).toThrow()
    expect(() => assertChar(0, meta)).toThrow()
    expect(() => assertChar(null, meta)).toThrow()
    expect(() => assertChar(true, meta)).toThrow()
    expect(() => assertChar(false, meta)).toThrow()
    expect(() => assertChar([`a`], meta)).toThrow()
    expect(() => assertChar({ a: `a` }, meta)).toThrow()
  })
  test(`asChar`, () => {
    expect(asChar(`2`, meta)).toBe(`2`)
    expect(() => asChar(`Albert`, meta)).toThrow()
    expect(() => asChar(0, meta)).toThrow()
    expect(() => asChar(null, meta)).toThrow()
    expect(() => asChar(true, meta)).toThrow()
    expect(() => asChar(false, meta)).toThrow()
    expect(() => asChar([`a`], meta)).toThrow()
    expect(() => asChar({ a: `a` }, meta)).toThrow()
  })

  test(`asColl`, () => {
    expect(asColl(`2`, meta)).toEqual(`2`)
    expect(asColl({ a: 1 }, meta)).toEqual({ a: 1 })
    expect(asColl([2], meta)).toEqual([2])
    expect(() => asColl(0, meta)).toThrow()
    expect(() => asColl(null, meta)).toThrow()
    expect(() => asColl(true, meta)).toThrow()
    expect(() => asColl(false, meta)).toThrow()
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
