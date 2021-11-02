import { AstNode, FUNCTION_SYMBOL, LitsFunction, NameNode, NormalExpressionNode } from '../src/parser/interface'
import { SourceCodeInfo } from '../src/tokenizer/interface'
import {
  asNameNode,
  asNotUndefined,
  assertLengthEven,
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
  assertPositiveNumber,
  assertRegExp,
  assertString,
  asNonEmptyString,
  asFiniteNumber,
  assertLength,
  assertStringOrRegExp,
  collHasKey,
  isRegExp,
  isNormalExpressionNodeName,
  deepEqual,
  assertNonEmptyString,
  assertNotUndefined,
  toNonNegativeInteger,
  assertMax,
  assertChar,
  asChar,
  cloneColl,
  asString,
} from '../src/utils'
import { any, collection, litsFunction, number, object, sequence, array } from '../src/utils/assertion'

const sourceCodeInfo: SourceCodeInfo = `EOF`
describe(`utils`, () => {
  test(`asAny`, () => {
    expect(() => any.as(undefined, sourceCodeInfo)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }

    expect(any.as(node, sourceCodeInfo)).toBe(node)
  })
  test(`assertAny`, () => {
    expect(() => any.assert(undefined, sourceCodeInfo)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }

    expect(() => any.assert(node, sourceCodeInfo)).not.toThrow()
  })
  test(`assertAny`, () => {
    expect(() => any.assert(undefined, sourceCodeInfo)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }

    expect(() => any.assert(node, sourceCodeInfo)).not.toThrow()
  })
  test(`asLitsFunction`, () => {
    expect(() => litsFunction.as(undefined, sourceCodeInfo)).toThrow()
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
    expect(litsFunction.as(lf, sourceCodeInfo)).toBe(lf)
  })
  test(`asNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => asNameNode(undefined, {} as any)).toThrow()
    expect(() =>
      asNameNode(
        {
          type: `Number`,
          value: 12,
          token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
        },
        { line: 0, column: 0, sourceCodeLine: null },
      ),
    ).toThrow()
    const nameNode: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }
    expect(asNameNode(nameNode, nameNode.token.sourceCodeInfo)).toBe(nameNode)
  })
  test(`assertNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => assertNameNode(undefined, {} as any)).toThrow()
    const nameNode: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }
    asNameNode(nameNode, nameNode.token.sourceCodeInfo)
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
    expect(asNonEmptyString(`1`, sourceCodeInfo)).toBe(`1`)
    expect(() => asNonEmptyString(``, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString(0, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString(1, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString(true, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString(false, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString(null, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString(undefined, sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString([], sourceCodeInfo)).toThrow()
    expect(() => asNonEmptyString({}, sourceCodeInfo)).toThrow()
  })

  test(`assertArr`, () => {
    expect(() => array.assert(0, sourceCodeInfo)).toThrow()
    expect(() => array.assert({}, sourceCodeInfo)).toThrow()
    expect(() => array.assert([], sourceCodeInfo)).not.toThrow()
    expect(() => array.assert([1], sourceCodeInfo)).not.toThrow()
    expect(() => array.assert(true, sourceCodeInfo)).toThrow()
    expect(() => array.assert(null, sourceCodeInfo)).toThrow()
    expect(() => array.assert(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertObj`, () => {
    expect(() => object.assert(0, sourceCodeInfo)).toThrow()
    expect(() => object.assert({}, sourceCodeInfo)).not.toThrow()
    expect(() => object.assert({ [FUNCTION_SYMBOL]: true }, sourceCodeInfo)).toThrow()
    expect(() => object.assert({ a: 1 }, sourceCodeInfo)).not.toThrow()
    expect(() => object.assert(/test/, sourceCodeInfo)).toThrow()
    expect(() => object.assert([], sourceCodeInfo)).toThrow()
    expect(() => object.assert([1], sourceCodeInfo)).toThrow()
    expect(() => object.assert(true, sourceCodeInfo)).toThrow()
    expect(() => object.assert(null, sourceCodeInfo)).toThrow()
    expect(() => object.assert(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertInteger`, () => {
    expect(() => number.assert(-0, sourceCodeInfo, { integer: true })).not.toThrow()
    expect(() => number.assert(-1, sourceCodeInfo, { integer: true })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { integer: true })).not.toThrow()
    expect(() => number.assert(-0.1, sourceCodeInfo, { integer: true })).toThrow()
    expect(() => number.assert(1.00001, sourceCodeInfo, { integer: true })).toThrow()
    expect(() => number.assert(`k`, sourceCodeInfo, { integer: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { integer: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { integer: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { integer: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { integer: true })).toThrow()
  })
  test(`assertRegExp`, () => {
    expect(() => assertRegExp(/a/, sourceCodeInfo)).not.toThrow()
    expect(() => assertRegExp(new RegExp(`a`), sourceCodeInfo)).not.toThrow()
    expect(() => assertRegExp(0, sourceCodeInfo)).toThrow()
    expect(() => assertRegExp(`0`, sourceCodeInfo)).toThrow()
    expect(() => assertRegExp(null, sourceCodeInfo)).toThrow()
    expect(() => assertRegExp(undefined, sourceCodeInfo)).toThrow()
    expect(() => assertRegExp(false, sourceCodeInfo)).toThrow()
    expect(() => assertRegExp(true, sourceCodeInfo)).toThrow()
    expect(() => assertRegExp([], sourceCodeInfo)).toThrow()
    expect(() => assertRegExp({}, sourceCodeInfo)).toThrow()
  })

  function node(arr: number[]): NormalExpressionNode {
    const astNodes: AstNode[] = arr.map(n => ({
      type: `Number`,
      value: n,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }))
    return {
      name: `let`,
      params: astNodes,
      type: `NormalExpression`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
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
    expect(() => litsFunction.assert(lf, sourceCodeInfo)).not.toThrow()
    expect(() => litsFunction.assert(1, sourceCodeInfo)).toThrow()
    expect(() => litsFunction.assert({}, sourceCodeInfo)).toThrow()
  })
  test(`assertPositiveNumber`, () => {
    expect(() => assertPositiveNumber(-1, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(-0.5, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(0, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(0.5, sourceCodeInfo)).not.toThrow()
    expect(() => assertPositiveNumber(1, sourceCodeInfo)).not.toThrow()
    expect(() => assertPositiveNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber([], sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber({}, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(true, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(false, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(null, sourceCodeInfo)).toThrow()
    expect(() => assertPositiveNumber(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertNegativeNumber`, () => {
    expect(() => assertNegativeNumber(-1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNegativeNumber(-0.5, sourceCodeInfo)).not.toThrow()
    expect(() => assertNegativeNumber(0, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(0.5, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(1, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber([], sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber({}, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(true, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(false, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(null, sourceCodeInfo)).toThrow()
    expect(() => assertNegativeNumber(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertNonNegativeNumber`, () => {
    expect(() => assertNonNegativeNumber(-1, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber(-1.1, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber(0, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonNegativeNumber(0.1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonNegativeNumber(1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonNegativeNumber(1.1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonNegativeNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber([], sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber({}, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber(true, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber(false, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber(null, sourceCodeInfo)).toThrow()
    expect(() => assertNonNegativeNumber(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertNonPositiveNumber`, () => {
    expect(() => assertNonPositiveNumber(-1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonPositiveNumber(-1.1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonPositiveNumber(0, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonPositiveNumber(0.1, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(1, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(1.1, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber([], sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber({}, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(true, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(false, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(null, sourceCodeInfo)).toThrow()
    expect(() => assertNonPositiveNumber(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertFiniteNumber`, () => {
    expect(() => assertFiniteNumber(-1, sourceCodeInfo)).not.toThrow()
    expect(() => assertFiniteNumber(-1.1, sourceCodeInfo)).not.toThrow()
    expect(() => assertFiniteNumber(0, sourceCodeInfo)).not.toThrow()
    expect(() => assertFiniteNumber(0.1, sourceCodeInfo)).not.toThrow()
    expect(() => assertFiniteNumber(1, sourceCodeInfo)).not.toThrow()
    expect(() => assertFiniteNumber(1.1, sourceCodeInfo)).not.toThrow()
    expect(() => assertFiniteNumber(Math.asin(2), sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber(1 / 0, sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber([], sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber({}, sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber(true, sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber(false, sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber(null, sourceCodeInfo)).toThrow()
    expect(() => assertFiniteNumber(undefined, sourceCodeInfo)).toThrow()
  })
  test(`asFiniteNumber`, () => {
    expect(asFiniteNumber(-1, sourceCodeInfo)).toBe(-1)
    expect(asFiniteNumber(-1.1, sourceCodeInfo)).toBe(-1.1)
    expect(asFiniteNumber(0, sourceCodeInfo)).toBe(0)
    expect(asFiniteNumber(0.1, sourceCodeInfo)).toBe(0.1)
    expect(asFiniteNumber(1, sourceCodeInfo)).toBe(1)
    expect(asFiniteNumber(1.1, sourceCodeInfo)).toBe(1.1)
    expect(() => asFiniteNumber(Math.asin(2), sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(1 / 0, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(`1`, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber([], sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber({}, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(true, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(false, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(null, sourceCodeInfo)).toThrow()
    expect(() => asFiniteNumber(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertNumberGt`, () => {
    expect(() => assertNumberGt(0, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGt(0.5, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGt(1, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGt(1.5, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberGt(2, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberGt(`2`, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGt([], 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGt(false, 1, sourceCodeInfo)).toThrow()
  })
  test(`assertNumberGte`, () => {
    expect(() => assertNumberGte(0, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGte(0.5, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGte(1, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberGte(1.5, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberGte(2, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberGte(`2`, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGte([], 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberGte(false, 1, sourceCodeInfo)).toThrow()
  })
  test(`assertNumberLt`, () => {
    expect(() => assertNumberLt(0, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberLt(0.5, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberLt(1, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLt(1.5, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLt(2, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLt(`2`, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLt([], 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLt(false, 1, sourceCodeInfo)).toThrow()
  })
  test(`assertNumberLte`, () => {
    expect(() => assertNumberLte(0, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberLte(0.5, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberLte(1, 1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberLte(1.5, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLte(2, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLte(`2`, 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLte([], 1, sourceCodeInfo)).toThrow()
    expect(() => assertNumberLte(false, 1, sourceCodeInfo)).toThrow()
  })
  test(`assertNumberNotZero`, () => {
    expect(() => assertNumberNotZero(-1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberNotZero(-0.5, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberNotZero(0, sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero(0.5, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberNotZero(1, sourceCodeInfo)).not.toThrow()
    expect(() => assertNumberNotZero(`1`, sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero([], sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero({}, sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero(true, sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero(false, sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero(null, sourceCodeInfo)).toThrow()
    expect(() => assertNumberNotZero(undefined, sourceCodeInfo)).toThrow()
  })
  test(`assertString`, () => {
    expect(() => assertString(``, sourceCodeInfo)).not.toThrow()
    expect(() => assertString(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => assertString(0, sourceCodeInfo)).toThrow()
    expect(() => assertString(1, sourceCodeInfo)).toThrow()
    expect(() => assertString(true, sourceCodeInfo)).toThrow()
    expect(() => assertString(false, sourceCodeInfo)).toThrow()
    expect(() => assertString(null, sourceCodeInfo)).toThrow()
    expect(() => assertString(undefined, sourceCodeInfo)).toThrow()
    expect(() => assertString([], sourceCodeInfo)).toThrow()
    expect(() => assertString({}, sourceCodeInfo)).toThrow()
  })
  test(`asString`, () => {
    expect(() => asString(``, sourceCodeInfo)).not.toThrow()
    expect(() => asString(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => asString(0, sourceCodeInfo)).toThrow()
    expect(() => asString(1, sourceCodeInfo)).toThrow()
    expect(() => asString(true, sourceCodeInfo)).toThrow()
    expect(() => asString(false, sourceCodeInfo)).toThrow()
    expect(() => asString(null, sourceCodeInfo)).toThrow()
    expect(() => asString(undefined, sourceCodeInfo)).toThrow()
    expect(() => asString([], sourceCodeInfo)).toThrow()
    expect(() => asString({}, sourceCodeInfo)).toThrow()
  })
  test(`assertNonEmptyString`, () => {
    expect(() => assertNonEmptyString(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonEmptyString(`abc`, sourceCodeInfo)).not.toThrow()
    expect(() => assertNonEmptyString(``, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString(0, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString(1, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString(true, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString(false, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString(null, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString(undefined, sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString([], sourceCodeInfo)).toThrow()
    expect(() => assertNonEmptyString({}, sourceCodeInfo)).toThrow()
  })

  test(`assertStringOrArray`, () => {
    expect(() => sequence.assert(``, sourceCodeInfo)).not.toThrow()
    expect(() => sequence.assert(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => sequence.assert([], sourceCodeInfo)).not.toThrow()
    expect(() => sequence.assert([1, 2, 3], sourceCodeInfo)).not.toThrow()
    expect(() => sequence.assert(0, sourceCodeInfo)).toThrow()
    expect(() => sequence.assert(1, sourceCodeInfo)).toThrow()
    expect(() => sequence.assert(true, sourceCodeInfo)).toThrow()
    expect(() => sequence.assert(false, sourceCodeInfo)).toThrow()
    expect(() => sequence.assert(null, sourceCodeInfo)).toThrow()
    expect(() => sequence.assert(undefined, sourceCodeInfo)).toThrow()
    expect(() => sequence.assert({}, sourceCodeInfo)).toThrow()
  })

  test(`assertStringOrRegExp`, () => {
    expect(() => assertStringOrRegExp(``, sourceCodeInfo)).not.toThrow()
    expect(() => assertStringOrRegExp(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => assertStringOrRegExp(/^a/, sourceCodeInfo)).not.toThrow()
    expect(() => assertStringOrRegExp([], sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp([1, 2, 3], sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp(0, sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp(1, sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp(true, sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp(false, sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp(null, sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp(undefined, sourceCodeInfo)).toThrow()
    expect(() => assertStringOrRegExp({}, sourceCodeInfo)).toThrow()
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
    expect(litsFunction.is(lf1)).toBe(true)
    expect(litsFunction.is(lf2)).toBe(true)
    expect(litsFunction.is(lf3)).toBe(true)
    expect(litsFunction.is(lf4)).toBe(true)
    expect(litsFunction.is(lf5)).toBe(true)
    expect(litsFunction.is(``)).toBe(false)
    expect(litsFunction.is(`1`)).toBe(false)
    expect(litsFunction.is(0)).toBe(false)
    expect(litsFunction.is(1)).toBe(false)
    expect(litsFunction.is(true)).toBe(false)
    expect(litsFunction.is(false)).toBe(false)
    expect(litsFunction.is(null)).toBe(false)
    expect(litsFunction.is(undefined)).toBe(false)
    expect(litsFunction.is([])).toBe(false)
    expect(litsFunction.is({})).toBe(false)
  })

  test(`isNumber`, () => {
    expect(number.is(1 / 0)).toBe(true)
    expect(number.is(Number(`abc`))).toBe(true)
    expect(number.is(0.12)).toBe(true)
    expect(number.is(undefined)).toBe(false)
    expect(number.is(`undefined`)).toBe(false)
    expect(number.is([])).toBe(false)
  })

  test(`asInteger`, () => {
    expect(() => number.as(1 / 0, `EOF`, { integer: true })).toThrow()
    expect(() => number.as(Number(`abc`), `EOF`, { integer: true })).toThrow()
    expect(() => number.as(12, `EOF`, { integer: true })).not.toThrow()
    expect(() => number.as(undefined, `EOF`, { integer: true })).toThrow()
    expect(() => number.as(`undefined`, `EOF`, { integer: true })).toThrow()
    expect(() => number.as([], `EOF`, { integer: true })).toThrow()
  })

  test(`isInteger`, () => {
    expect(number.is(1 / 0, { integer: true })).toBe(false)
    expect(number.is(Number(`abc`), { integer: true })).toBe(false)
    expect(number.is(0.12, { integer: true })).toBe(false)
    expect(number.is(-12, { integer: true })).toBe(true)
    expect(number.is(0, { integer: true })).toBe(true)
    expect(number.is(12, { integer: true })).toBe(true)
    expect(number.is(undefined, { integer: true })).toBe(false)
    expect(number.is(`undefined`, { integer: true })).toBe(false)
    expect(number.is([], { integer: true })).toBe(false)
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
    expect(() => number.assert(1 / 0, sourceCodeInfo)).not.toThrow()
    expect(() => number.assert(Number(`abc`), sourceCodeInfo)).not.toThrow()
    expect(() => number.assert(0.12, sourceCodeInfo)).not.toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo)).toThrow()
    expect(() => number.assert(`undefined`, sourceCodeInfo)).toThrow()
    expect(() => number.assert([], sourceCodeInfo)).toThrow()
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
        token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
      }),
    ).toBe(true)
    expect(
      isNormalExpressionNodeName({
        type: `NormalExpression`,
        params: [],
        expression: {
          type: `NormalExpression`,
          name: `+`,
          params: [
            {
              type: `Number`,
              value: 2,
              token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
            },
          ],
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
          expect(deepEqual(a, b, sourceCodeInfo)).toBe(a === b)
        }
      }
    })
    test(`RegExp`, () => {
      expect(deepEqual(/^ab/, /^ab/, sourceCodeInfo)).toBe(true)
      expect(deepEqual(/^ab/, new RegExp(`^ab`), sourceCodeInfo)).toBe(true)
      expect(deepEqual(/^ab/gi, new RegExp(`^ab`, `ig`), sourceCodeInfo)).toBe(true)
      expect(deepEqual(/^ab/g, /^ab/, sourceCodeInfo)).toBe(false)
      expect(deepEqual(/ab/, /^ab/, sourceCodeInfo)).toBe(false)
    })
    test(`nested structures`, () => {
      expect(deepEqual([1, 2, 3], [1, 2, 3], sourceCodeInfo)).toBe(true)
      expect(deepEqual({ a: 1, b: 2 }, { a: 1, b: 2 }, sourceCodeInfo)).toBe(true)
      expect(deepEqual([1, 2, { a: 1, b: 2 }], [1, 2, { b: 2, a: 1 }], sourceCodeInfo)).toBe(true)
      expect(deepEqual(/^ab/, new RegExp(`^ab`), sourceCodeInfo)).toBe(true)
      expect(deepEqual(/^ab/gi, new RegExp(`^ab`, `ig`), sourceCodeInfo)).toBe(true)
      expect(deepEqual(/^ab/g, /^ab/, sourceCodeInfo)).toBe(false)
      expect(deepEqual(/ab/, /^ab/, sourceCodeInfo)).toBe(false)
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
    expect(() => assertMax(12, 10, sourceCodeInfo)).toThrow()
    expect(() => assertMax(-12, -10, sourceCodeInfo)).not.toThrow()
    expect(() => assertMax(-8, -10, sourceCodeInfo)).toThrow()
    expect(() => assertMax(10, 10, sourceCodeInfo)).not.toThrow()
    expect(() => assertMax(0, 10, sourceCodeInfo)).not.toThrow()
  })
  test(`assertChar`, () => {
    expect(() => assertChar(`2`, sourceCodeInfo)).not.toThrow()
    expect(() => assertChar(`Albert`, sourceCodeInfo)).toThrow()
    expect(() => assertChar(0, sourceCodeInfo)).toThrow()
    expect(() => assertChar(null, sourceCodeInfo)).toThrow()
    expect(() => assertChar(true, sourceCodeInfo)).toThrow()
    expect(() => assertChar(false, sourceCodeInfo)).toThrow()
    expect(() => assertChar([`a`], sourceCodeInfo)).toThrow()
    expect(() => assertChar({ a: `a` }, sourceCodeInfo)).toThrow()
  })
  test(`asChar`, () => {
    expect(asChar(`2`, sourceCodeInfo)).toBe(`2`)
    expect(() => asChar(`Albert`, sourceCodeInfo)).toThrow()
    expect(() => asChar(0, sourceCodeInfo)).toThrow()
    expect(() => asChar(null, sourceCodeInfo)).toThrow()
    expect(() => asChar(true, sourceCodeInfo)).toThrow()
    expect(() => asChar(false, sourceCodeInfo)).toThrow()
    expect(() => asChar([`a`], sourceCodeInfo)).toThrow()
    expect(() => asChar({ a: `a` }, sourceCodeInfo)).toThrow()
  })

  test(`asColl`, () => {
    expect(collection.as(`2`, sourceCodeInfo)).toEqual(`2`)
    expect(collection.as({ a: 1 }, sourceCodeInfo)).toEqual({ a: 1 })
    expect(collection.as([2], sourceCodeInfo)).toEqual([2])
    expect(() => collection.as(0, sourceCodeInfo)).toThrow()
    expect(() => collection.as(null, sourceCodeInfo)).toThrow()
    expect(() => collection.as(true, sourceCodeInfo)).toThrow()
    expect(() => collection.as(false, sourceCodeInfo)).toThrow()
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
