import { AstNode, FUNCTION_SYMBOL, LitsFunction, NameNode, NormalExpressionNode } from '../src/parser/interface'
import { SourceCodeInfo } from '../src/tokenizer/interface'
import {
  assertLengthEven,
  assertRegExp,
  assertLength,
  assertStringOrRegExp,
  collHasKey,
  isRegExp,
  isNormalExpressionNodeName,
  deepEqual,
  assertNotUndefined,
  toNonNegativeInteger,
  cloneColl,
  asX,
} from '../src/utils'
import {
  any,
  collection,
  litsFunction,
  number,
  object,
  sequence,
  array,
  nameNode,
  string,
} from '../src/utils/assertion'

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
    expect(() => nameNode.as(undefined, {} as any)).toThrow()
    expect(() =>
      nameNode.as(
        {
          type: `Number`,
          value: 12,
          token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
        },
        { line: 0, column: 0, sourceCodeLine: null },
      ),
    ).toThrow()
    const node: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }
    expect(nameNode.as(node, node.token.sourceCodeInfo)).toBe(node)
  })
  test(`assertNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => nameNode.assert(undefined, {} as any)).toThrow()
    const node: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
    }
    nameNode.as(node, node.token.sourceCodeInfo)
  })
  test(`asNotUndefined`, () => {
    expect(() => asX(undefined, `EOF`)).toThrow()
    expect(asX(null, `EOF`)).toBe(null)
    expect(asX(false, `EOF`)).toBe(false)
    expect(asX(true, `EOF`)).toBe(true)
    expect(asX(0, `EOF`)).toBe(0)
    const obj = {}
    expect(asX(obj, `EOF`)).toBe(obj)
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
    expect(string.as(`1`, sourceCodeInfo, { nonEmpty: true })).toBe(`1`)
    expect(() => string.as(``, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(0, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(1, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(true, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(false, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(null, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(undefined, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as([], sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as({}, sourceCodeInfo, { nonEmpty: true })).toThrow()
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
    expect(() => number.assert(-1, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(-0.5, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { positive: true })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { positive: true })).not.toThrow()
    expect(() => number.assert(`1`, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert({}, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(true, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { positive: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { positive: true })).toThrow()
  })
  test(`assertNegativeNumber`, () => {
    expect(() => number.assert(-1, sourceCodeInfo, { negative: true })).not.toThrow()
    expect(() => number.assert(-0.5, sourceCodeInfo, { negative: true })).not.toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(`1`, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert({}, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(true, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { negative: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { negative: true })).toThrow()
  })
  test(`assertNonNegativeNumber`, () => {
    expect(() => number.assert(-1, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(-1.1, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(0.1, sourceCodeInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(1.1, sourceCodeInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(`1`, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert({}, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(true, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { nonNegative: true })).toThrow()
  })
  test(`assertNonPositiveNumber`, () => {
    expect(() => number.assert(-1, sourceCodeInfo, { nonPositive: true })).not.toThrow()
    expect(() => number.assert(-1.1, sourceCodeInfo, { nonPositive: true })).not.toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { nonPositive: true })).not.toThrow()
    expect(() => number.assert(0.1, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(1.1, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(`1`, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert({}, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(true, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { nonPositive: true })).toThrow()
  })
  test(`assertFiniteNumber`, () => {
    expect(() => number.assert(-1, sourceCodeInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(-1.1, sourceCodeInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(0.1, sourceCodeInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(1.1, sourceCodeInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(Math.asin(2), sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert(1 / 0, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert(`1`, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert({}, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert(true, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { finite: true })).toThrow()
  })
  test(`asFiniteNumber`, () => {
    expect(number.as(-1, sourceCodeInfo, { finite: true })).toBe(-1)
    expect(number.as(-1.1, sourceCodeInfo, { finite: true })).toBe(-1.1)
    expect(number.as(0, sourceCodeInfo, { finite: true })).toBe(0)
    expect(number.as(0.1, sourceCodeInfo, { finite: true })).toBe(0.1)
    expect(number.as(1, sourceCodeInfo, { finite: true })).toBe(1)
    expect(number.as(1.1, sourceCodeInfo, { finite: true })).toBe(1.1)
    expect(() => number.as(Math.asin(2), sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(1 / 0, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(`1`, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(`1`, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as([], sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as({}, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(true, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(false, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(null, sourceCodeInfo, { finite: true })).toThrow()
    expect(() => number.as(undefined, sourceCodeInfo, { finite: true })).toThrow()
  })
  test(`assertNumberGt`, () => {
    expect(() => number.assert(0, sourceCodeInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(1.5, sourceCodeInfo, { gt: 1 })).not.toThrow()
    expect(() => number.assert(2, sourceCodeInfo, { gt: 1 })).not.toThrow()
    expect(() => number.assert(`2`, sourceCodeInfo, { gt: 1 })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { gt: 1 })).toThrow()
  })
  test(`assertNumberGte`, () => {
    expect(() => number.assert(0, sourceCodeInfo, { gte: 1 })).toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { gte: 1 })).toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { gte: 1 })).not.toThrow()
    expect(() => number.assert(1.5, sourceCodeInfo, { gte: 1 })).not.toThrow()
    expect(() => number.assert(2, sourceCodeInfo, { gte: 1 })).not.toThrow()
    expect(() => number.assert(`2`, sourceCodeInfo, { gte: 1 })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { gte: 1 })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { gte: 1 })).toThrow()
  })
  test(`assertNumberLt`, () => {
    expect(() => number.assert(0, sourceCodeInfo, { lt: 1 })).not.toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { lt: 1 })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(1.5, sourceCodeInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(2, sourceCodeInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(`2`, sourceCodeInfo, { lt: 1 })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { lt: 1 })).toThrow()
  })
  test(`assertNumberLte`, () => {
    expect(() => number.assert(0, sourceCodeInfo, { lte: 1 })).not.toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { lte: 1 })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { lte: 1 })).not.toThrow()
    expect(() => number.assert(1.5, sourceCodeInfo, { lte: 1 })).toThrow()
    expect(() => number.assert(2, sourceCodeInfo, { lte: 1 })).toThrow()
    expect(() => number.assert(`2`, sourceCodeInfo, { lte: 1 })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { lte: 1 })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { lte: 1 })).toThrow()
  })
  test(`assertNumberNotZero`, () => {
    expect(() => number.assert(-1, sourceCodeInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(-0.5, sourceCodeInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(0.5, sourceCodeInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(1, sourceCodeInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(`1`, sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert([], sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert({}, sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(true, sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(false, sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(null, sourceCodeInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo, { nonZero: true })).toThrow()
  })
  test(`assertString`, () => {
    expect(() => string.assert(``, sourceCodeInfo)).not.toThrow()
    expect(() => string.assert(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => string.assert(0, sourceCodeInfo)).toThrow()
    expect(() => string.assert(1, sourceCodeInfo)).toThrow()
    expect(() => string.assert(true, sourceCodeInfo)).toThrow()
    expect(() => string.assert(false, sourceCodeInfo)).toThrow()
    expect(() => string.assert(null, sourceCodeInfo)).toThrow()
    expect(() => string.assert(undefined, sourceCodeInfo)).toThrow()
    expect(() => string.assert([], sourceCodeInfo)).toThrow()
    expect(() => string.assert({}, sourceCodeInfo)).toThrow()
  })
  test(`asString`, () => {
    expect(() => string.as(``, sourceCodeInfo)).not.toThrow()
    expect(() => string.as(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => string.as(0, sourceCodeInfo)).toThrow()
    expect(() => string.as(1, sourceCodeInfo)).toThrow()
    expect(() => string.as(true, sourceCodeInfo)).toThrow()
    expect(() => string.as(false, sourceCodeInfo)).toThrow()
    expect(() => string.as(null, sourceCodeInfo)).toThrow()
    expect(() => string.as(undefined, sourceCodeInfo)).toThrow()
    expect(() => string.as([], sourceCodeInfo)).toThrow()
    expect(() => string.as({}, sourceCodeInfo)).toThrow()
  })
  test(`assertNonEmptyString`, () => {
    expect(() => string.assert(`1`, sourceCodeInfo, { nonEmpty: true })).not.toThrow()
    expect(() => string.assert(`abc`, sourceCodeInfo, { nonEmpty: true })).not.toThrow()
    expect(() => string.assert(``, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(0, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(1, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(true, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(false, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(null, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(undefined, sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert([], sourceCodeInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert({}, sourceCodeInfo, { nonEmpty: true })).toThrow()
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
    expect(() => number.assert(12, sourceCodeInfo, { lte: 10 })).toThrow()
    expect(() => number.assert(-12, sourceCodeInfo, { lte: -10 })).not.toThrow()
    expect(() => number.assert(-8, sourceCodeInfo, { lte: -10 })).toThrow()
    expect(() => number.assert(10, sourceCodeInfo, { lte: 10 })).not.toThrow()
    expect(() => number.assert(0, sourceCodeInfo, { lte: 10 })).not.toThrow()
  })
  test(`assertChar`, () => {
    expect(() => string.assert(`2`, sourceCodeInfo, { char: true })).not.toThrow()
    expect(() => string.assert(`Albert`, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.assert(0, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.assert(null, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.assert(true, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.assert(false, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.assert([`a`], sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.assert({ a: `a` }, sourceCodeInfo, { char: true })).toThrow()
  })
  test(`asChar`, () => {
    expect(string.as(`2`, sourceCodeInfo, { char: true })).toBe(`2`)
    expect(() => string.as(`Albert`, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.as(0, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.as(null, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.as(true, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.as(false, sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.as([`a`], sourceCodeInfo, { char: true })).toThrow()
    expect(() => string.as({ a: `a` }, sourceCodeInfo, { char: true })).toThrow()
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
