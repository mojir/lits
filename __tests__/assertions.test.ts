import {
  AstNode,
  FUNCTION_SYMBOL,
  LitsFunction,
  NameNode,
  NormalExpressionNode,
  REGEXP_SYMBOL,
  RegularExpression,
} from '../src/parser/interface'
import { DebugInfo, Token } from '../src/tokenizer/interface'
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
  asValue,
  assertValue,
  regularExpression,
  assertEvenNumberOfParams,
  assertNumberOfParams,
  stringOrRegExp,
  normalExpressionNodeWithName,
  expressionNode,
  astNode,
  token,
} from '../src/utils/assertion'

const debugInfo: DebugInfo = `EOF`
describe(`utils`, () => {
  test(`asAny`, () => {
    expect(() => any.as(undefined, debugInfo)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, value: `X` },
    }

    expect(any.as(node, debugInfo)).toBe(node)
  })
  test(`assertAny`, () => {
    expect(() => any.assert(undefined, debugInfo)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, value: `X` },
    }

    expect(() => any.assert(node, debugInfo)).not.toThrow()
  })
  test(`assertAny`, () => {
    expect(() => any.assert(undefined, debugInfo)).toThrow()
    const node: AstNode = {
      type: `Name`,
      value: `test`,
      token: { type: `name`, value: `X` },
    }

    expect(() => any.assert(node, debugInfo)).not.toThrow()
  })
  test(`asLitsFunction`, () => {
    expect(() => litsFunction.as(undefined, debugInfo)).toThrow()
    const lf: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: `EOF`,
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
    expect(litsFunction.as(lf, debugInfo)).toBe(lf)
  })
  test(`asNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => nameNode.as(undefined, {} as any)).toThrow()
    expect(() =>
      nameNode.as({
        type: `Number`,
        value: 12,
        token: { type: `name`, value: `X` },
      }),
    ).toThrow()
    const node: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, value: `X` },
    }
    expect(nameNode.as(node, node.token?.debugInfo)).toBe(node)
  })
  test(`assertNameNode`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    expect(() => nameNode.assert(undefined, {} as any)).toThrow()
    const node: NameNode = {
      type: `Name`,
      value: `a-name`,
      token: { type: `name`, value: `X` },
    }
    nameNode.as(node, node.token?.debugInfo)
  })
  test(`asNotUndefined`, () => {
    expect(() => asValue(undefined, `EOF`)).toThrow()
    expect(asValue(null, `EOF`)).toBe(null)
    expect(asValue(false, `EOF`)).toBe(false)
    expect(asValue(true, `EOF`)).toBe(true)
    expect(asValue(0, `EOF`)).toBe(0)
    const obj = {}
    expect(asValue(obj, `EOF`)).toBe(obj)
  })
  test(`assertNotUndefined`, () => {
    expect(() => assertValue(undefined, `EOF`)).toThrow()
    expect(() => assertValue(undefined, `EOF`)).toThrow()
    expect(() => assertValue(null, `EOF`)).not.toThrow()
    expect(() => assertValue(false, `EOF`)).not.toThrow()
    expect(() => assertValue(true, `EOF`)).not.toThrow()
    expect(() => assertValue(0, `EOF`)).not.toThrow()
    expect(() => assertValue({}, `EOF`)).not.toThrow()
  })
  test(`asNonEmptyString`, () => {
    expect(string.as(`1`, debugInfo, { nonEmpty: true })).toBe(`1`)
    expect(() => string.as(``, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(0, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(1, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(true, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(false, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(null, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as(undefined, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as([], debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.as({}, debugInfo, { nonEmpty: true })).toThrow()
  })

  test(`assertArr`, () => {
    expect(() => array.assert(0, debugInfo)).toThrow()
    expect(() => array.assert({}, debugInfo)).toThrow()
    expect(() => array.assert([], debugInfo)).not.toThrow()
    expect(() => array.assert([1], debugInfo)).not.toThrow()
    expect(() => array.assert(true, debugInfo)).toThrow()
    expect(() => array.assert(null, debugInfo)).toThrow()
    expect(() => array.assert(undefined, debugInfo)).toThrow()
  })
  test(`assertObj`, () => {
    expect(() => object.assert(0, debugInfo)).toThrow()
    expect(() => object.assert({}, debugInfo)).not.toThrow()
    expect(() => object.assert({ [FUNCTION_SYMBOL]: true }, debugInfo)).toThrow()
    expect(() => object.assert({ a: 1 }, debugInfo)).not.toThrow()
    expect(() => object.assert(/test/, debugInfo)).toThrow()
    expect(() => object.assert([], debugInfo)).toThrow()
    expect(() => object.assert([1], debugInfo)).toThrow()
    expect(() => object.assert(true, debugInfo)).toThrow()
    expect(() => object.assert(null, debugInfo)).toThrow()
    expect(() => object.assert(undefined, debugInfo)).toThrow()
  })
  test(`assertInteger`, () => {
    expect(() => number.assert(-0, debugInfo, { integer: true })).not.toThrow()
    expect(() => number.assert(-1, debugInfo, { integer: true })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { integer: true })).not.toThrow()
    expect(() => number.assert(-0.1, debugInfo, { integer: true })).toThrow()
    expect(() => number.assert(1.00001, debugInfo, { integer: true })).toThrow()
    expect(() => number.assert(`k`, debugInfo, { integer: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { integer: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { integer: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { integer: true })).toThrow()
    expect(() => number.assert([], debugInfo, { integer: true })).toThrow()
  })
  test(`assertRegExp`, () => {
    const a: RegularExpression = {
      [REGEXP_SYMBOL]: true,
      source: `^ab`,
      flags: ``,
    }
    expect(() => regularExpression.assert(/a/, debugInfo)).toThrow()
    expect(() => regularExpression.assert(a, debugInfo)).not.toThrow()
    expect(() => regularExpression.assert(new RegExp(`a`), debugInfo)).toThrow()
    expect(() => regularExpression.assert(0, debugInfo)).toThrow()
    expect(() => regularExpression.assert(`0`, debugInfo)).toThrow()
    expect(() => regularExpression.assert(null, debugInfo)).toThrow()
    expect(() => regularExpression.assert(undefined, debugInfo)).toThrow()
    expect(() => regularExpression.assert(false, debugInfo)).toThrow()
    expect(() => regularExpression.assert(true, debugInfo)).toThrow()
    expect(() => regularExpression.assert([], debugInfo)).toThrow()
    expect(() => regularExpression.assert({}, debugInfo)).toThrow()
  })

  function node(arr: number[]): NormalExpressionNode {
    const astNodes: AstNode[] = arr.map(n => ({
      type: `Number`,
      value: n,
      token: { type: `name`, value: `X` },
    }))
    return {
      name: `let`,
      params: astNodes,
      type: `NormalExpression`,
      token: { type: `name`, value: `X` },
    }
  }

  test(`assertLengthEven`, () => {
    expect(() => assertEvenNumberOfParams(node([]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertEvenNumberOfParams(node([0]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertEvenNumberOfParams(node([0, 1]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertEvenNumberOfParams(node([0, 1, 2]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertEvenNumberOfParams(node([0, 1, 2, 3]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertEvenNumberOfParams(node([0, 1, 2, 3, 4]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertEvenNumberOfParams(node([0, 1, 2, 3, 4, 5]).params.length, `foo`, undefined)).not.toThrow()
  })

  test(`assertLength`, () => {
    expect(() => assertNumberOfParams(0, node([]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams(0, node([1]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams(1, node([1]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams(1, node([]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams(1, node([1, 2]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams(2, node([1, 2]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams(2, node([1]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams(2, node([1, 2, 3]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({}, node([]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2, 3, 4, 5]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2, 3, 4]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2, 3]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2, 3, 4, 5]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2, 3, 4]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2, 3]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1]).params.length, `foo`, undefined)).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2]).params.length, `foo`, undefined)).not.toThrow()
    expect(() =>
      assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3]).params.length, `foo`, undefined),
    ).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3, 4]).params.length, `foo`, undefined)).toThrow()
    expect(() =>
      assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3, 4, 5]).params.length, `foo`, undefined),
    ).toThrow()
    expect(() =>
      assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3, 4, 5, 6]).params.length, `foo`, undefined),
    ).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3]).params.length, `foo`, undefined)).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3, 4]).params.length, `foo`, undefined)).toThrow()
    expect(() =>
      assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3, 4, 5]).params.length, `foo`, undefined),
    ).toThrow()
    expect(() =>
      assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3, 4, 5, 6]).params.length, `foo`, undefined),
    ).toThrow()
  })

  test(`assertLitsFunction`, () => {
    const lf: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: `EOF`,
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
    expect(() => litsFunction.assert(lf, debugInfo)).not.toThrow()
    expect(() => litsFunction.assert(1, debugInfo)).toThrow()
    expect(() => litsFunction.assert({}, debugInfo)).toThrow()
  })
  test(`assertPositiveNumber`, () => {
    expect(() => number.assert(-1, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(-0.5, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(0, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(0.5, debugInfo, { positive: true })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { positive: true })).not.toThrow()
    expect(() => number.assert(`1`, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert([], debugInfo, { positive: true })).toThrow()
    expect(() => number.assert({}, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(true, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { positive: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { positive: true })).toThrow()
  })
  test(`assertNegativeNumber`, () => {
    expect(() => number.assert(-1, debugInfo, { negative: true })).not.toThrow()
    expect(() => number.assert(-0.5, debugInfo, { negative: true })).not.toThrow()
    expect(() => number.assert(0, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(0.5, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(1, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(`1`, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert([], debugInfo, { negative: true })).toThrow()
    expect(() => number.assert({}, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(true, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { negative: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { negative: true })).toThrow()
  })
  test(`assertNonNegativeNumber`, () => {
    expect(() => number.assert(-1, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(-1.1, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(0, debugInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(0.1, debugInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(1.1, debugInfo, { nonNegative: true })).not.toThrow()
    expect(() => number.assert(`1`, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert([], debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert({}, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(true, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { nonNegative: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { nonNegative: true })).toThrow()
  })
  test(`assertNonPositiveNumber`, () => {
    expect(() => number.assert(-1, debugInfo, { nonPositive: true })).not.toThrow()
    expect(() => number.assert(-1.1, debugInfo, { nonPositive: true })).not.toThrow()
    expect(() => number.assert(0, debugInfo, { nonPositive: true })).not.toThrow()
    expect(() => number.assert(0.1, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(1, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(1.1, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(`1`, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert([], debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert({}, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(true, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { nonPositive: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { nonPositive: true })).toThrow()
  })
  test(`assertFiniteNumber`, () => {
    expect(() => number.assert(-1, debugInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(-1.1, debugInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(0, debugInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(0.1, debugInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(1.1, debugInfo, { finite: true })).not.toThrow()
    expect(() => number.assert(Math.asin(2), debugInfo, { finite: true })).toThrow()
    expect(() => number.assert(1 / 0, debugInfo, { finite: true })).toThrow()
    expect(() => number.assert(`1`, debugInfo, { finite: true })).toThrow()
    expect(() => number.assert([], debugInfo, { finite: true })).toThrow()
    expect(() => number.assert({}, debugInfo, { finite: true })).toThrow()
    expect(() => number.assert(true, debugInfo, { finite: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { finite: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { finite: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { finite: true })).toThrow()
  })
  test(`asFiniteNumber`, () => {
    expect(number.as(-1, debugInfo, { finite: true })).toBe(-1)
    expect(number.as(-1.1, debugInfo, { finite: true })).toBe(-1.1)
    expect(number.as(0, debugInfo, { finite: true })).toBe(0)
    expect(number.as(0.1, debugInfo, { finite: true })).toBe(0.1)
    expect(number.as(1, debugInfo, { finite: true })).toBe(1)
    expect(number.as(1.1, debugInfo, { finite: true })).toBe(1.1)
    expect(() => number.as(Math.asin(2), debugInfo, { finite: true })).toThrow()
    expect(() => number.as(1 / 0, debugInfo, { finite: true })).toThrow()
    expect(() => number.as(`1`, debugInfo, { finite: true })).toThrow()
    expect(() => number.as(`1`, debugInfo, { finite: true })).toThrow()
    expect(() => number.as([], debugInfo, { finite: true })).toThrow()
    expect(() => number.as({}, debugInfo, { finite: true })).toThrow()
    expect(() => number.as(true, debugInfo, { finite: true })).toThrow()
    expect(() => number.as(false, debugInfo, { finite: true })).toThrow()
    expect(() => number.as(null, debugInfo, { finite: true })).toThrow()
    expect(() => number.as(undefined, debugInfo, { finite: true })).toThrow()
  })
  test(`assertNumberGt`, () => {
    expect(() => number.assert(0, debugInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(0.5, debugInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(1, debugInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(1.5, debugInfo, { gt: 1 })).not.toThrow()
    expect(() => number.assert(2, debugInfo, { gt: 1 })).not.toThrow()
    expect(() => number.assert(`2`, debugInfo, { gt: 1 })).toThrow()
    expect(() => number.assert([], debugInfo, { gt: 1 })).toThrow()
    expect(() => number.assert(false, debugInfo, { gt: 1 })).toThrow()
  })
  test(`assertNumberGte`, () => {
    expect(() => number.assert(0, debugInfo, { gte: 1 })).toThrow()
    expect(() => number.assert(0.5, debugInfo, { gte: 1 })).toThrow()
    expect(() => number.assert(1, debugInfo, { gte: 1 })).not.toThrow()
    expect(() => number.assert(1.5, debugInfo, { gte: 1 })).not.toThrow()
    expect(() => number.assert(2, debugInfo, { gte: 1 })).not.toThrow()
    expect(() => number.assert(`2`, debugInfo, { gte: 1 })).toThrow()
    expect(() => number.assert([], debugInfo, { gte: 1 })).toThrow()
    expect(() => number.assert(false, debugInfo, { gte: 1 })).toThrow()
  })
  test(`assertNumberLt`, () => {
    expect(() => number.assert(0, debugInfo, { lt: 1 })).not.toThrow()
    expect(() => number.assert(0.5, debugInfo, { lt: 1 })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(1.5, debugInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(2, debugInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(`2`, debugInfo, { lt: 1 })).toThrow()
    expect(() => number.assert([], debugInfo, { lt: 1 })).toThrow()
    expect(() => number.assert(false, debugInfo, { lt: 1 })).toThrow()
  })
  test(`assertNumberLte`, () => {
    expect(() => number.assert(0, debugInfo, { lte: 1 })).not.toThrow()
    expect(() => number.assert(0.5, debugInfo, { lte: 1 })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { lte: 1 })).not.toThrow()
    expect(() => number.assert(1.5, debugInfo, { lte: 1 })).toThrow()
    expect(() => number.assert(2, debugInfo, { lte: 1 })).toThrow()
    expect(() => number.assert(`2`, debugInfo, { lte: 1 })).toThrow()
    expect(() => number.assert([], debugInfo, { lte: 1 })).toThrow()
    expect(() => number.assert(false, debugInfo, { lte: 1 })).toThrow()
  })
  test(`assertNumberNotZero`, () => {
    expect(() => number.assert(-1, debugInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(-0.5, debugInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(0, debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(0.5, debugInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(1, debugInfo, { nonZero: true })).not.toThrow()
    expect(() => number.assert(`1`, debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert([], debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert({}, debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(true, debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(false, debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(null, debugInfo, { nonZero: true })).toThrow()
    expect(() => number.assert(undefined, debugInfo, { nonZero: true })).toThrow()
  })
  test(`assertString`, () => {
    expect(() => string.assert(``, debugInfo)).not.toThrow()
    expect(() => string.assert(`1`, debugInfo)).not.toThrow()
    expect(() => string.assert(0, debugInfo)).toThrow()
    expect(() => string.assert(1, debugInfo)).toThrow()
    expect(() => string.assert(true, debugInfo)).toThrow()
    expect(() => string.assert(false, debugInfo)).toThrow()
    expect(() => string.assert(null, debugInfo)).toThrow()
    expect(() => string.assert(undefined, debugInfo)).toThrow()
    expect(() => string.assert([], debugInfo)).toThrow()
    expect(() => string.assert({}, debugInfo)).toThrow()
  })
  test(`asString`, () => {
    expect(() => string.as(``, debugInfo)).not.toThrow()
    expect(() => string.as(`1`, debugInfo)).not.toThrow()
    expect(() => string.as(0, debugInfo)).toThrow()
    expect(() => string.as(1, debugInfo)).toThrow()
    expect(() => string.as(true, debugInfo)).toThrow()
    expect(() => string.as(false, debugInfo)).toThrow()
    expect(() => string.as(null, debugInfo)).toThrow()
    expect(() => string.as(undefined, debugInfo)).toThrow()
    expect(() => string.as([], debugInfo)).toThrow()
    expect(() => string.as({}, debugInfo)).toThrow()
  })
  test(`assertNonEmptyString`, () => {
    expect(() => string.assert(`1`, debugInfo, { nonEmpty: true })).not.toThrow()
    expect(() => string.assert(`abc`, debugInfo, { nonEmpty: true })).not.toThrow()
    expect(() => string.assert(``, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(0, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(1, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(true, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(false, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(null, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert(undefined, debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert([], debugInfo, { nonEmpty: true })).toThrow()
    expect(() => string.assert({}, debugInfo, { nonEmpty: true })).toThrow()
  })

  test(`assertStringOrArray`, () => {
    expect(() => sequence.assert(``, debugInfo)).not.toThrow()
    expect(() => sequence.assert(`1`, debugInfo)).not.toThrow()
    expect(() => sequence.assert([], debugInfo)).not.toThrow()
    expect(() => sequence.assert([1, 2, 3], debugInfo)).not.toThrow()
    expect(() => sequence.assert(0, debugInfo)).toThrow()
    expect(() => sequence.assert(1, debugInfo)).toThrow()
    expect(() => sequence.assert(true, debugInfo)).toThrow()
    expect(() => sequence.assert(false, debugInfo)).toThrow()
    expect(() => sequence.assert(null, debugInfo)).toThrow()
    expect(() => sequence.assert(undefined, debugInfo)).toThrow()
    expect(() => sequence.assert({}, debugInfo)).toThrow()
  })

  test(`assertStringOrRegExp`, () => {
    const a: RegularExpression = {
      [REGEXP_SYMBOL]: true,
      source: `^ab`,
      flags: ``,
    }
    expect(() => stringOrRegExp.assert(``, debugInfo)).not.toThrow()
    expect(() => stringOrRegExp.assert(`1`, debugInfo)).not.toThrow()
    expect(() => stringOrRegExp.assert(a, debugInfo)).not.toThrow()
    expect(() => stringOrRegExp.assert(/^a/, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert([], debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert([1, 2, 3], debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert(0, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert(1, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert(true, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert(false, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert(null, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert(undefined, debugInfo)).toThrow()
    expect(() => stringOrRegExp.assert({}, debugInfo)).toThrow()
  })

  test(`isLitsFunction`, () => {
    const lf1: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: `EOF`,
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
      debugInfo: `EOF`,
      type: `builtin`,
      name: `+`,
    }
    const lf3: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: `EOF`,
      type: `partial`,
      fn: { a: 10, b: 20 },
      params: [],
    }
    const lf4: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: `EOF`,
      type: `comp`,
      fns: [`x`],
    }
    const lf5: LitsFunction = {
      [FUNCTION_SYMBOL]: true,
      debugInfo: `EOF`,
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

  test(`assertNumber`, () => {
    expect(() => number.assert(1 / 0, debugInfo)).not.toThrow()
    expect(() => number.assert(Number(`abc`), debugInfo)).not.toThrow()
    expect(() => number.assert(0.12, debugInfo)).not.toThrow()
    expect(() => number.assert(undefined, debugInfo)).toThrow()
    expect(() => number.assert(`undefined`, debugInfo)).toThrow()
    expect(() => number.assert([], debugInfo)).toThrow()
  })

  test(`isRegexp`, () => {
    const a: RegularExpression = {
      [REGEXP_SYMBOL]: true,
      source: `^ab`,
      flags: ``,
    }

    expect(regularExpression.is(`Hej`)).toBe(false)
    expect(regularExpression.is({})).toBe(false)
    expect(regularExpression.is(a)).toBe(true)
  })

  test(`isNormalExpressionNodeName`, () => {
    expect(
      normalExpressionNodeWithName.is({
        type: `NormalExpression`,
        params: [],
        name: `object`,
        token: { type: `name`, value: `X` },
      }),
    ).toBe(true)
    expect(
      normalExpressionNodeWithName.is({
        type: `NormalExpression`,
        params: [],
        expression: {
          type: `NormalExpression`,
          name: `+`,
          params: [
            {
              type: `Number`,
              value: 2,
              token: { type: `name`, value: `X` },
            },
          ],
        },
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } as any),
    ).toBe(false)
    expect(
      normalExpressionNodeWithName.is({
        params: [],
        expression: {
          type: `NormalExpression`,
          name: `+`,
          params: [
            {
              type: `Number`,
              value: 2,
              token: { type: `name`, value: `X` },
            },
          ],
        },
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } as any),
    ).toBe(false)
  })

  test(`assertMax`, () => {
    expect(() => number.assert(12, debugInfo, { lte: 10 })).toThrow()
    expect(() => number.assert(-12, debugInfo, { lte: -10 })).not.toThrow()
    expect(() => number.assert(-8, debugInfo, { lte: -10 })).toThrow()
    expect(() => number.assert(10, debugInfo, { lte: 10 })).not.toThrow()
    expect(() => number.assert(0, debugInfo, { lte: 10 })).not.toThrow()
  })
  test(`assertChar`, () => {
    expect(() => string.assert(`2`, debugInfo, { char: true })).not.toThrow()
    expect(() => string.assert(`Albert`, debugInfo, { char: true })).toThrow()
    expect(() => string.assert(0, debugInfo, { char: true })).toThrow()
    expect(() => string.assert(null, debugInfo, { char: true })).toThrow()
    expect(() => string.assert(true, debugInfo, { char: true })).toThrow()
    expect(() => string.assert(false, debugInfo, { char: true })).toThrow()
    expect(() => string.assert([`a`], debugInfo, { char: true })).toThrow()
    expect(() => string.assert({ a: `a` }, debugInfo, { char: true })).toThrow()
  })
  test(`asChar`, () => {
    expect(string.as(`2`, debugInfo, { char: true })).toBe(`2`)
    expect(() => string.as(`Albert`, debugInfo, { char: true })).toThrow()
    expect(() => string.as(0, debugInfo, { char: true })).toThrow()
    expect(() => string.as(null, debugInfo, { char: true })).toThrow()
    expect(() => string.as(true, debugInfo, { char: true })).toThrow()
    expect(() => string.as(false, debugInfo, { char: true })).toThrow()
    expect(() => string.as([`a`], debugInfo, { char: true })).toThrow()
    expect(() => string.as({ a: `a` }, debugInfo, { char: true })).toThrow()
  })

  test(`asColl`, () => {
    expect(collection.as(`2`, debugInfo)).toEqual(`2`)
    expect(collection.as({ a: 1 }, debugInfo)).toEqual({ a: 1 })
    expect(collection.as([2], debugInfo)).toEqual([2])
    expect(() => collection.as(0, debugInfo)).toThrow()
    expect(() => collection.as(null, debugInfo)).toThrow()
    expect(() => collection.as(true, debugInfo)).toThrow()
    expect(() => collection.as(false, debugInfo)).toThrow()
  })

  test(`expressionNode`, () => {
    expect(expressionNode.is(`2`)).toBe(false)
  })

  test(`isAstNode`, () => {
    const node: AstNode = {
      type: `Name`,
      token: { debugInfo: `EOF`, type: `paren`, value: `(` },
      value: `A name`,
    }
    const nonNode = {
      ...node,
      type: `name`,
    }
    expect(astNode.is(node)).toBe(true)
    expect(astNode.is(nonNode)).toBe(false)
  })

  test(`number`, () => {
    expect(() => number.assert(0, `EOF`, { zero: true })).not.toThrow()
    expect(() => number.assert(1, `EOF`, { zero: true })).toThrow()
    expect(() => number.assert(1.5, `EOF`, { gt: 1, lt: 2 })).not.toThrow()
    expect(() => number.assert(1, `EOF`, { gt: 1, lt: 2 })).toThrow()
    expect(() => number.assert(2, `EOF`, { gt: 1, lt: 2 })).toThrow()
    expect(() => number.assert(1.5, `EOF`, { gte: 1, lte: 2 })).not.toThrow()
    expect(() => number.assert(1, `EOF`, { gte: 1, lte: 2 })).not.toThrow()
    expect(() => number.assert(2.5, `EOF`, { gte: 1, lte: 2 })).toThrow()
  })
  test(`character`, () => {
    expect(() => string.assert(`k`, `EOF`, { char: true })).not.toThrow()
    expect(() => string.assert(`k1`, `EOF`, { char: true })).toThrow()
    expect(() => string.assert(1, `EOF`, { char: true })).toThrow()
  })

  test(`token`, () => {
    const tkn: Token = {
      debugInfo: `EOF`,
      type: `name`,
      value: `Albert`,
    }
    const nonTkn = {
      ...tkn,
      type: `Name`,
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const nonTkn2: any = {
      ...tkn,
    }
    delete nonTkn2.type
    expect(token.is(tkn)).toBe(true)
    expect(token.is(nonTkn)).toBe(false)
    expect(() => token.assert(tkn, `EOF`)).not.toThrow()
    expect(() => token.assert(nonTkn, `EOF`)).toThrow()
    expect(() => token.assert(nonTkn2, `EOF`)).toThrow()
    expect(() => token.assert(tkn, `EOF`, { type: `name` })).not.toThrow()
    expect(() => token.assert(tkn, `EOF`, { type: `number` })).toThrow()
    expect(() => token.assert(tkn, `EOF`, { type: `name`, value: `Albert` })).not.toThrow()
    expect(() => token.assert(tkn, `EOF`, { type: `name`, value: `Mojir` })).toThrow()
  })
})
