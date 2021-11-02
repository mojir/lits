import { AstNode, FUNCTION_SYMBOL, LitsFunction, NameNode, NormalExpressionNode } from '../src/parser/interface'
import { SourceCodeInfo, Token } from '../src/tokenizer/interface'
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
  regExp,
  assertEventNumberOfParams,
  assertNumberOfParams,
  stringOrRegExp,
  normalExpressionNodeWithName,
  expressionNode,
  astNode,
  token,
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
    expect(() => regExp.assert(/a/, sourceCodeInfo)).not.toThrow()
    expect(() => regExp.assert(new RegExp(`a`), sourceCodeInfo)).not.toThrow()
    expect(() => regExp.assert(0, sourceCodeInfo)).toThrow()
    expect(() => regExp.assert(`0`, sourceCodeInfo)).toThrow()
    expect(() => regExp.assert(null, sourceCodeInfo)).toThrow()
    expect(() => regExp.assert(undefined, sourceCodeInfo)).toThrow()
    expect(() => regExp.assert(false, sourceCodeInfo)).toThrow()
    expect(() => regExp.assert(true, sourceCodeInfo)).toThrow()
    expect(() => regExp.assert([], sourceCodeInfo)).toThrow()
    expect(() => regExp.assert({}, sourceCodeInfo)).toThrow()
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
    expect(() => assertEventNumberOfParams(node([]))).not.toThrow()
    expect(() => assertEventNumberOfParams(node([0]))).toThrow()
    expect(() => assertEventNumberOfParams(node([0, 1]))).not.toThrow()
    expect(() => assertEventNumberOfParams(node([0, 1, 2]))).toThrow()
    expect(() => assertEventNumberOfParams(node([0, 1, 2, 3]))).not.toThrow()
    expect(() => assertEventNumberOfParams(node([0, 1, 2, 3, 4]))).toThrow()
    expect(() => assertEventNumberOfParams(node([0, 1, 2, 3, 4, 5]))).not.toThrow()
  })

  test(`assertLength`, () => {
    expect(() => assertNumberOfParams(0, node([]))).not.toThrow()
    expect(() => assertNumberOfParams(0, node([1]))).toThrow()
    expect(() => assertNumberOfParams(1, node([1]))).not.toThrow()
    expect(() => assertNumberOfParams(1, node([]))).toThrow()
    expect(() => assertNumberOfParams(1, node([1, 2]))).toThrow()
    expect(() => assertNumberOfParams(2, node([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams(2, node([1]))).toThrow()
    expect(() => assertNumberOfParams(2, node([1, 2, 3]))).toThrow()
    expect(() => assertNumberOfParams({}, node([]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2, 3, 4, 5]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2, 3, 4]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([1]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1 }, node([]))).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([1]))).not.toThrow()
    expect(() => assertNumberOfParams({ max: 3 }, node([]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3]))).not.toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertNumberOfParams({ min: 1, max: 3 }, node([1, 2, 3, 4, 5, 6]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3, 4]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3, 4, 5]))).toThrow()
    expect(() => assertNumberOfParams({ min: 3, max: 1 }, node([1, 2, 3, 4, 5, 6]))).toThrow()
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
    expect(() => stringOrRegExp.assert(``, sourceCodeInfo)).not.toThrow()
    expect(() => stringOrRegExp.assert(`1`, sourceCodeInfo)).not.toThrow()
    expect(() => stringOrRegExp.assert(/^a/, sourceCodeInfo)).not.toThrow()
    expect(() => stringOrRegExp.assert([], sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert([1, 2, 3], sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert(0, sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert(1, sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert(true, sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert(false, sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert(null, sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert(undefined, sourceCodeInfo)).toThrow()
    expect(() => stringOrRegExp.assert({}, sourceCodeInfo)).toThrow()
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

  test(`assertNumber`, () => {
    expect(() => number.assert(1 / 0, sourceCodeInfo)).not.toThrow()
    expect(() => number.assert(Number(`abc`), sourceCodeInfo)).not.toThrow()
    expect(() => number.assert(0.12, sourceCodeInfo)).not.toThrow()
    expect(() => number.assert(undefined, sourceCodeInfo)).toThrow()
    expect(() => number.assert(`undefined`, sourceCodeInfo)).toThrow()
    expect(() => number.assert([], sourceCodeInfo)).toThrow()
  })

  test(`isRegexp`, () => {
    expect(regExp.is(`Hej`)).toBe(false)
    expect(regExp.is({})).toBe(false)
    expect(regExp.is(/^a/)).toBe(true)
  })

  test(`isNormalExpressionNodeName`, () => {
    expect(
      normalExpressionNodeWithName.is({
        type: `NormalExpression`,
        params: [],
        name: `object`,
        token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
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
              token: { type: `name`, sourceCodeInfo: { line: 0, column: 0, sourceCodeLine: null }, value: `X` },
            },
          ],
        },
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } as any),
    ).toBe(false)
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

  test(`expressionNode`, () => {
    expect(expressionNode.is(`2`)).toBe(false)
  })

  test(`isAstNode`, () => {
    const node: AstNode = {
      type: `Name`,
      token: { sourceCodeInfo: `EOF`, type: `paren`, value: `(` },
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
      sourceCodeInfo: `EOF`,
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
    delete nonTkn2.sourceCodeInfo
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
