import { LitsError } from '../errors'
import { Any, Arr, Coll, Obj, Seq } from '../interface'
import {
  AstNode,
  ExpressionNode,
  FUNCTION_SYMBOL,
  LitsFunction,
  NameNode,
  NodeType,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  SpecialExpressionNode,
} from '../parser/interface'
import { SourceCodeInfo } from '../tokenizer/interface'
import { isAstNode } from './astNodeAsserter'
import { string } from './stringAssertion'

export { number } from './numberAssertion'
export { token } from './tokenAssertion'
export { string } from './stringAssertion'

class Asserter<T> {
  private typeName: string
  private predicate: (value: unknown) => boolean
  constructor(typeName: string, predicate: (value: unknown) => boolean) {
    this.typeName = typeName
    this.predicate = predicate
  }

  public is(value: unknown): value is T {
    return this.predicate(value)
  }

  public assert(value: unknown, sourceCodeInfo: SourceCodeInfo): asserts value is T {
    if (!this.predicate(value)) {
      throw new LitsError(`Expected ${this.typeName}, got ${value}`, sourceCodeInfo)
    }
  }

  public as(value: unknown, sourceCodeInfo: SourceCodeInfo): T {
    this.assert(value, sourceCodeInfo)
    return value
  }
}

export function isLitsFunction(func: unknown): func is LitsFunction {
  if (func === null || typeof func !== `object`) {
    return false
  }
  return !!(func as LitsFunction)[FUNCTION_SYMBOL]
}
export const litsFunction: Asserter<LitsFunction> = new Asserter(`LitsFunction`, isLitsFunction)
export const stringOrNumber: Asserter<string | number> = new Asserter(
  `string or number`,
  value => typeof value === `string` || typeof value === `number`,
)
export const any: Asserter<Any> = new Asserter(`Any`, value => value !== undefined)
export const sequence: Asserter<Seq> = new Asserter(`Seq`, value => Array.isArray(value) || string.is(value))
export const object: Asserter<Obj> = new Asserter(
  `Obj`,
  value =>
    !(
      value === null ||
      typeof value !== `object` ||
      Array.isArray(value) ||
      value instanceof RegExp ||
      isLitsFunction(value)
    ),
)
export const collection: Asserter<Coll> = new Asserter(`Coll`, value => sequence.is(value) || object.is(value))
export const array: Asserter<Arr> = new Asserter(`Arr`, value => Array.isArray(value))
export const astNode: Asserter<AstNode> = new Asserter(`AstNode`, isAstNode)
export const nameNode: Asserter<NameNode> = new Asserter(`NameNode`, value => {
  if (!isAstNode(value)) {
    return false
  }
  const nodeType: NodeType = `Name`
  return value.type === nodeType
})
export const normalExpressionNodeWithName: Asserter<NormalExpressionNodeWithName> = new Asserter(
  `Normal expression node with name`,
  value => {
    if (!isAstNode(value)) {
      return false
    }
    const nodeType: NodeType = `NormalExpression`
    return value.type === nodeType && typeof value.name === `string`
  },
)

export const stringArray: Asserter<string[]> = new Asserter(
  `string array`,
  value => Array.isArray(value) && value.every(v => typeof v === `string`),
)
export const charArray: Asserter<string[]> = new Asserter(
  `character array`,
  value => Array.isArray(value) && value.every(v => typeof v === `string` && v.length === 1),
)
export const regExp: Asserter<RegExp> = new Asserter(`RegExp`, value => value instanceof RegExp)
export const stringOrRegExp: Asserter<string | RegExp> = new Asserter(
  `string or RegExp`,
  value => value instanceof RegExp || typeof value === `string`,
)
export const expressionNode: Asserter<ExpressionNode> = new Asserter(`expression node`, value => {
  if (!astNode.is(value)) {
    return false
  }
  return (
    value.type === `NormalExpression` ||
    value.type === `SpecialExpression` ||
    value.type === `Number` ||
    value.type === `String`
  )
})

export function assertNumberOfParams(
  count: number | { min?: number; max?: number },
  node: NormalExpressionNode | SpecialExpressionNode,
): void {
  const length = node.params.length
  if (typeof count === `number`) {
    if (length !== count) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected ${count}, got ${length}`,
        node.token.sourceCodeInfo,
      )
    }
  } else {
    const { min, max } = count
    if (min === undefined && max === undefined) {
      throw new LitsError(`Min or max must be specified`, node.token.sourceCodeInfo)
    }

    if (typeof min === `number` && length < min) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected at least ${min}, got ${length}`,
        node.token.sourceCodeInfo,
      )
    }

    if (typeof max === `number` && length > max) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected at most ${max}, got ${length}`,
        node.token.sourceCodeInfo,
      )
    }
  }
}

export function assertEventNumberOfParams(node: NormalExpressionNode): void {
  const length = node.params.length
  if (length % 2 !== 0) {
    throw new LitsError(`Wrong number of arguments, expected an even number, got ${length}`, node.token.sourceCodeInfo)
  }
}

export function asValue<T>(value: T | undefined, sourceCodeInfo: SourceCodeInfo): T {
  if (value === undefined) {
    throw new LitsError(`Unexpected nil`, sourceCodeInfo)
  }
  return value
}

export function assertValue<T>(value: T | undefined, sourceCodeInfo: SourceCodeInfo): asserts value is T {
  if (value === undefined) {
    throw new LitsError(`Unexpected nil`, sourceCodeInfo)
  }
}
