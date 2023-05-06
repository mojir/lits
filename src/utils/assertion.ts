import { LitsError } from '../errors'
import { Any, Arr, Coll, Obj, Seq } from '../interface'
import {
  AstNode,
  ExpressionNode,
  LitsFunction,
  NameNode,
  NodeType,
  NormalExpressionNode,
  NormalExpressionNodeWithName,
  RegularExpression,
  SpecialExpressionNode,
} from '../parser/interface'
import { DebugInfo } from '../tokenizer/interface'
import { getDebugInfo, isAstNode, isLitsFunction, isRegularExpression, valueToString } from './helpers'
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

  public assert(value: unknown, debugInfo?: DebugInfo): asserts value is T {
    if (!this.predicate(value)) {
      throw new LitsError(`Expected ${this.typeName}, got ${valueToString(value)}.`, getDebugInfo(value, debugInfo))
    }
  }

  public as(value: unknown, debugInfo?: DebugInfo): T {
    this.assert(value, debugInfo)
    return value
  }
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
      isLitsFunction(value) ||
      isRegularExpression(value)
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
export const regularExpression: Asserter<RegularExpression> = new Asserter(`regularExpression`, isRegularExpression)
export const stringOrRegExp: Asserter<string | RegularExpression> = new Asserter(
  `string or regularExpression`,
  value => isRegularExpression(value) || typeof value === `string`,
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
  const debugInfo = node.token?.debugInfo
  if (typeof count === `number`) {
    if (length !== count) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected ${count}, got ${valueToString(length)}.`,
        node.token?.debugInfo,
      )
    }
  } else {
    const { min, max } = count
    if (min === undefined && max === undefined) {
      throw new LitsError(`Min or max must be specified.`, debugInfo)
    }

    if (typeof min === `number` && length < min) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected at least ${min}, got ${valueToString(length)}.`,
        debugInfo,
      )
    }

    if (typeof max === `number` && length > max) {
      throw new LitsError(
        `Wrong number of arguments to "${node.name}", expected at most ${max}, got ${valueToString(length)}.`,
        debugInfo,
      )
    }
  }
}

export function assertEventNumberOfParams(node: NormalExpressionNode): void {
  const length = node.params.length
  if (length % 2 !== 0) {
    throw new LitsError(
      `Wrong number of arguments, expected an even number, got ${valueToString(length)}.`,
      node.token?.debugInfo,
    )
  }
}

export function asValue<T>(value: T | undefined, debugInfo?: DebugInfo): T {
  if (value === undefined) {
    throw new LitsError(`Unexpected nil`, getDebugInfo(value, debugInfo))
  }
  return value
}

export function assertValue<T>(value: T | undefined, debugInfo?: DebugInfo): asserts value is T {
  if (value === undefined) {
    throw new LitsError(`Unexpected nil.`, getDebugInfo(value, debugInfo))
  }
}
