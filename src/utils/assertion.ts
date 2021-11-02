import { LitsError } from '../errors'
import { Any, Arr, Coll, Obj, Seq } from '../interface'
import { AstNode, FUNCTION_SYMBOL, LitsFunction, NameNode, NodeType } from '../parser/interface'
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
