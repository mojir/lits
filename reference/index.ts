import { isUnknownRecord } from '../src/typeGuards'
import { collectionReference } from './categories/collection'
import { functionalReference } from './categories/functional'
import { arrayReference } from './categories/array'
import { sequenceReference } from './categories/sequence'
import { mathReference } from './categories/math'
import { miscReference } from './categories/misc'
import { assertReference } from './categories/assert'
import { objectReference } from './categories/object'
import { predicateReference } from './categories/predicate'
import { regularExpressionReference } from './categories/regularExpression'
import { specialExpressionsReference } from './categories/specialExpressions'
import { stringReference } from './categories/string'
import { bitwiseReference } from './categories/bitwise'
import { shorthand } from './shorthand'
import { datatype } from './datatype'
import type { ApiName, Category, DataType, FunctionName } from './api'

export interface TypedValue {
  type: DataType[] | DataType
  rest?: true
  array?: true
}

type NormalExpressionArgument = TypedValue & {
  description?: string
}
interface SpecialExpressionArgument {
  type: '*expression' | '*name' | '*binding' | '*arguments' | '*catch-expression' | '*conditions' | '*for-binding'
  rest?: true
  array?: true
  description?: string
}

export type Argument = NormalExpressionArgument | SpecialExpressionArgument

export function isSpecialExpressionArgument(arg?: Argument): arg is SpecialExpressionArgument {
  return isUnknownRecord(arg) && typeof arg.type === 'string' && arg.type.startsWith('*')
}

export function isNormalExpressionArgument(arg?: Argument): arg is NormalExpressionArgument {
  return isUnknownRecord(arg) && !isSpecialExpressionArgument(arg)
}

export function isTypedValue(arg?: Argument): arg is TypedValue {
  return isUnknownRecord(arg) && !isSpecialExpressionArgument(arg)
}

interface Variant {
  argumentNames: string[]
}

export interface CommonReference<T extends Category> {
  title: string
  category: T
  linkName: string
  examples: string[]
  description: string
  clojureDocs?: string | null
  seeAlso?: ApiName[]
  algebraic?: boolean
  operator?: boolean
}
export interface FunctionReference<T extends Category = Category> extends CommonReference<T> {
  returns: TypedValue
  args: Record<string, Argument>
  variants: Variant[]
}

export interface ShorthandReference extends CommonReference<'Shorthand'> {
  shorthand: true
  linkName: `-short-${string}`
}

export interface DatatypeReference extends CommonReference<'Datatype'> {
  datatype: true
  linkName: `-type-${string}`
}

export type Reference<T extends Category = Category> = FunctionReference<T> | ShorthandReference | DatatypeReference

export function isFunctionReference<T extends Category>(ref: Reference<T>): ref is FunctionReference<T> {
  return 'returns' in ref && 'args' in ref && 'variants' in ref
}

export function isShorthandReference<T extends Category>(ref: Reference<T>): ref is ShorthandReference {
  return 'shorthand' in ref
}

export function isDatatypeReference<T extends Category>(ref: Reference<T>): ref is DatatypeReference {
  return 'datatype' in ref
}

export const functionReference: Record<FunctionName, FunctionReference> = {
  ...collectionReference,
  ...arrayReference,
  ...sequenceReference,
  ...mathReference,
  ...functionalReference,
  ...miscReference,
  ...objectReference,
  ...predicateReference,
  ...regularExpressionReference,
  ...specialExpressionsReference,
  ...stringReference,
  ...bitwiseReference,
  ...assertReference,
}

export const apiReference: Record<ApiName, Reference> = { ...functionReference, ...shorthand, ...datatype }

Object.values(apiReference).forEach((ref) => {
  ref.title = ref.title.replace(/"/g, '&quot;')
})
