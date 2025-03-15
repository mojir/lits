import { builtin } from '../src/builtin'
import type { Count } from '../src/builtin/interface'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { isSymbolicOperator } from '../src/tokenizer/operators'
import { canBeOperator, isUnknownRecord } from '../src/typeGuards'
import type { ApiName, Category, DataType, NormalExpressionName } from './api'
import { arrayReference } from './categories/array'
import { assertReference } from './categories/assert'
import { bitwiseReference } from './categories/bitwise'
import { collectionReference } from './categories/collection'
import { functionalReference } from './categories/functional'
import { mathReference } from './categories/math'
import { miscReference } from './categories/misc'
import { objectReference } from './categories/object'
import { predicateReference } from './categories/predicate'
import { regularExpressionReference } from './categories/regularExpression'
import { sequenceReference } from './categories/sequence'
import { specialExpressionsReference } from './categories/specialExpressions'
import { stringReference } from './categories/string'
import { datatype } from './datatype'
import { shorthand } from './shorthand'

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
}
export type FunctionReference<T extends Category = Category> = CommonReference<T> & {
  returns: TypedValue
  args: Record<string, Argument>
  variants: Variant[]
  aliases?: string[]
  noOperatorDocumentation?: true
  _isOperator?: boolean
  _prefereOperator?: boolean
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

export const normalExpressionReference: Record<NormalExpressionName, FunctionReference> = {
  ...collectionReference,
  ...arrayReference,
  ...sequenceReference,
  ...mathReference,
  ...functionalReference,
  ...miscReference,
  ...objectReference,
  ...predicateReference,
  ...regularExpressionReference,
  ...stringReference,
  ...bitwiseReference,
  ...assertReference,
}

Object.entries(normalExpressionReference).forEach(([key, obj]) => {
  const paramCount = normalExpressions[key]!.paramCount
  if (!obj.noOperatorDocumentation && canBeOperator(paramCount)) {
    obj._isOperator = true
    if (isSymbolicOperator(key)) {
      obj._prefereOperator = true
    }
  }
})

Object.entries(specialExpressionsReference).forEach(([key, obj]) => {
  const specialExpressions = builtin.specialExpressions as Record<string, { paramCount: Count }>
  const paramCount = specialExpressions[key]?.paramCount
  if (paramCount && canBeOperator(paramCount)) {
    obj._isOperator = true
  }
})

export const functionReference = {
  ...normalExpressionReference,
  ...specialExpressionsReference,
}

export const apiReference: Record<ApiName, Reference> = { ...functionReference, ...shorthand, ...datatype }

Object.values(apiReference).forEach((ref) => {
  ref.title = ref.title.replace(/"/g, '&quot;')
})
