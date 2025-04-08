import type { SpecialExpressionName } from '../src/builtin'
import { specialExpressions } from '../src/builtin'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { specialExpressionTypes } from '../src/builtin/specialExpressionTypes'
import { isSymbolicOperator } from '../src/tokenizer/operators'
import { canBeOperator } from '../src/typeGuards'
import type { ApiName, Category, DataType, NormalExpressionName } from './api'
import { arrayReference } from './categories/array'
import { assertReference } from './categories/assert'
import { bitwiseReference } from './categories/bitwise'
import { collectionReference } from './categories/collection'
import { functionalReference } from './categories/functional'
import { mathReference } from './categories/math'
import { miscReference } from './categories/misc'
import { objectReference } from './categories/object'
import { matrixReference } from './categories/matrix'
import { predicateReference } from './categories/predicate'
import { regularExpressionReference } from './categories/regularExpression'
import { sequenceReference } from './categories/sequence'
import { specialExpressionsReference } from './categories/specialExpressions'
import { stringReference } from './categories/string'
import { datatype } from './datatype'
import { shorthand } from './shorthand'
import { numberTheoryReference } from './categories/numberTheory'

export interface TypedValue {
  type: DataType[] | DataType
  rest?: true
  array?: true
}

type NormalExpressionArgument = TypedValue & {
  description?: string
}

export type Argument = NormalExpressionArgument

interface Variant {
  argumentNames: string[]
}

export interface CommonReference<T extends Category> {
  title: string
  category: T
  linkName: string
  examples: string[]
  description: string
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

export type CustomReference<T extends Category = Category> = CommonReference<T> & {
  customVariants: string[]
  details?: [string, string, string | undefined][]
}

export interface ShorthandReference extends CommonReference<'Shorthand'> {
  shorthand: true
  linkName: `-short-${string}`
}

export interface DatatypeReference extends CommonReference<'Datatype'> {
  datatype: true
  linkName: `-type-${string}`
}

export type Reference<T extends Category = Category> = FunctionReference<T> | CustomReference<T> | ShorthandReference | DatatypeReference

export function isFunctionReference<T extends Category>(ref: Reference<T>): ref is FunctionReference<T> {
  return 'returns' in ref && 'args' in ref && 'variants' in ref
}

export function isCustomReference<T extends Category>(ref: Reference<T>): ref is CustomReference<T> {
  return 'customVariants' in ref
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
  ...matrixReference,
  ...numberTheoryReference,
}

Object.entries(normalExpressionReference).forEach(([key, obj]) => {
  if (!normalExpressions[key]) {
    throw new Error(`Missing normal expression ${key} in normalExpressions`)
  }
  const paramCount = normalExpressions[key].paramCount
  if (!obj.noOperatorDocumentation && canBeOperator(paramCount)) {
    obj._isOperator = true
    if (isSymbolicOperator(key)) {
      obj._prefereOperator = true
    }
  }
})

Object.entries(specialExpressionsReference).forEach(([key, obj]) => {
  if (isFunctionReference(obj)) {
    const paramCount = specialExpressions[specialExpressionTypes[key as SpecialExpressionName]]?.paramCount
    if (paramCount && canBeOperator(paramCount)) {
      obj._isOperator = true
    }
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
