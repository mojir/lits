import type { SpecialExpressionName } from '../src/builtin'
import { specialExpressions } from '../src/builtin'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { specialExpressionTypes } from '../src/builtin/specialExpressionTypes'
import { isSymbolicOperator } from '../src/tokenizer/operators'
import { canBeOperator } from '../src/utils/arity'
import type { ApiName, Category, DataType, NormalExpressionName } from './api'
import { arrayReference } from './categories/array'
import { assertReference } from './categories/assert'
import { bitwiseReference } from './categories/bitwise'
import { collectionReference } from './categories/collection'
import { functionalReference } from './categories/functional'
import { gridReference } from './categories/grid'
import { linAlgReference } from './categories/linearAlgebra'
import { mathReference } from './categories/math'
import { matrixReference } from './categories/matrix'
import { metaReference } from './categories/meta'
import { miscReference } from './categories/misc'
import { numberTheoryReference } from './categories/numberTheory'
import { objectReference } from './categories/object'
import { predicateReference } from './categories/predicate'
import { randomReference } from './categories/random'
import { regularExpressionReference } from './categories/regularExpression'
import { sequenceReference } from './categories/sequence'
import { specialExpressionsReference } from './categories/specialExpressions'
import { stringReference } from './categories/string'
import { vectorReference } from './categories/vector'
import { datatype } from './datatype'
import { shorthand } from './shorthand'

export interface TypedValue {
  type: DataType[] | DataType
  rest?: true
  array?: true
}

export type NormalExpressionArgument = TypedValue & {
  description?: string
}

export type Argument = NormalExpressionArgument

interface Variant {
  argumentNames: string[]
}

export interface CommonReference<T extends Category> {
  title: string
  category: T
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
}

export interface DatatypeReference extends CommonReference<'Datatype'> {
  datatype: true
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
  ...metaReference,
  ...miscReference,
  ...objectReference,
  ...predicateReference,
  ...regularExpressionReference,
  ...stringReference,
  ...bitwiseReference,
  ...assertReference,
  ...vectorReference,
  ...linAlgReference,
  ...matrixReference,
  ...numberTheoryReference,
  ...gridReference,
  ...randomReference,
}

Object.entries(normalExpressionReference).forEach(([key, obj]) => {
  if (!normalExpressions[key]) {
    throw new Error(`Missing normal expression ${key} in normalExpressions`)
  }
  const arity = normalExpressions[key].arity
  if (!obj.noOperatorDocumentation && canBeOperator(arity)) {
    obj._isOperator = true
    if (isSymbolicOperator(key)) {
      obj._prefereOperator = true
    }
  }
})

Object.entries(specialExpressionsReference).forEach(([key, obj]) => {
  if (isFunctionReference(obj)) {
    const arity = specialExpressions[specialExpressionTypes[key as SpecialExpressionName]]?.arity
    if (arity && canBeOperator(arity)) {
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

export function getLinkName(reference: Reference): string {
  return encodeURIComponent(`${reference.category}-${reference.title}`)
}
