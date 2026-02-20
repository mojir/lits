import type { SpecialExpressionName } from '../src/builtin'
import { specialExpressions } from '../src/builtin'
import { normalExpressions } from '../src/builtin/normalExpressions'
import { specialExpressionTypes } from '../src/builtin/specialExpressionTypes'
import { isSymbolicOperator } from '../src/tokenizer/operators'
import { canBeOperator } from '../src/utils/arity'
import type { BuiltinNormalExpressions, FunctionDocs, SpecialExpressionDocs } from '../src/builtin/interface'
import { isFunctionDocs } from '../src/builtin/interface'

// Core categories — all derive reference from co-located docs
import { bitwiseNormalExpression } from '../src/builtin/core/bitwise'
import { arrayNormalExpression } from '../src/builtin/core/array'
import { collectionNormalExpression } from '../src/builtin/core/collection'
import { functionalNormalExpression } from '../src/builtin/core/functional'
import { mathNormalExpression } from '../src/builtin/core/math'
import { getMetaNormalExpression } from '../src/builtin/core/meta'
import { miscNormalExpression } from '../src/builtin/core/misc'
import { objectNormalExpression } from '../src/builtin/core/object'
import { predicatesNormalExpression } from '../src/builtin/core/predicates'
import { regexpNormalExpression } from '../src/builtin/core/regexp'
import { sequenceNormalExpression } from '../src/builtin/core/sequence'
import { stringNormalExpression } from '../src/builtin/core/string'

// Module categories — derive reference from co-located docs
import { assertModule } from '../src/builtin/modules/assert'
import { gridModule } from '../src/builtin/modules/grid'
import { randomModule } from '../src/builtin/modules/random'
import { vectorModule } from '../src/builtin/modules/vector'
import { linearAlgebraModule } from '../src/builtin/modules/linearAlgebra'
import { matrixModule } from '../src/builtin/modules/matrix'
import { numberTheoryModule } from '../src/builtin/modules/numberTheory'
import { stringUtilsModule } from '../src/builtin/modules/stringUtils'
import { collectionUtilsModule } from '../src/builtin/modules/collectionUtils'
import type { ApiName, ArrayApiName, BitwiseApiName, Category, CollectionApiName, CoreApiName, CoreNormalExpressionName, DataType, FunctionalApiName, MathApiName, MetaApiName, MiscApiName, ModuleExpressionName, ObjectApiName, PredicateApiName, RegularExpressionApiName, SequenceApiName, StringApiName } from './api'
import { datatype } from './datatype'
import { shorthand } from './shorthand'

// --- Helper: derive FunctionReference from co-located docs ---

function docsToReference(expressions: BuiltinNormalExpressions): Record<string, FunctionReference> {
  const result: Record<string, FunctionReference> = {}
  for (const [key, expr] of Object.entries(expressions)) {
    const docs: FunctionDocs | undefined = expr.docs
    if (!docs) {
      throw new Error(`Missing docs for expression "${key}"`)
    }
    result[key] = {
      title: key,
      category: docs.category,
      description: docs.description,
      returns: docs.returns,
      args: docs.args,
      variants: docs.variants,
      examples: docs.examples,
      ...(docs.seeAlso ? { seeAlso: docs.seeAlso as ApiName[] } : {}),
      ...(docs.hideOperatorForm ? { noOperatorDocumentation: true } : {}),
    }
  }
  return result
}

// --- Helper: derive FunctionReference from module co-located docs ---

function moduledDocsToReference(moduleName: string, expressions: BuiltinNormalExpressions): Record<string, FunctionReference> {
  const result: Record<string, FunctionReference> = {}
  for (const [key, expr] of Object.entries(expressions)) {
    const docs: FunctionDocs | undefined = expr.docs
    if (!docs) {
      throw new Error(`Missing docs for ${moduleName}.${key}`)
    }
    const qualifiedKey = `${moduleName}.${key}`
    result[qualifiedKey] = {
      title: qualifiedKey,
      category: docs.category,
      description: docs.description,
      returns: docs.returns,
      args: docs.args,
      variants: docs.variants,
      examples: docs.examples,
      ...(docs.seeAlso ? { seeAlso: docs.seeAlso as ApiName[] } : {}),
      ...(docs.hideOperatorForm ? { noOperatorDocumentation: true } : {}),
    }
  }
  return result
}

// Derive all core category references from co-located docs
const bitwiseReference = docsToReference(bitwiseNormalExpression) as Record<BitwiseApiName, FunctionReference<'Bitwise'>>
const arrayRef = docsToReference(arrayNormalExpression) as Record<ArrayApiName, FunctionReference<'Array'>>
const collectionRef = docsToReference(collectionNormalExpression) as Record<CollectionApiName, FunctionReference<'Collection'>>
const functionalRef = docsToReference(functionalNormalExpression) as Record<FunctionalApiName, FunctionReference<'Functional'>>
const mathRef = docsToReference(mathNormalExpression) as Record<MathApiName, FunctionReference<'Math'>>
const emptyRef: Record<string, FunctionReference> = {}
const metaRef = docsToReference(getMetaNormalExpression(emptyRef)) as Record<MetaApiName, FunctionReference<'Meta'>>
const miscRef = docsToReference(miscNormalExpression) as Record<MiscApiName, FunctionReference<'Misc'>>
const objectRef = docsToReference(objectNormalExpression) as Record<ObjectApiName, FunctionReference<'Object'>>
const predicatesRef = docsToReference(predicatesNormalExpression) as Record<PredicateApiName, FunctionReference<'Predicate'>>
const regexpRef = docsToReference(regexpNormalExpression) as Record<RegularExpressionApiName, FunctionReference<'Regular expression'>>
const sequenceRef = docsToReference(sequenceNormalExpression) as Record<SequenceApiName, FunctionReference<'Sequence'>>
const stringRef = docsToReference(stringNormalExpression) as Record<StringApiName, FunctionReference<'String'>>

// --- Helper: derive special expression reference from co-located docs ---

function specialExpressionDocsToReference(): Record<string, FunctionReference<'Special expression'> | CustomReference<'Special expression'>> {
  const result: Record<string, FunctionReference<'Special expression'> | CustomReference<'Special expression'>> = {}
  for (const [name, index] of Object.entries(specialExpressionTypes)) {
    const expr = specialExpressions[index]
    const docs: SpecialExpressionDocs | undefined = expr?.docs
    if (!docs) {
      continue // skip undocumented special expressions
    }
    if (isFunctionDocs(docs)) {
      result[name] = {
        title: name,
        category: docs.category as 'Special expression',
        description: docs.description,
        returns: docs.returns,
        args: docs.args,
        variants: docs.variants,
        examples: docs.examples,
        ...(docs.seeAlso ? { seeAlso: docs.seeAlso as ApiName[] } : {}),
        ...(docs.hideOperatorForm ? { noOperatorDocumentation: true } : {}),
      }
    }
    else {
      result[name] = {
        title: name,
        category: docs.category as 'Special expression',
        description: docs.description,
        customVariants: docs.customVariants,
        ...(docs.details ? { details: docs.details } : {}),
        ...(docs.returns ? { returns: docs.returns } : {}),
        examples: docs.examples,
        ...(docs.seeAlso ? { seeAlso: docs.seeAlso as ApiName[] } : {}),
      }
    }
  }
  return result
}

const specialExpressionsReference = specialExpressionDocsToReference()

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

export const normalExpressionReference: Record<CoreNormalExpressionName, FunctionReference> = {
  // Core categories — all derived from co-located docs
  ...bitwiseReference,
  ...collectionRef,
  ...arrayRef,
  ...sequenceRef,
  ...mathRef,
  ...functionalRef,
  ...metaRef,
  ...miscRef,
  ...objectRef,
  ...predicatesRef,
  ...regexpRef,
  ...stringRef,
}

// Module functions — all derived from co-located docs
// eslint-disable-next-line ts/consistent-type-assertions
export const moduleReference: Record<ModuleExpressionName, FunctionReference> = {
  ...moduledDocsToReference(assertModule.name, assertModule.functions),
  ...moduledDocsToReference(gridModule.name, gridModule.functions),
  ...moduledDocsToReference(randomModule.name, randomModule.functions),
  ...moduledDocsToReference(vectorModule.name, vectorModule.functions),
  ...moduledDocsToReference(linearAlgebraModule.name, linearAlgebraModule.functions),
  ...moduledDocsToReference(matrixModule.name, matrixModule.functions),
  ...moduledDocsToReference(numberTheoryModule.name, numberTheoryModule.functions),
  ...moduledDocsToReference(stringUtilsModule.name, stringUtilsModule.functions),
  ...moduledDocsToReference(collectionUtilsModule.name, collectionUtilsModule.functions),
} as Record<ModuleExpressionName, FunctionReference>

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

// Core API reference (always available)
export const apiReference: Record<CoreApiName, Reference> = { ...functionReference, ...shorthand, ...datatype }

// All references including modules (for search and full documentation)
export const allReference: Record<ApiName, Reference> = { ...apiReference, ...moduleReference }

Object.values(allReference).forEach((ref) => {
  ref.title = ref.title.replace(/"/g, '&quot;')
})

export function getLinkName(reference: Reference): string {
  return encodeURIComponent(`${reference.category}-${reference.title}`)
}
