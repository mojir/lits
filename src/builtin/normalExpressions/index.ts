import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../interface'
import type { Any } from '../../interface'
import type { FunctionReference } from '../../../reference'
import type { CoreNormalExpressionName } from '../../../reference/api'

// Core categories - always available
import { assertionNormalExpression } from '../core/assertion'
import { bitwiseNormalExpression } from '../core/bitwise'
import { collectionNormalExpression } from '../core/collection'
import { arrayNormalExpression } from '../core/array'
import { sequenceNormalExpression } from '../core/sequence'
import { mathNormalExpression } from '../core/math'
import { miscNormalExpression } from '../core/misc'
import { objectNormalExpression } from '../core/object'
import { predicatesNormalExpression } from '../core/predicates'
import { regexpNormalExpression } from '../core/regexp'
import { stringNormalExpression } from '../core/string'
import { functionalNormalExpression } from '../core/functional'
import { getMetaNormalExpression } from '../core/meta'
import { vectorNormalExpression } from '../core/vector'

const normalExpressionReference: Record<string, FunctionReference> = {}

export function setNormalExpressionReference(reference: Record<CoreNormalExpressionName, FunctionReference>) {
  Object.assign(normalExpressionReference, reference)
}

const expressions: BuiltinNormalExpressions = {
  // Core categories
  ...assertionNormalExpression,
  ...bitwiseNormalExpression,
  ...collectionNormalExpression,
  ...arrayNormalExpression,
  ...sequenceNormalExpression,
  ...mathNormalExpression,
  ...getMetaNormalExpression(normalExpressionReference),
  ...miscNormalExpression,
  ...objectNormalExpression,
  ...predicatesNormalExpression,
  ...regexpNormalExpression,
  ...stringNormalExpression,
  ...functionalNormalExpression,
  ...vectorNormalExpression,
}

Object.entries(expressions).forEach(([name, expression]) => {
  expression.name = name
})

export const normalExpressions: BuiltinNormalExpressions = {
  ...expressions,
}

export const normalExpressionTypes: Record<string, number> = {}
export const allNormalExpressions: BuiltinNormalExpression<Any>[] = []

Object.entries(normalExpressions).forEach(([key, value], index) => {
  normalExpressionTypes[key] = index
  allNormalExpressions.push(value)
})
