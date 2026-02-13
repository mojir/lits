import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../interface'
import type { Any } from '../../interface'
import type { FunctionReference } from '../../../reference'
import type { CoreNormalExpressionName } from '../../../reference/api'

// Core categories - always available
import { bitwiseNormalExpression } from './categories/bitwise'
import { collectionNormalExpression } from './categories/collection'
import { arrayNormalExpression } from './categories/array'
import { sequenceNormalExpression } from './categories/sequence'
import { mathNormalExpression } from './categories/math'
import { miscNormalExpression } from './categories/misc'
import { assertNormalExpression } from './categories/assert'
import { objectNormalExpression } from './categories/object'
import { predicatesNormalExpression } from './categories/predicates'
import { regexpNormalExpression } from './categories/regexp'
import { stringNormalExpression } from './categories/string'
import { functionalNormalExpression } from './categories/functional'
import { getMetaNormalExpression } from './categories/meta'

// TODO: Phase 1 - Namespaces commented out for refactoring
// These will require import() to use
// import { gridNormalExpression } from './categories/namespaces/grid'
// import { vectorNormalExpression } from './categories/namespaces/vector'
// import { linearAlgebraNormalExpression } from './categories/namespaces/linearAlgebra'
// import { matrixNormalExpression } from './categories/namespaces/matrix'
// import { combinatoricalNormalExpression } from './categories/namespaces/numberTheory'
// import { randomNormalExpression } from './categories/namespaces/random'

const normalExpressionReference: Record<string, FunctionReference> = {}

export function setNormalExpressionReference(reference: Record<CoreNormalExpressionName, FunctionReference>) {
  Object.assign(normalExpressionReference, reference)
}

const expressions: BuiltinNormalExpressions = {
  // Core categories
  ...bitwiseNormalExpression,
  ...collectionNormalExpression,
  ...arrayNormalExpression,
  ...sequenceNormalExpression,
  ...mathNormalExpression,
  ...getMetaNormalExpression(normalExpressionReference),
  ...miscNormalExpression,
  ...assertNormalExpression,
  ...objectNormalExpression,
  ...predicatesNormalExpression,
  ...regexpNormalExpression,
  ...stringNormalExpression,
  ...functionalNormalExpression,

  // TODO: Phase 1 - Namespaces commented out for refactoring
  // These will require import() to use
  // ...vectorNormalExpression,
  // ...linearAlgebraNormalExpression,
  // ...gridNormalExpression,
  // ...matrixNormalExpression,
  // ...combinatoricalNormalExpression,
  // ...randomNormalExpression,
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
