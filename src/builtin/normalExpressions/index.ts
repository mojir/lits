import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../interface'
import type { Any } from '../../interface'
import type { FunctionReference } from '../../../reference'
import type { NormalExpressionName } from '../../../reference/api'
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
import { gridNormalExpression } from './categories/namespaces/grid'
import { vectorNormalExpression } from './categories/namespaces/vector'
import { linearAlgebraNormalExpression } from './categories/namespaces/linearAlgebra'
import { matrixNormalExpression } from './categories/namespaces/matrix'
import { combinatoricalNormalExpression } from './categories/namespaces/numberTheory'
import { randomNormalExpression } from './categories/namespaces/random'
import { getMetaNormalExpression } from './categories/meta'

const normalExpressionReference: Record<string, FunctionReference> = {}

export function setNormalExpressionReference(reference: Record<NormalExpressionName, FunctionReference>) {
  Object.assign(normalExpressionReference, reference)
}

const expressions: BuiltinNormalExpressions = {
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
  ...vectorNormalExpression,
  ...linearAlgebraNormalExpression,
  ...gridNormalExpression,
  ...matrixNormalExpression,
  ...combinatoricalNormalExpression,
  ...randomNormalExpression,
}

Object.entries(expressions).forEach(([name, expression]) => {
  expression.name = name
})

const aliases: BuiltinNormalExpressions = {}

Object.values(expressions).forEach((normalExpression) => {
  normalExpression.aliases?.forEach((alias) => {
    aliases[alias] = normalExpression
  })
})

export const normalExpressions: BuiltinNormalExpressions = {
  ...expressions,
  ...aliases,
}

export const normalExpressionTypes: Record<string, number> = {}
export const allNormalExpressions: BuiltinNormalExpression<Any>[] = []

Object.entries(normalExpressions).forEach(([key, value], index) => {
  normalExpressionTypes[key] = index
  allNormalExpressions.push(value)
})
