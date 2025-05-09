import type { BuiltinNormalExpression, BuiltinNormalExpressions } from '../interface'
import type { Any } from '../../interface'
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

const expressions: BuiltinNormalExpressions = {
  ...bitwiseNormalExpression,
  ...collectionNormalExpression,
  ...arrayNormalExpression,
  ...sequenceNormalExpression,
  ...mathNormalExpression,
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
