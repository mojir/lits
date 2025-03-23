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
