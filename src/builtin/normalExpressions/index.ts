import { BuiltinNormalExpressions } from '../interface'
import { bitwiseNormalExpression } from './categories/bitwise'
import { collectionNormalExpression } from './categories/collection'
import { arrayNormalExpression } from './categories/array'
import { sequenceNormalExpression } from './categories/sequence'
import { mathNormalExpression } from './categories/math'
import { miscNormalExpression } from './categories/misc'
import { objectNormalExpression } from './categories/object'
import { predicatesNormalExpression } from './categories/predicates'
import { regexpNormalExpression } from './categories/regexp'
import { stringNormalExpression } from './categories/string'
import { functionalNormalExpression } from './categories/functional'

export const normalExpressions: BuiltinNormalExpressions = {
  ...bitwiseNormalExpression,
  ...collectionNormalExpression,
  ...arrayNormalExpression,
  ...sequenceNormalExpression,
  ...mathNormalExpression,
  ...miscNormalExpression,
  ...objectNormalExpression,
  ...predicatesNormalExpression,
  ...regexpNormalExpression,
  ...stringNormalExpression,
  ...functionalNormalExpression,
}
