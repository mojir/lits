import { BuiltinNormalExpressions } from '../interface'
import { bitwiseNormalExpression } from './categories/bitwise'
import { listNormalExpression } from './categories/array'
import { mathNormalExpression } from './categories/math'
import { miscNormalExpression } from './categories/misc'
import { objectNormalExpression } from './categories/object'
import { predicatesNormalExpression } from './categories/predicates'
import { regexpNormalExpression } from './categories/regexp'
import { stringNormalExpression } from './categories/string'

export const normalExpressions: BuiltinNormalExpressions = {
  ...bitwiseNormalExpression,
  ...listNormalExpression,
  ...mathNormalExpression,
  ...miscNormalExpression,
  ...objectNormalExpression,
  ...predicatesNormalExpression,
  ...regexpNormalExpression,
  ...stringNormalExpression,
}
