import { BuiltinNormalExpressions } from './interface'
import { list } from './list'
import { math } from './math'
import { misc } from './misc'
import { object } from './object'
import { predicates } from './predicates'
import { regexp } from './regexp'
import { string } from './string'

export const normalExpressions: BuiltinNormalExpressions = {
  ...list,
  ...math,
  ...misc,
  ...object,
  ...predicates,
  ...regexp,
  ...string,
}
