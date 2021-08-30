export { specialExpressions } from './specialExpressions'
export { normalExpressions } from './normalExpressions'

import { specialExpressions } from './specialExpressions'
import { normalExpressions } from './normalExpressions'

Object.keys(specialExpressions).forEach(key => {
  if (normalExpressions[key]) {
    throw Error(`Expression ${key} is defined as both a normal expression and a special expression`)
  }
})
