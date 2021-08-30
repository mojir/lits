import { specialExpressions } from './specialExpressions'
import { normalExpressions } from './normalExpressions'

Object.keys(specialExpressions).forEach(key => {
  if (normalExpressions[key]) {
    throw Error(`Expression ${key} is defined as both a normal expression and a special expression`)
  }
})

export const builtin = {
  normalExpressions,
  specialExpressions,
}
