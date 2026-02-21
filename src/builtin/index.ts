import type { Builtin } from './interface'
import { allNormalExpressions, normalExpressions } from './normalExpressions'
import { andSpecialExpression } from './specialExpressions/and'
import { condSpecialExpression } from './specialExpressions/cond'
import { switchSpecialExpression } from './specialExpressions/switch'
import { definedSpecialExpression } from './specialExpressions/defined'
import { defSpecialExpression } from './specialExpressions/def'
import { doSpecialExpression } from './specialExpressions/block'
import { lambdaSpecialExpression } from './specialExpressions/functions'
import { ifSpecialExpression } from './specialExpressions/if'
import { unlessSpecialExpression } from './specialExpressions/unless'
import { letSpecialExpression } from './specialExpressions/let'
import { loopSpecialExpression } from './specialExpressions/loop'
import { doseqSpecialExpression, forSpecialExpression } from './specialExpressions/loops'
import { orSpecialExpression } from './specialExpressions/or'
import { qqSpecialExpression } from './specialExpressions/qq'
import { recurSpecialExpression } from './specialExpressions/recur'
import { throwSpecialExpression } from './specialExpressions/throw'
import { trySpecialExpression } from './specialExpressions/try'
import { arraySpecialExpression } from './specialExpressions/array'
import { objectSpecialExpression } from './specialExpressions/object'
import { specialExpressionTypes } from './specialExpressionTypes'

export const specialExpressions = [
  qqSpecialExpression,
  andSpecialExpression,
  orSpecialExpression,
  arraySpecialExpression,
  condSpecialExpression,
  defSpecialExpression,
  definedSpecialExpression,
  doSpecialExpression,
  doseqSpecialExpression,
  lambdaSpecialExpression,
  forSpecialExpression,
  ifSpecialExpression,
  letSpecialExpression,
  loopSpecialExpression,
  objectSpecialExpression,
  recurSpecialExpression,
  switchSpecialExpression,
  throwSpecialExpression,
  trySpecialExpression,
  unlessSpecialExpression,
] as const

export type SpecialExpressions = typeof specialExpressions
export type SpecialExpression = SpecialExpressions[number]
export type SpecialExpressionName = keyof typeof specialExpressionTypes
export type CommonSpecialExpressionType = [
  | typeof specialExpressionTypes['??']
  | typeof specialExpressionTypes['&&']
  | typeof specialExpressionTypes['cond']
  | typeof specialExpressionTypes['switch']
  | typeof specialExpressionTypes['defined?']
  | typeof specialExpressionTypes['block']
  | typeof specialExpressionTypes['if']
  | typeof specialExpressionTypes['unless']
  | typeof specialExpressionTypes['||']
  | typeof specialExpressionTypes['throw']
  | typeof specialExpressionTypes['array']
  | typeof specialExpressionTypes['object'],
]

export type SpecialExpressionType = typeof specialExpressionTypes[SpecialExpressionName]

export const builtin: Builtin = {
  normalExpressions,
  specialExpressions,
  allNormalExpressions,
}

export const normalExpressionKeys = Object.keys(normalExpressions)
export const specialExpressionKeys = Object.keys(specialExpressionTypes)
