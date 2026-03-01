import type { Builtin } from './interface'
import { allNormalExpressions, normalExpressions } from './normalExpressions'
import { andSpecialExpression } from './specialExpressions/and'
import { condSpecialExpression } from './specialExpressions/cond'
import { matchSpecialExpression } from './specialExpressions/match'
import { definedSpecialExpression } from './specialExpressions/defined'
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
import { effectSpecialExpression } from './specialExpressions/effect'
import { objectSpecialExpression } from './specialExpressions/object'
import { importSpecialExpression } from './specialExpressions/import'
import { parallelSpecialExpression } from './specialExpressions/parallel'
import { performSpecialExpression } from './specialExpressions/perform'
import { raceSpecialExpression } from './specialExpressions/race'
import { specialExpressionTypes } from './specialExpressionTypes'

export const specialExpressions = [
  qqSpecialExpression,
  andSpecialExpression,
  orSpecialExpression,
  arraySpecialExpression,
  condSpecialExpression,
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
  matchSpecialExpression,
  throwSpecialExpression,
  trySpecialExpression,
  unlessSpecialExpression,
  importSpecialExpression,
  effectSpecialExpression,
  performSpecialExpression,
  parallelSpecialExpression,
  raceSpecialExpression,
] as const

export type SpecialExpressions = typeof specialExpressions
export type SpecialExpression = SpecialExpressions[number]
export type SpecialExpressionName = keyof typeof specialExpressionTypes
export type CommonSpecialExpressionType = [
  | typeof specialExpressionTypes['??']
  | typeof specialExpressionTypes['&&']
  | typeof specialExpressionTypes['cond']
  | typeof specialExpressionTypes['match']
  | typeof specialExpressionTypes['defined?']
  | typeof specialExpressionTypes['block']
  | typeof specialExpressionTypes['if']
  | typeof specialExpressionTypes['unless']
  | typeof specialExpressionTypes['||']
  | typeof specialExpressionTypes['throw']
  | typeof specialExpressionTypes['array']
  | typeof specialExpressionTypes['object']
  | typeof specialExpressionTypes['effect']
  | typeof specialExpressionTypes['perform']
  | typeof specialExpressionTypes['parallel']
  | typeof specialExpressionTypes['race'],
]

export type SpecialExpressionType = typeof specialExpressionTypes[SpecialExpressionName]

export const builtin: Builtin = {
  normalExpressions,
  specialExpressions,
  allNormalExpressions,
}

export const normalExpressionKeys = Object.keys(normalExpressions)
export const specialExpressionKeys = Object.keys(specialExpressionTypes)
