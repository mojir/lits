import type { Builtin } from './interface'
import { normalExpressions } from './normalExpressions'
import type { AndNode } from './specialExpressions/and'
import { andSpecialExpression } from './specialExpressions/and'
import type { CondNode } from './specialExpressions/cond'
import { condSpecialExpression } from './specialExpressions/cond'
import type { SwitchNode } from './specialExpressions/switch'
import { switchSpecialExpression } from './specialExpressions/switch'
import type { DeclaredNode } from './specialExpressions/declared'
import { declaredSpecialExpression } from './specialExpressions/declared'
import type { DefNode } from './specialExpressions/def'
import { defSpecialExpression } from './specialExpressions/def'
import type { DoNode } from './specialExpressions/do'
import { doSpecialExpression } from './specialExpressions/do'
import type { DefnNode, FnNode, FunctionNode } from './specialExpressions/functions'
import { defnSpecialExpression, fnSpecialExpression, functionSpecialExpression } from './specialExpressions/functions'
import type { IfNode } from './specialExpressions/if'
import { ifSpecialExpression } from './specialExpressions/if'
import type { UnlessNode } from './specialExpressions/unless'
import { unlessSpecialExpression } from './specialExpressions/unless'
import type { LetNode } from './specialExpressions/let'
import { letSpecialExpression } from './specialExpressions/let'
import type { LoopNode } from './specialExpressions/loop'
import { loopSpecialExpression } from './specialExpressions/loop'
import type { DoSeqNode, ForNode } from './specialExpressions/loops'
import { doseqSpecialExpression, forSpecialExpression } from './specialExpressions/loops'
import type { OrNode } from './specialExpressions/or'
import { orSpecialExpression } from './specialExpressions/or'
import type { QqNode } from './specialExpressions/qq'
import { qqSpecialExpression } from './specialExpressions/qq'
import type { RecurNode } from './specialExpressions/recur'
import { recurSpecialExpression } from './specialExpressions/recur'
import type { ThrowNode } from './specialExpressions/throw'
import { throwSpecialExpression } from './specialExpressions/throw'
import type { TryNode } from './specialExpressions/try'
import { trySpecialExpression } from './specialExpressions/try'
import type { ArrayNode } from './specialExpressions/array'
import { arraySpecialExpression } from './specialExpressions/array'
import type { ObjectNode } from './specialExpressions/object'
import { objectSpecialExpression } from './specialExpressions/object'

const specialExpressions = {
  '&&': andSpecialExpression,
  'cond': condSpecialExpression,
  'switch': switchSpecialExpression,
  'def': defSpecialExpression,
  'defn': defnSpecialExpression,
  'function': functionSpecialExpression,
  'do': doSpecialExpression,
  'doseq': doseqSpecialExpression,
  'for': forSpecialExpression,
  'fn': fnSpecialExpression,
  'if': ifSpecialExpression,
  'unless': unlessSpecialExpression,
  'let': letSpecialExpression,
  'loop': loopSpecialExpression,
  '||': orSpecialExpression,
  'recur': recurSpecialExpression,
  'throw': throwSpecialExpression,
  'try': trySpecialExpression,
  'defined?': declaredSpecialExpression,
  '??': qqSpecialExpression,
  'array': arraySpecialExpression,
  'object': objectSpecialExpression,
} as const

export type SpecialExpressionName = keyof typeof specialExpressions
export type CommonSpecialExpressionName = keyof Pick<
  typeof specialExpressions,
  | '??'
  | '&&'
  | 'cond'
  | 'switch'
  | 'defined?'
  | 'do'
  | 'if'
  | 'unless'
  | '||'
  | 'throw'
  | 'array'
  | 'object'
>

export type BuiltinSpecialExpressions = typeof specialExpressions
export type BuiltinSpecialExpression = typeof specialExpressions[SpecialExpressionName]
export type BuiltinCommonSpecialExpression = typeof specialExpressions[CommonSpecialExpressionName]
export type SpecialExpressionNode =
  | AndNode
  | CondNode
  | SwitchNode
  | DefNode
  | DefnNode
  | FunctionNode
  | DoNode
  | DoSeqNode
  | ForNode
  | FnNode
  | IfNode
  | UnlessNode
  | LetNode
  | LoopNode
  | OrNode
  | RecurNode
  | ThrowNode
  | TryNode
  | DeclaredNode
  | QqNode
  | ArrayNode
  | ObjectNode

export const builtin: Builtin = {
  normalExpressions,
  specialExpressions,
}

export const normalExpressionKeys = Object.keys(normalExpressions)
export const specialExpressionKeys = Object.keys(specialExpressions)
