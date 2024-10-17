import type { RegularExpression } from './parser/interface'
import type { LitsFunction } from '.'

export type Arr = unknown[]
export type Seq = string | Arr
export type Obj = Record<string, unknown>
export type Coll = Seq | Obj
export type Any = Coll | string | number | boolean | null | LitsFunction | RegularExpression

export type UnknownRecord = Record<string, unknown>
