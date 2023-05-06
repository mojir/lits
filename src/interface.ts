import { LitsFunction } from '.'
import { RegularExpression } from './parser/interface'

export type Arr = unknown[]
export type Seq = string | Arr
export type Obj = Record<string, unknown>
export type Coll = Seq | Obj
export type Any = Coll | string | number | boolean | null | LitsFunction | RegularExpression
