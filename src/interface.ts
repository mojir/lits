import { LispishFunction } from '.'

export type Arr = unknown[]
export type Seq = string | Arr
export type Obj = Record<string, unknown>
export type Coll = Seq | Obj
export type Any = Coll | string | number | boolean | RegExp | null | LispishFunction
